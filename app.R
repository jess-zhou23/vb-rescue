library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(plotly)
library(shinyWidgets)

# Robust date parser: accepts ISO (YYYY-MM-DD, YYYY/MM/DD) and US month/day/year (M/D/YYYY).
# Anything unparseable or implausible (before year 2000) becomes NA and is dropped downstream.
parse_dates <- function(x){
  x <- trimws(as.character(x))
  iso <- grepl('^[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}', x)
  out <- as.Date(rep(NA_character_, length(x)))
  out[iso] <- as.Date(gsub('/', '-', x[iso]), format='%Y-%m-%d')
  out[!iso] <- as.Date(x[!iso], format='%m/%d/%Y')
  out[!is.na(out) & out < as.Date('2000-01-01')] <- NA
  out
}

endpoint_pred <- function(train_df, new_df) {
  
  x_train <- as.numeric(
    difftime(
      train_df$date,
      train_df$date[1],
      units = "days"
    )
  ) / 30.4375
  
  y_train <- train_df$bls_emts
  
  slope <- (
    tail(y_train, 1) - head(y_train, 1)
  ) / (
    tail(x_train, 1) - head(x_train, 1)
  )
  
  if (!is.finite(slope)) {
    stop("Cannot estimate trend from training dates")
  }
  
  last_date <- tail(train_df$date, 1)
  current <- tail(y_train, 1)
  
  elapsed <- as.numeric(
    difftime(new_df$date, last_date, units = "days")
  ) / 30.4375
  
  pred <- pmax(0, current + slope * elapsed)
  
  return(pred)
}

base <- read.csv('bls_2026.csv', stringsAsFactors=FALSE)
base$date <- parse_dates(base$date)
# These are annual cross-sectional ratios, not confirmed same-cohort probabilities.
# The ref and rate is the 2025 data we got from the email. 
ref <- list(applications=1229, approved=408, graduates=161, aics=101,
            academy_sessions=6, seats_per_session=60,
            lost_approved=47, lost_students_testing=105, lost_interns=45)
rates <- c(approval=408/1229, graduation=161/408, qualification=101/161)
labels <- c(pending_academy='Pending academy enrollment', awaiting_academy='Awaiting EMT academy',
            pending_bls_intern='Pending BLS internship', emt_students='EMT academy students',
            testing='Students in testing', ops_pending='Pending operations track',
            bls_interns='BLS interns', bls_emts='Active BLS EMT AICs (proxy)',
            graduates='EMT graduates (all)', ops_completions='Operations completions (all)',
            new_bls='New BLS AICs')
ui <- fluidPage(
  tags$head(
      tags$style(HTML('
    body {
      background: #f6f8fb;
      color: #1e293b;
    }

    .well {
      background: white;
      border: 1px solid #e2e8f0;
    }

    .metric {
      background: white;
      border: 1px solid #e2e8f0;
      border-radius: 10px;
      padding: 16px;
      margin-bottom: 15px;
    }

    .note {
      color: #64748b;
      font-size: 13px;
    }

    .tab-content .plotly {
      margin-bottom: 24px;
    }
    .chart-card {
      background: white;
      margin-bottom: 24px;
      padding-bottom: 12px;
    }
    .chart-card .plotly {
      margin-bottom: 0;
    }
    .chart-card .note {
      padding: 0 16px;
      margin: 0;
    }
    
#comparison {
  width: 100%;
}

#comparison table {
  width: 100% !important;
  table-layout: fixed;
}

#comparison th,
#comparison td {
  text-align: center;
  padding: 10px;
}

#comparison th:first-child,
#comparison td:first-child {
  width: 75%;
}

#comparison th:last-child,
#comparison td:last-child {
  width: 25%;
}

    @media (min-width: 768px) {

  /* Fixed-height sidebar */
  .col-sm-4 > .well {
    height: calc(100vh - 20px);
    max-height: calc(100vh - 20px);
    display: flex;
    flex-direction: column;
    overflow: hidden;
  }

  /* Header stays visible */
  .control-header {
    flex: 0 0 auto;
    background: white;
    padding: 10px 0 15px;
    border-bottom: 1px solid #e2e8f0;
  }

  /* Independently scrollable controls */
  .control-scroll {
    flex: 1 1 auto;
    min-height: 0;
    overflow-y: auto;
    overflow-x: hidden;
    padding-right: 10px;
  }

  /* Reset button sticks after reaching the top */
  .sticky-reset {
    position: sticky;
    top: 0;
    z-index: 100;
    background: white;
    padding: 10px 0;
    border-bottom: 1px solid #e2e8f0;
  }
  
  .col-sm-4 {
  position: sticky;
  top: 10px;
  align-self: flex-start;
}

}
    
    
.detail-section {
  background: #ffffff;
  border: 1px solid #e2e8f0;
  border-radius: 10px;
  padding: 22px 26px;
  margin-bottom: 20px;
}

.detail-section h4 {
  color: #17324d;
  font-weight: 700;
  margin-top: 0;
  margin-bottom: 18px;
  padding-bottom: 10px;
  border-bottom: 1px solid #e2e8f0;
}

.detail-section strong {
  color: #243b53;
}

.detail-section p {
  line-height: 1.6;
  margin-top: 6px;
  margin-bottom: 16px;
}

.detail-section .note {
  font-size: 13px;
  color: #64748b;
}

    
  '))
    ),
  div(
    style = 'text-align: center; padding: 24px 12px 20px; margin-bottom: 12px;',
    h1('Virginia Beach Rescue Foundation | BLS Volunteer Planner',
       style = 'font-size: 30px; font-weight: 700; color: #17324d; margin: 0; line-height: 1.3;')
  ),
  sidebarLayout(sidebarPanel(width=4,
                             div(
                               class = "control-header",
                               h3("Control Panel"),
                             ),
                             div(
                               class = "control-scroll",
                             numericInput('target','BLS provider target',320,min=1),
                             sliderInput('horizon','Forecast horizon (months)',12,120,30,step=6), # start, stop, default, step
                             selectInput('baseline','Trend benchmark method',c('Linear regression'='linear','Endpoint average change'='change')),
                             tags$hr(),strong('Recruitment Funnel Scenario'),
                             p(class='note','Sliders adjust the 2025 annual ratios. 100% means no improvement from baseline.'),
                             div(
                               class = "sticky-reset",
                               actionButton(
                                 "reset_scenario",
                                 "Reset entire scenario",
                                 class = "btn-warning"
                               )
                             ),
                             fluidRow(
                               column(10, sliderInput('application_mult','Application approval/progression (% of 2025 baseline)',50,200,100,step=5,post='%')),
                               column(2, actionButton('reset_application', '↻', class = 'btn-sm'))
                             ),
                             fluidRow(
                               column(10, sliderInput('academy_mult','Academy graduation (% of 2025 baseline)',50,200,100,step=5,post='%')),
                               column(2, actionButton('reset_academy', '↻', class = 'btn-sm'))
                             ),
                             fluidRow(
                               column(10, sliderInput('intern_mult','Graduate-to-AIC yield (% of 2025 baseline)',50,200,100,step=5,post='%')),
                               column(2, actionButton('reset_intern', '↻', class = 'btn-sm'))
                             ),
                             numericInput('applications','Applications per year',1229,min=0,step=25),
                             numericInput('sessions','Academies per year',6,min=0,max=12,step=1),
                             numericInput('seats','Seats per academy',60,min=0,max=200,step=5),
                             numericInput('seat_utilization','Usable seat capacity (%)',100,min=0,max=100,step=5),
                             tags$hr(), h4('Workforce & timing'),
                             numericInput('pipeline_lag','Pipeline delay to new AICs (months)',6,min=0,max=36),
                             p(class='note',
                               'Estimated time between implementing recruitment improvements and seeing their impact on active BLS membership. During this delay, membership follows the status quo forecast.'),
                             tags$hr(),h4('Update membership data'),
                             uiOutput('csv_upload_ui'),
                             uiOutput('csv_status_ui'),
                             uiOutput('remove_csv_ui'),
                             downloadButton('template','CSV template'),
                             downloadButton('export','Export projections')
                             )
                             ),
                mainPanel(width=8,
                          fluidRow(column(4,div(class='metric',strong('Latest Active BLS Providers'),h2(textOutput('current',inline=TRUE)))),
                                   column(4,div(class='metric',strong('Status Quo Trend Target Date'),h2(textOutput('trend_eta',inline=TRUE)))),
                                   column(4,div(class='metric',strong('Funnel Scenario Target Date'),h2(textOutput('scenario_eta',inline=TRUE))))),
                          tabsetPanel(
                            tabPanel('Forecast',
                                     plotlyOutput('forecast',height='410px'),
                                     downloadButton('download_forecast','Download forecast graph'),
                                     tableOutput('comparison'),
                                     p(class='note','Funnel forecasts are scenario calculations, not statistical prediction intervals. The 2025 ratios may mix recruitment cohorts.')),
                            tabPanel('Latest trends',
                                     uiOutput('trend_range_ui'),
                                     plotlyOutput('g_goal',height='330px'),
                                     plotlyOutput('g_snapshot',height='300px'),
                                     div(class='chart-card',
                                         plotlyOutput('g_funnel',height='330px'),
                                         p(class='note','Year-to-date totals. Not the same people moving through: someone graduating now may have applied last year. Approved includes all tracks, not only BLS.')),
                                     div(class='chart-card',
                                         plotlyOutput('g_flow',height='330px'),
                                         p(class='note',
                                           strong('Joined: '), 'volunteers who newly qualified as BLS AICs that month. ',
                                           strong('Left: '), 'volunteer AICs who exited the system that month.')),
                                     plotlyOutput('g_losses',height='300px'),
                                     tags$hr(),selectInput('stage','Explore a membership or pipeline measure',choices=setNames(names(labels),unname(labels)),selected='bls_emts'),plotlyOutput('stageplot',height='290px')),
                            tabPanel('Scenario Details',
                                     
                                     h3('Understanding Your Scenario'),
                                     p('Use the control panel on the left to explore how recruitment, training capacity, and timing assumptions may affect future BLS membership. The Forecast tab displays the resulting projections.'),
                                     
                                     # SECTION 1: FORECAST SETTINGS
                                     div(class = 'detail-section',
                                         h4('1. Forecast Settings'),
                                         
                                         strong('BLS provider target'),
                                         p('Sets the desired number of active BLS providers. The target appears as a horizontal line on the forecast graph and is used to calculate the estimated target dates.'),
                                         
                                         strong('Forecast horizon'),
                                         p('Determines how many months into the future the dashboard projects membership. A longer horizon extends the forecast but does not change the underlying monthly growth assumptions.'),
                                         
                                         strong('Trend benchmark method'),
                                         p('Selects how the status quo membership forecast is calculated. This forecast serves as the starting point for evaluating recruitment improvements.'),
                                         
                                         tags$ul(
                                           tags$li(strong('Linear regression: '),
                                                   'Uses historical membership, time, EMT academy students, and pending operations candidates to estimate future membership. Future student and operations counts are held at their latest observed values.'),
                                           tags$li(strong('Endpoint average change: '),
                                                   'Calculates the average monthly membership change between the first and most recent observations, then extends that trend into the future.')
                                         )
                                     ),
                                     
                                     # SECTION 2: RECRUITMENT FUNNEL
                                     div(class = 'detail-section',
                                         h4('2. Recruitment Funnel Improvements'),
                                         
                                         p('These three sliders explore improvements at different stages of the recruitment process. Each is measured relative to the corresponding 2025 baseline rate.'),
                                         
                                         p(strong('How to interpret the sliders: '),
                                           '100% maintains the historical rate, 150% represents a 50% increase in that rate, and 50% represents a 50% decrease. Rates cannot exceed 100%.'),
                                         
                                         strong('Application approval / progression'),
                                         p('Adjusts the proportion of applicants who are approved or progress toward academy enrollment. Increasing this slider allows more applicants to enter the recruitment pipeline, subject to available academy capacity.'),
                                         
                                         strong('Academy graduation'),
                                         p('Adjusts the proportion of modeled academy entrants who successfully graduate. Increasing this slider represents improvements in student retention, training support, or graduation outcomes.'),
                                         
                                         strong('Graduate-to-AIC yield'),
                                         p('Adjusts the proportion of graduates who ultimately become qualified AICs. Increasing this slider represents improvements in the transition from graduation through the remaining qualification process.'),
                                         
                                         p(class = 'note',
                                           'These adjustments are hypothetical scenarios, not guaranteed outcomes. The historical rates are based on annual totals rather than verified individual recruitment cohorts.')
                                     ),
                                     
                                     # SECTION 3: RECRUITMENT CAPACITY
                                     div(class = 'detail-section',
                                         h4('3. Recruitment Volume & Academy Capacity'),
                                         
                                         strong('Applications per year'),
                                         p(
                                           'Sets the assumed number of applications received annually. ',
                                           strong('Additional applications may not increase AIC production if academy capacity is already full.')
                                         ),
                                         
                                         strong('Academies per year'),
                                         p('Sets the number of academy sessions offered annually. More sessions increase the potential number of recruits who can begin training.'),
                                         
                                         strong('Seats per academy'),
                                         p('Sets the maximum number of students each academy can accommodate. Together with the number of sessions, this determines total annual academy capacity.'),
                                         
                                         strong('Usable seat capacity'),
                                         p('Specifies the percentage of academy seats expected to be filled and usable. Lower utilization reduces the number of recruits who can enter training.'),
                                         
                                         p(class = 'note',
                                           'The model limits annual academy entrants to the smaller of approved applicants and usable academy capacity. Increasing capacity will only improve outcomes when sufficient applicants are available.')
                                     ),
                                     
                                     # SECTION 4: WORKFORCE & TIMING
                                     div(class = 'detail-section',
                                         h4('4. Workforce & Timing Assumptions'),
                                         
                                         strong('Pipeline delay to new AICs'),
                                         p('Represents the assumed number of months between implementing recruitment improvements and seeing their effects on active BLS membership.'),
                                         
                                         p('During this delay, the scenario follows the status quo forecast. Afterward, the difference between scenario and baseline annual AIC production is gradually incorporated into projected membership.'),
                                         
                                         strong('How the scenario forecast works'),
                                         p('The dashboard first estimates future membership using the selected benchmark method. It then calculates the change in annual AIC production under the recruitment and capacity assumptions. This difference is converted into a monthly adjustment and applied after the pipeline delay.'),
                                         
                                         p(class = 'note',
                                           'The scenario is a planning projection, not a statistical prediction interval. It assumes the selected recruitment conditions remain constant over the forecast horizon.')
                                     ),
                                     
                                     # SECTION 5: DATA & LIMITATIONS
                                     div(class = 'detail-section',
                                         h4('5. Data & Interpretation'),
                                         
                                         p('The recruitment funnel is calibrated using reported 2025 annual totals. These totals may include people from different application cohorts, and some academy and graduation counts may include non-BLS candidates.'),
                                         
                                         p('Reported pipeline losses include withdrawals before academy, during training or testing, and during internship. These are different from departures of already active BLS providers and are not separately subtracted from the membership forecast.'),
                                         
                                         p('Use the projections to compare possible strategies rather than as guaranteed future staffing levels.')
                                     )
                            ),
                            tabPanel('Source Data',
                                     
                                     h3('Historical Membership & Recruitment Data'),
                                     
                                     p('Explore the historical data used to generate membership forecasts and recruitment trends. Uploaded monthly updates are automatically reflected in this table.'),
                                     
                                     div(
                                       class = 'detail-section',
                                       DTOutput('source')
                                     )
                            )
                          )
                ))
)
server <- function(input,output,session){
  
  observeEvent(input$reset_application, {
    updateSliderInput(session, 'application_mult', value = 100)
  })
  
  observeEvent(input$reset_academy, {
    updateSliderInput(session, 'academy_mult', value = 100)
  })
  
  observeEvent(input$reset_intern, {
    updateSliderInput(session, 'intern_mult', value = 100)
  })
  
  observeEvent(input$reset_scenario, {
    updateSliderInput(session, 'application_mult', value = 100)
    updateSliderInput(session, 'academy_mult', value = 100)
    updateSliderInput(session, 'intern_mult', value = 100)
    
    updateNumericInput(session, 'applications', value = 1229)
    updateNumericInput(session, 'sessions', value = 6)
    updateNumericInput(session, 'seats', value = 60)
    updateNumericInput(session, 'seat_utilization', value = 100)
    updateNumericInput(session, 'pipeline_lag', value = 6)
  })
  
  # CSV upload: validate first, then use only accepted data.
  uploaded_data <- reactiveVal(NULL)
  upload_version <- reactiveVal(0L)
  
  output$csv_upload_ui <- renderUI({
    fileInput(
      paste0('upload_', upload_version()),
      'Upload monthly CSV',
      accept = '.csv'
    )
  })
  
  observe({
    # A new input ID is used after removal, so the old selection is cleared.
    upload <- input[[paste0('upload_', upload_version())]]
    req(upload)
    
    required_cols <- c('date', 'bls_emts', 'new_bls', 'emt_students', 'ops_pending')
    u <- tryCatch(
      read.csv(upload$datapath, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    
    if (!grepl('\\.csv$', upload$name, ignore.case = TRUE) ||
        is.null(u) || !all(required_cols %in% names(u))) {
      uploaded_data(NULL)
      showNotification(
        'Invalid CSV. Required columns: date, bls_emts, new_bls, emt_students, ops_pending.',
        type = 'error', duration = 8
      )
      return()
    }
    
    u$date <- parse_dates(u$date)
    u$bls_emts <- suppressWarnings(as.numeric(u$bls_emts))
    u$new_bls <- suppressWarnings(as.numeric(u$new_bls))
    
    valid_rows <- !is.na(u$date) &
      is.finite(u$bls_emts) & is.finite(u$new_bls) &
      u$bls_emts >= 0 & u$new_bls >= 0 &
      u$bls_emts == floor(u$bls_emts) &
      u$new_bls == floor(u$new_bls)
    
    if (nrow(u) == 0 || !all(valid_rows) || anyDuplicated(u$date) > 0) {
      uploaded_data(NULL)
      showNotification(
        paste('Invalid CSV. Dates must be valid and unique;',
              'bls_emts and new_bls must be nonnegative whole numbers.',
              'Dates: YYYY-MM-DD or MM/DD/YYYY.'),
        type = 'error', duration = 8
      )
      return()
    }
    
    # Other fields in the baseline dataset are optional numeric counts.
    optional_cols <- setdiff(names(base), required_cols)
    for (col in intersect(optional_cols, names(u))) {
      raw <- u[[col]]
      converted <- suppressWarnings(as.numeric(raw))
      invalid <- !is.na(raw) & trimws(as.character(raw)) != '' &
        !is.finite(converted)
      if (any(invalid)) {
        uploaded_data(NULL)
        showNotification(
          paste('Invalid CSV. Optional column', col, 'must contain numeric values.'),
          type = 'error', duration = 8
        )
        return()
      }
      u[[col]] <- converted
    }
    
    uploaded_data(u)
    showNotification('Upload complete', type = 'message', duration = 4)
  })
  
  output$csv_status_ui <- renderUI({
    if (is.null(uploaded_data())) {
      p(class = 'note', 'Using original baseline dataset')
    } else {
      tags$p(
        style = 'color: #198754; font-weight: bold;',
        icon('check-circle'), ' Upload complete'
      )
    }
  })
  
  output$remove_csv_ui <- renderUI({
    req(uploaded_data())
    actionButton('remove_csv', 'Remove uploaded CSV',
                 icon = icon('trash'), class = 'btn-warning')
  })
  
  observeEvent(input$remove_csv, {
    uploaded_data(NULL)
    upload_version(upload_version() + 1L)
    showNotification('Uploaded CSV removed. Baseline restored.',
                     type = 'message', duration = 4)
  })
  
  data <- reactive({
    d <- base
    u <- uploaded_data()
    if (!is.null(u)) {
      for (col in setdiff(names(d), names(u))) {
        u[[col]] <- NA_real_
      }
      u <- u[, names(d), drop = FALSE]
      d <- bind_rows(d[!d$date %in% u$date, ], u)
    }
    d |> arrange(date)
  })
  
  model <- reactive({

    # Historical data
    z <- c("bls_emts", "date", "emt_students", "ops_pending","new_bls")
    d <- data() |>
      select(all_of(z)) |>
      filter(!is.na(bls_emts), !is.na(date), !is.na(emt_students), !is.na(ops_pending)) |>
      arrange(date)
    
    validate(need(nrow(d) >= 2,"At least two membership observations required"))
    
    # Time index
    origin_date <- d$date[1]
    d$x <- as.numeric(difftime(d$date, origin_date, units = "days"))/30.4375
    y <- d$bls_emts
    
    # Last observed values
    last <- tail(d$date, 1)
    current <- tail(y, 1)
    
    # Future dates
    H <- input$horizon
    h <- seq_len(H)
    
    future <- last %m+% months(h)
    
    # Future time index
    future_x <- as.numeric(difftime(future, origin_date, units = "days")) / 30.4375
    
    # Future predictors
    # Assume latest observed values remain unchanged
    future_df <- data.frame(
      x = future_x,
      emt_students = rep(tail(d$emt_students, 1), H),
      ops_pending = rep(tail(d$ops_pending, 1), H)
    )
    
    # 1. MLR -----
    mlr_mod <- lm(bls_emts ~ x + emt_students + ops_pending,data = d)
    
    mlr_pred <- predict(mlr_mod,newdata = future_df)
    mlr_pred <- pmax(0, mlr_pred)
    
    slope_pred <- predict(mlr_mod, d)
    n <- length(slope_pred)
    
    mlr_slope <- (slope_pred[n]-slope_pred[1])/n
    
    # 2. Endpoint model -------------------------------
    
    endpoint_slope <- (tail(d$bls_emts, 1) - head(d$bls_emts, 1)) / (tail(d$x, 1) - head(d$x, 1))
    ep_pred <- endpoint_pred(train_df = d,new_df = data.frame(date = future))
    
    # Select model ------------------------------------
    
    trend <- switch(
      input$baseline,
      linear = mlr_pred,
      change = ep_pred
    )
    
    trend_slope <- switch(
      input$baseline,
      linear = as.numeric(mlr_slope),
      change = as.numeric(endpoint_slope)
    )
    
    # Recruitment funnel --------------------------
    
    p1 <- min(1, rates["approval"]*input$application_mult / 100)
    p2 <- min(1, rates["graduation"]*input$academy_mult / 100)
    p3 <- min(1, rates["qualification"]*input$intern_mult / 100)
    
    annual_approved <- input$applications * p1
    annual_entrants <- min(annual_approved, input$sessions*input$seats*input$seat_utilization / 100)
    annual_graduates <- annual_entrants * p2
    annual_aics <- annual_graduates * p3
    
    # Baseline funnel: use the same capacity constraint
    # and conversion-rate calculations as the scenario
    
    base_approved <- ref$applications * rates["approval"]
    
    base_entrants <- min(
      base_approved,
      ref$academy_sessions * ref$seats_per_session
    )
    
    base_graduates <- base_entrants * rates["graduation"]
    
    base_aics <- base_graduates * rates["qualification"]
    
    # Funnel scenario
    annual_delta <- annual_aics - base_aics
    
    # Scenario forecast
    lag <- input$pipeline_lag
    
    months_active <- pmax(0, h - lag)
    
    scenario <- pmax(
      0,
      trend + annual_delta / 12 * months_active
    )
    
    # Scenario slope after the pipeline delay
    scenario_slope <- trend_slope + annual_delta / 12
    
    validate(
      need(
        length(trend) == length(future),
        paste(
          "Trend forecast returned",
          length(trend),
          "predictions, but",
          length(future),
          "future dates were requested."
        )
      )
    )
    
    
    # Forecast output
    f <- data.frame(
      date = future,
      status_quo = trend,
      scenario = scenario
    )
    
    list(
      f = f,
      
      # Forecast slopes
      slope = trend_slope,
      scenario_slope = scenario_slope,
      
      current = current,
      
      # Recruitment funnel
      annual_approved = annual_approved,
      annual_entrants = annual_entrants,
      annual_graduates = annual_graduates,
      annual_aics = annual_aics,
      
      # Baseline comparison
      base_aics = base_aics,
      annual_delta = annual_delta,
      
      rates = c(p1, p2, p3),
      lag = lag
    )

  })
  eta<-function(v,dates,target){j<-which(v>=target);if(length(j))format(dates[j[1]],'%b %Y') else 'Not within horizon'}
  output$current<-renderText(round(model()$current))
  output$trend_eta<-renderText({m<-model();if(m$current>=input$target)'Already reached' else eta(m$f$status_quo,m$f$date,input$target)})
  output$scenario_eta<-renderText({m<-model();if(m$current>=input$target)'Already reached' else eta(m$f$scenario,m$f$date,input$target)})
  # Turns a ggplot into an interactive chart: bold title, and the number shows when you hover
  hover <- function(p, title) {
    ggplotly(p + theme_minimal(base_size = 13), tooltip = 'text') %>%
      layout(title = list(text = paste0('<b>', title, '</b>'), x = 0.01, font = list(size = 20)),
             margin = list(t = 60),
             legend = list(orientation = 'h', x = 0.35, y = -0.12)) %>%
      config(
        displayModeBar = TRUE,
        displaylogo = FALSE,
        modeBarButtons = list(
          list("zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d", "toImage")
        ),
        toImageButtonOptions = list(
          format = "png",
          filename = "VB_Rescue_Graph",
          width = 1200,
          height = 700,
          scale = 2
        )
      )
  }
  output$forecast<-renderPlotly({
    m<-model();d<-data();f<-m$f

    d$tip <- paste0(format(d$date, '%b %Y'), ': ', round(d$bls_emts))
    f$tip_sq <- paste0(format(f$date, '%b %Y'), ': ', round(f$status_quo), ' (Status quo)')
    f$tip_sc <- paste0(format(f$date, '%b %Y'), ': ', round(f$scenario), ' (Scenario)')
    
    p <- ggplot() +
      geom_line(data=d, aes(date, bls_emts, color='Observed BLS', text=tip, group=1), linewidth=0.9) +
      geom_point(data=d, aes(date, bls_emts, color='Observed BLS', text=tip, group=1), size=2) +
      geom_line(data=f, aes(date, status_quo, color='Status quo trend', text=tip_sq, group=1), linetype=2, linewidth=0.7) +
      geom_line(data=f, aes(date, scenario, color='Funnel scenario', text=tip_sc, group=1), linewidth=0.9) +
      geom_hline(yintercept=input$target, linetype='dotdash', color='firebrick', linewidth = 1) +
      annotate('text', x = min(d$date, na.rm=TRUE), y = input$target + 2, label = paste('Goal:', input$target), colour = 'firebrick', hjust = 0) +
      scale_color_manual(values=c('Observed BLS'='#176b79', 'Status quo trend'='#64748b', 'Funnel scenario'='#d8892c')) +
      labs(x=NULL, y='Active BLS AICs', color=NULL) +
      theme_minimal(base_size=13) +
      theme(legend.position='bottom')
    
    hover(p, 'Forecast vs. Scenario')
  })

  output$download_forecast <- downloadHandler(
    filename = function() {
      'BLS_forecast_graph.png'
    },
    content = function(file) {
      m <- model()
      d <- data()
      f <- m$f
      
      p <- ggplot() +
        geom_line(data=d, aes(date, bls_emts, color='Observed BLS'), linewidth=1.2) +
        geom_point(data=d, aes(date, bls_emts, color='Observed BLS'), size=2) +
        geom_line(data=f, aes(date, status_quo, color='Status quo trend'), linetype=2, linewidth=1) +
        geom_line(data=f, aes(date, scenario, color='Funnel scenario'), linewidth=1.2) +
        geom_hline(yintercept=input$target,
                   linetype='dashed', color='firebrick', linewidth=1) +
        annotate('text', x = min(d$date, na.rm=TRUE) + 30, y = input$target + 5, label = paste('Goal:', input$target), colour = 'firebrick', hjust = 0) +
        scale_color_manual(values=c(
          'Observed BLS'='#176b79',
          'Status quo trend'='#64748b',
          'Funnel scenario'='#d8892c'
        )) +
        labs(x=NULL, y='Active BLS AICs (proxy)', color=NULL) +
        theme_minimal(base_size=13) +
        theme(legend.position='bottom')
    }
  )
  output$comparison <- renderTable({
    m <- model()
    
    data.frame(
      Measure = c(
        'Status quo predicted BLS providers/month',
        'Scenario predicted BLS providers/month after pipeline delay',
        'Historical 2025 AIC baseline (reported)',
        'Scenario new AICs/year',
        'Predicted Additional AICs from 2025'
      ),
      Value = round(c(
        m$slope,
        m$scenario_slope,
        ref$aics,
        m$annual_aics,
        (m$annual_aics-ref$aics) # delta
      ), 1)
    )
  }, striped = TRUE, width = '100%', align = 'c')

  output$funnel_table<-renderTable({m<-model();data.frame(
    Stage=c('Applications','Approved / progressed','Academy entrants (capacity capped)','Academy graduates','New AICs'),
    `2025 annual count`=c(1229,408,NA,161,101),
    `Scenario annual count`=round(c(input$applications,m$annual_approved,m$annual_entrants,m$annual_graduates,m$annual_aics),1),check.names=FALSE)},striped=TRUE)
  output$math_summary <- renderUI({
    m <- model()
    
    selected_model <- if (input$baseline == "linear") {
      "Multiple Linear Regression"
    } else {
      "Endpoint Average Change"
    }
    
    div(
      class = 'scenario-details',
      
      # 1. Forecast benchmark
      h4('1. Selected forecasting model'),
      
      p(
        strong('Benchmark method: '),
        selected_model
      ),
      
      p(
        strong('Status quo predicted membership change: '),
        round(m$slope, 2), ' BLS providers per month.'
      ),
      
      p(
        strong('Scenario predicted membership change: '),
        round(m$scenario_slope, 2), ' BLS providers per month after the pipeline delay.'
      ),
      
      p(
        class = 'note',
        'The status quo slope is determined by the selected forecasting model. ',
        'The scenario slope additionally accounts for the change in annual AIC production.'
      ),
      
      # 2. Historical conversion rates
      h4('2. Historical conversion rates (from 2025 baseline)'),
      
      p(
        strong('Approval: '), round(100 * rates['approval'], 1), '%',
        ' | ',
        strong('Graduation: '), round(100 * rates['graduation'], 1), '%',
        ' | ',
        strong('AIC qualification: '), round(100 * rates['qualification'], 1), '%'
      ),
      
      # 3. Scenario assumptions
      h4('3. Scenario assumptions'),
      
      p(
        strong('Adjusted rates: '),
        paste(round(100 * m$rates, 1), collapse = '%, '), '%'
      ),
      
      p(
        strong('Annual academy capacity: '),
        input$sessions, ' academies × ', input$seats,
        ' seats × ', input$seat_utilization, '% utilization = ',
        input$sessions * input$seats * input$seat_utilization / 100,
        ' usable seats.'
      ),
      
      # 4. Annual recruitment projections
      h4('4. Annual recruitment projections'),
      
      p(
        strong('Projected new AICs: '),
        round(m$annual_aics, 1), ' per year.'
      ),
      
      p(
        strong('Modeled 2025 baseline: '),
        round(m$base_aics, 1), ' AICs per year.'
      ),
      
      p(
        strong('Change from the modeled baseline: '),
        round(m$annual_delta, 1), ' AICs per year.'
      ),
      
      # 5. Membership forecast
      h4('5. Membership forecast'),
      
      p(
        'Projected membership follows the selected status quo trend, ',
        'with the annual recruitment adjustment beginning after the ',
        m$lag, '-month pipeline delay.'
      ),
      
      p(
        strong('Inferred active departures: '),
        round(m$inferred_exits, 1), ' per year.'
      ),
      
      p(
        strong('User-assumed departures: '),
        round(m$exits, 1), ' per year.'
      ),
      
      p(
        class = 'note',
        'Departure estimates are based on historical membership growth and ',
        'new BLS AIC counts, not the selected forecasting model. They are ',
        'shown for context only and do not alter the calibrated forecast. ',
        'Annual funnel rates are not cohort-verified, and graduation/AIC ',
        'populations may not be strictly BLS-only.'
      )
    )
  })
  outputOptions(
    output,
    "math_summary",
    suspendWhenHidden = FALSE
  )
  
  
  # ---- Latest trends tab: these use data(), so they update when new months are uploaded ----
  # Time range picker for the Latest trends tab. Defaults to the latest year in the data.
  output$trend_range_ui <- renderUI({
    dts <- data()$date
    m <- seq(floor_date(min(dts, na.rm = TRUE), 'month'), floor_date(max(dts, na.rm = TRUE), 'month'), by = 'month')
    lab <- format(m, '%b %Y')
    shinyWidgets::sliderTextInput('trend_range', 'Time range', choices = lab,
                                  selected = c(lab[which(year(m) == year(max(m)))[1]], lab[length(lab)]),
                                  grid = TRUE, force_edges = TRUE, width = '100%')
  })
  trend_data <- reactive({
    req(input$trend_range)
    r <- as.Date(paste('01', input$trend_range), format = '%d %b %Y')
    d <- data() %>% filter(floor_date(date, 'month') >= r[1], floor_date(date, 'month') <= r[2])
    validate(need(nrow(d) > 0, 'No data in the selected time range'))
    d
  })
  
  months_axis <- scale_x_date(date_breaks = '1 month', date_labels = '%b %y')
  
  
  # 1. EMTs vs goal
  output$g_goal <- renderPlotly({
    d <- trend_data() %>% filter(!is.na(bls_emts))
    d$tip <- paste0(format(d$date, '%B'), ': ', d$bls_emts, ' EMTs')
    p <- ggplot(d, aes(date, bls_emts, text = tip, group = 1)) +
      geom_hline(yintercept = input$target, colour = 'firebrick', linetype = 'dashed') +
      annotate('text', x = min(d$date) + 0.15 * diff(range(d$date)), y = input$target + 2, label = paste('Goal:', input$target),
               colour = 'firebrick', hjust = 0) +
      geom_line(color = '#176b79') +
      geom_point(size = 2, color = '#176b79') +
      geom_text(aes(y = bls_emts + 3.5, label = bls_emts), size = 2.5) +
      months_axis +
      expand_limits(y = c(min(d$bls_emts) - 5, input$target + 5)) +
      labs(x = NULL, y = 'EMT AICs')
    hover(p, paste('EMTs vs goal of', input$target))
  })
  
  # 2. Where everyone is right now (latest month)
  output$g_snapshot <- renderPlotly({
    r <- trend_data() %>% filter(!is.na(bls_emts)) %>% filter(date == max(date))
    s <- data.frame(stage = c('In academy or testing', 'BLS interns', 'Released EMTs'),
                    value = c(r$emt_students + r$testing, r$bls_interns, r$bls_emts))
    s$stage <- factor(s$stage, levels = s$stage)
    s$tip <- paste0(s$stage, ': ', s$value, ' people')
    p <- ggplot(s, aes(stage, value, text = tip)) +
      geom_col(fill = '#176b79', width = 0.6) +
      geom_text(aes(y = value + max(value, na.rm = TRUE) * 0.05, label = value), size = 3.5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = NULL, y = 'People')
    hover(p, paste('Where members are in', format(r$date, '%B %Y')))
  })
  
  # 3. Pipeline (year to date)
  output$g_funnel <- renderPlotly({
    d <- trend_data()
    funnel <- data.frame(stage = c('Applied', 'Approved', 'Graduated', 'New BLS AIC'),
                         total = c(sum(d$applications_all, na.rm = TRUE), sum(d$approved_all, na.rm = TRUE),
                                   sum(d$graduates, na.rm = TRUE), sum(d$new_bls, na.rm = TRUE)))
    funnel$stage <- factor(funnel$stage, levels = funnel$stage)
    funnel$pct <- round(100 * funnel$total / dplyr::lag(funnel$total))
    funnel$lab <- ifelse(is.na(funnel$pct), funnel$total, paste0(funnel$total, '\n(', funnel$pct, '% of previous)'))
    funnel$tip <- ifelse(is.na(funnel$pct), paste0(funnel$stage, ': ', funnel$total),
                         paste0(funnel$stage, ': ', funnel$total, ' (', funnel$pct, '% of previous step)'))
    p <- ggplot(funnel, aes(stage, total, text = tip)) +
      geom_col(fill = '#176b79', width = 0.7) +
      geom_text(aes(y = total + max(total) * 0.11, label = lab), size = 3.5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = NULL, y = 'People')
    hover(p, paste0('Pipeline, ', format(min(d$date), '%b %Y'), ' to ', format(max(d$date), '%b %Y')))
  })
  
  # 4. Joined vs left each month
  output$g_flow <- renderPlotly({
    d <- trend_data()
    flow <- bind_rows(data.frame(date = d$date, Type = 'Joined', Value = d$new_bls),
                      data.frame(date = d$date, Type = 'Left', Value = -d$exits_all_aic)) %>%
      filter(!is.na(Value))
    flow$tip <- paste0(format(flow$date, '%B'), ': ', abs(flow$Value), ' ', tolower(flow$Type))
    p <- ggplot(flow, aes(date, Value, fill = Type, text = tip)) +
      geom_col(width = 20) +
      geom_text(aes(y = Value + ifelse(Value > 0, 1.2, -1.2), label = abs(Value)), size = 3.5) +
      geom_hline(yintercept = 0) +
      scale_fill_manual(values = c(Joined = '#176b79', Left = 'firebrick')) +
      scale_y_continuous(labels = abs, expand = expansion(mult = 0.1)) +
      months_axis +
      labs(x = NULL, y = 'People', fill = NULL)
    hover(p, 'Joined vs left')
  })
  
  # 5. Where we lose people (year to date)
  output$g_losses <- renderPlotly({
    d <- trend_data()
    losses <- data.frame(stage = c('Before academy', 'In academy or testing', 'During internship', 'After release'),
                         total = c(sum(d$withdraw_approved, na.rm = TRUE), sum(d$withdraw_students, na.rm = TRUE),
                                   sum(d$intern_exits_all, na.rm = TRUE), sum(d$exits_all_aic, na.rm = TRUE)))
    losses$stage <- factor(losses$stage, levels = losses$stage)
    losses$tip <- paste0(losses$stage, ': ', losses$total, ' people lost')
    p <- ggplot(losses, aes(stage, total, text = tip)) +
      geom_col(fill = 'firebrick', width = 0.6) +
      geom_text(aes(y = total + max(total) * 0.05, label = total), size = 3.5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = NULL, y = 'People lost')
    hover(p, paste0('Where we lose people, ', format(min(d$date), '%b %Y'), ' to ', format(max(d$date), '%b %Y')))
  })
  
  # 6. Explore a membership or pipeline measure
  output$stageplot <- renderPlotly({
    req(input$stage)
    validate(need(input$stage %in% names(labels), 'Select a valid series'))
    d <- trend_data()
    d$tip <- paste0(format(d$date, '%B'), ': ', d[[input$stage]])
    p <- ggplot(d, aes(date, .data[[input$stage]], text = tip, group = 1)) +
      geom_line(color = '#176b79') +
      geom_point(color = '#176b79', size = 2) +
      months_axis +
      labs(x = NULL, y = 'Reported count')
    hover(p, unname(labels[[input$stage]]))
  })
  
  
  output$source<-renderDT(datatable(data(),options=list(scrollX=TRUE,pageLength=10)))
  output$template<-downloadHandler(filename=function()'bls_monthly_update_template.csv',content=function(file){d<-data();d$date<-format(d$date,'%Y-%m-%d');write.csv(d[,setdiff(names(base),'month')],file,row.names=FALSE,na='')})
  output$export <- downloadHandler(
    filename = function() 'bls_funnel_forecast.csv',
    content = function(file) {
      projections <- model()$f
      projections$status_quo <- pmax(0, round(projections$status_quo))
      projections$scenario <- pmax(0, round(projections$scenario))
      write.csv(projections, file, row.names = FALSE)
    }
  )
}
shinyApp(ui,server)