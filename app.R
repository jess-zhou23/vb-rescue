library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(plotly)

base <- read.csv('bls_2026.csv', stringsAsFactors=FALSE)
base$date <- as.Date(base$date)
# These are annual cross-sectional ratios, not confirmed same-cohort probabilities.
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
  tags$head(tags$style(HTML('body{background:#f6f8fb;color:#1e293b}.well{background:white;border:1px solid #e2e8f0}.metric{background:white;border:1px solid #e2e8f0;border-radius:10px;padding:16px;margin-bottom:15px}.metric h2{margin:5px 0;color:#176b79}.note{color:#64748b;font-size:13px}.tab-content{padding-top:18px}'))),
  titlePanel('VB Rescue | BLS workforce planning'),
  p('2026 BLS membership snapshots + 2025 recruitment funnel assumptions | Target: 320'),
  sidebarLayout(sidebarPanel(width=3,
                             numericInput('target','BLS provider target',320,min=1),
                             sliderInput('horizon','Forecast horizon (months)',12,120,60,step=12),
                             selectInput('baseline','Trend benchmark method',c('Linear regression'='linear','Endpoint average change'='change')),
                             tags$hr(),h4('Recruitment funnel scenario'),
                             p(class='note','Sliders adjust the 2025 annual ratios. 100% means no improvement from baseline.'),
                             sliderInput('application_mult','Application approval/progression (% of baseline)',50,200,100,step=5,post='%'),
                             sliderInput('academy_mult','Academy graduation (% of baseline)',50,200,100,step=5,post='%'),
                             sliderInput('intern_mult','Graduate-to-AIC yield (% of baseline)',50,200,100,step=5,post='%'),
                             numericInput('applications','Applications per year',1229,min=0,step=25),
                             numericInput('sessions','Academies per year',6,min=0,max=12,step=1),
                             numericInput('seats','Seats per academy',60,min=0,max=200,step=5),
                             numericInput('seat_utilization','Usable seat capacity (%)',100,min=0,max=100,step=5),
                             tags$hr(),h4('Workforce & timing'),
                             numericInput('annual_exits','Assumed active BLS departures per year',NA,min=0,step=1),
                             p(class='note','Leave blank to infer departures from 2026 new-BLS counts minus observed membership growth. This is not a measured attrition rate.'),
                             numericInput('pipeline_lag','Pipeline delay to new AICs (months)',6,min=0,max=36),
                             tags$hr(),h4('Update membership data'),
                             fileInput('upload','Upload monthly CSV',accept='.csv'),
                             downloadButton('template','CSV template'),downloadButton('export','Export projections')),
                mainPanel(width=9,
                          fluidRow(column(4,div(class='metric',strong('Latest active BLS proxy'),h2(textOutput('current',inline=TRUE)))),
                                   column(4,div(class='metric',strong('2026 trend target date'),h2(textOutput('trend_eta',inline=TRUE)))),
                                   column(4,div(class='metric',strong('Funnel scenario target date'),h2(textOutput('scenario_eta',inline=TRUE))))),
                          tabsetPanel(
                            tabPanel('Forecast',plotOutput('forecast',height='410px'),tableOutput('comparison'),p(class='note','Funnel forecasts are scenario calculations, not statistical prediction intervals. The 2025 ratios may mix recruitment cohorts.')),
                            tabPanel('2026 trends',
                                     plotlyOutput('g_goal',height='330px'),
                                     plotlyOutput('g_snapshot',height='300px'),
                                     plotlyOutput('g_funnel',height='330px'),
                                     p(class='note','Year-to-date totals. Not the same people moving through: someone graduating now may have applied last year. Approved includes all tracks, not only BLS.'),
                                     plotlyOutput('g_flow',height='330px'),
                                     p(class='note','Exits include a few ALS volunteers, so the BLS-only number is slightly lower.'),
                                     plotlyOutput('g_losses',height='300px'),
                                     tags$hr(),selectInput('stage','Explore a membership or pipeline measure',choices=setNames(names(labels),unname(labels)),selected='bls_emts'),plotlyOutput('stageplot',height='290px')),
                            tabPanel('2025 funnel & assumptions',h4('Historical 2025 funnel'),tableOutput('funnel_table'),h4('2025 reported pipeline losses'),p('47 approved applicants, 105 academy students or testing candidates, and 45 interns were lost from the recruitment pipeline. These are NOT active BLS workforce exits, and are not subtracted from membership. They may overlap with the funnel ratios.'),h4('Scenario calculation'),verbatimTextOutput('math_summary'),p(class='note','Annual counts are ratios of annual totals, not verified same-cohort conversion probabilities. Academy seat capacity is an upper bound, not a target. Application and graduation totals may contain non-BLS candidates; verify with the client before interpreting this as a BLS-only pipeline.'),h4('2026 source data'),DTOutput('source'))
                          )
                ))
)
server <- function(input,output,session){
  # ---- Upload: read the CSV, tell the user what happened, then merge into data() ----
  uploaded <- reactiveVal(NULL)
  observeEvent(input$upload,{
    # UTF-8-BOM handles CSVs saved from Excel; names are cleaned so 'Date ' still matches 'date'
    u<-tryCatch(read.csv(input$upload$datapath,stringsAsFactors=FALSE,check.names=FALSE,fileEncoding='UTF-8-BOM'),
                error=function(e)NULL)
    if(is.null(u)){showNotification('Could not read that file. Make sure it is saved as a .csv.',type='error',duration=NULL);return()}
    names(u)<-tolower(trimws(names(u)))
    need_cols<-c('date','bls_emts','new_bls')
    if(!all(need_cols%in%names(u))){
      showNotification(paste('Upload ignored: the file is missing',paste(setdiff(need_cols,names(u)),collapse=', '),
                             '. Start from the CSV template button.'),type='error',duration=NULL);return()}
    # Accepts 2026-09-01 as well as Excel-style 9/1/2026 or 9/1/26
    u$date<-as.Date(parse_date_time(as.character(u$date),orders=c('Ymd','mdY','mdy'),quiet=TRUE))
    for(k in intersect(names(u),setdiff(names(base),'date')))u[[k]]<-suppressWarnings(as.numeric(u[[k]]))
    u<-u[!is.na(u$date)&!is.na(u$bls_emts)&!is.na(u$new_bls),,drop=FALSE]
    if(!nrow(u)){showNotification('Upload ignored: no rows had a readable date plus bls_emts and new_bls filled in.',type='error',duration=NULL);return()}
    uploaded(u)
    replaced<-sum(u$date%in%base$date)
    msg<-paste0('Loaded ',nrow(u),' month(s): ',paste(format(sort(u$date),'%b %Y'),collapse=', '),'.')
    if(replaced)msg<-paste0(msg,' ',replaced,' of them were already in the app and were replaced with the uploaded numbers.')
    blank<-setdiff(setdiff(names(base),c('month','date')),names(u))
    if(length(blank))msg<-paste0(msg,' These columns were not in the file, so months already in the app keep their old numbers and new months are left blank: ',paste(blank,collapse=', '),'. Use the CSV template button to get every column.')
    showNotification(msg,type=if(length(blank))'warning' else 'message',duration=NULL)
  })
  data <- reactive({
    d<-base
    u<-uploaded()
    if(!is.null(u)){
      for(k in setdiff(names(d),names(u)))u[[k]]<-NA_real_
      u<-u[,names(d),drop=FALSE]
      # For months already in the app, keep the existing number wherever the upload leaves a column out or blank
      old<-d[match(u$date,d$date),,drop=FALSE]
      for(k in setdiff(names(d),'date'))u[[k]]<-ifelse(is.na(u[[k]]),old[[k]],u[[k]])
      d<-bind_rows(d[!d$date%in%u$date,],u)
    }
    d|>arrange(date)
  })
  model <- reactive({
    d<-data()|>filter(!is.na(bls_emts))|>arrange(date)
    validate(need(nrow(d)>=2,'At least two membership observations required'))
    x<-as.numeric(difftime(d$date,d$date[1],units='days'))/30.4375
    y<-d$bls_emts
    slope<-if(input$baseline=='linear') unname(coef(lm(y~x))[2]) else (tail(y,1)-head(y,1))/(tail(x,1)-head(x,1))
    validate(need(is.finite(slope),'Cannot estimate trend from these dates'))
    last<-tail(d$date,1); current<-tail(y,1); H<-input$horizon; h<-seq_len(H)
    future<-last %m+% months(h)
    elapsed<-as.numeric(difftime(future,last,units='days'))/30.4375
    trend<-pmax(0,current+slope*elapsed)
    # Funnel: annual observed ratios; caps are applied to entering academy, not graduates.
    p1<-min(1,rates['approval']*input$application_mult/100)
    p2<-min(1,rates['graduation']*input$academy_mult/100)
    p3<-min(1,rates['qualification']*input$intern_mult/100)
    annual_approved<-input$applications*p1
    annual_entrants<-min(annual_approved,input$sessions*input$seats*input$seat_utilization/100)
    annual_graduates<-annual_entrants*p2
    annual_aics<-annual_graduates*p3
    # A capacity-constrained baseline at unchanged 2025 ratios and the SAME operational capacity.
    base_entrants<-min(ref$applications*rates['approval'],input$sessions*input$seats*input$seat_utilization/100)
    base_aics<-base_entrants*rates['graduation']*rates['qualification']
    # When no BLS departure count is supplied, infer from observed 2026 new BLS inflow and net growth.
    observed_new<-mean(d$new_bls,na.rm=TRUE)
    inferred_exits<-max(0,12*(observed_new-slope))
    exits<-if(is.na(input$annual_exits)) inferred_exits else input$annual_exits
    # Calibrate the no-change funnel to observed 2026 net growth, preserving the 2026 status quo.
    # Scenario changes are the DIFFERENCE between new and baseline annual AIC throughput.
    # This avoids an unsupported claim that 2025 gross flows reproduce 2026 net growth.
    annual_delta<-annual_aics-base_aics
    lag<-input$pipeline_lag
    months_active<-pmax(0,h-lag)
    scenario<-pmax(0,trend+annual_delta/12*months_active)
    f<-data.frame(date=future,status_quo=trend,scenario=scenario)
    list(f=f,slope=slope,current=current,annual_approved=annual_approved,annual_entrants=annual_entrants,
         annual_graduates=annual_graduates,annual_aics=annual_aics,base_aics=base_aics,
         annual_delta=annual_delta,exits=exits,inferred_exits=inferred_exits,
         rates=c(p1,p2,p3),lag=lag)
  })
  eta<-function(v,dates,target){j<-which(v>=target);if(length(j))format(dates[j[1]],'%b %Y') else 'Not within horizon'}
  output$current<-renderText(round(model()$current))
  output$trend_eta<-renderText({m<-model();if(m$current>=input$target)'Already reached' else eta(m$f$status_quo,m$f$date,input$target)})
  output$scenario_eta<-renderText({m<-model();if(m$current>=input$target)'Already reached' else eta(m$f$scenario,m$f$date,input$target)})
  output$forecast<-renderPlot({
    m<-model();d<-data();f<-m$f
    ggplot()+geom_line(data=d,aes(date,bls_emts,color='Observed BLS'),linewidth=1.2)+
      geom_point(data=d,aes(date,bls_emts,color='Observed BLS'),size=2)+
      geom_line(data=f,aes(date,status_quo,color='2026 trend'),linetype=2,linewidth=1)+
      geom_line(data=f,aes(date,scenario,color='Funnel scenario'),linewidth=1.2)+
      geom_hline(yintercept=input$target,linetype='dotdash',color='#6b7280')+
      scale_color_manual(values=c('Observed BLS'='#176b79','2026 trend'='#64748b','Funnel scenario'='#d8892c'))+
      labs(x=NULL,y='Active BLS AICs (proxy)',color=NULL)+theme_minimal(base_size=13)+theme(legend.position='bottom')
  })
  output$comparison<-renderTable({m<-model();data.frame(
    Measure=c('2026 observed trend (AICs/month)','2025 baseline new AICs/year (capacity constrained)',
              'Scenario new AICs/year','Additional AICs/year vs 2025 baseline',
              'Inferred active BLS departures/year (context only)'),
    Value=round(c(m$slope,m$base_aics,m$annual_aics,m$annual_delta,m$exits),1))},striped=TRUE)
  output$funnel_table<-renderTable({m<-model();data.frame(
    Stage=c('Applications','Approved / progressed','Academy entrants (capacity capped)','Academy graduates','New AICs'),
    `2025 annual count`=c(1229,408,NA,161,101),
    `Scenario annual count`=round(c(input$applications,m$annual_approved,m$annual_entrants,m$annual_graduates,m$annual_aics),1),check.names=FALSE)},striped=TRUE)
  output$math_summary<-renderText({m<-model();paste0(
    'Historical annual ratios: 408/1229 = ',round(100*rates['approval'],1),'% approved; 161/408 = ',round(100*rates['graduation'],1),'% graduated; 101/161 = ',round(100*rates['qualification'],1),'% new AICs.\n',
    'Scenario rates (capped at 100%): ',paste(round(100*m$rates,1),collapse='%, '),'%.\n',
    'Annual academy capacity = ',input$sessions,' x ',input$seats,' x ',input$seat_utilization/100,' = ',input$sessions*input$seats*input$seat_utilization/100,' usable seats.\n',
    'Scenario new AICs = min(applications x approval rate, usable seats) x graduation rate x AIC yield = ',round(m$annual_aics,1),'/year.\n',
    'Increment vs unchanged 2025 funnel at same capacity = ',round(m$annual_delta,1),'/year.\n',
    'Projected membership at month h = 2026 trend forecast + (annual increment / 12) x max(0, h - ',m$lag,').\n',
    '2026 implied active departures = ',round(m$inferred_exits,1),'/year; user assumption = ',round(m$exits,1),'/year. These are shown for context only and do NOT alter the calibrated forecast.\n',
    'Important: the annual funnel rates are not cohort-verified, and graduation/AIC populations may not be strictly BLS-only.')})
  # ---- 2026 trends tab: these all use data(), so they update when new months are uploaded ----
  # Only the latest year in the data, so year-to-date totals don't mix years
  trend_data <- reactive({
    d <- data()
    d %>% filter(year(date) == year(max(date)))
  })
  months_axis <- scale_x_date(date_breaks = '1 month', date_labels = '%b')
  
  # Turns a ggplot into an interactive chart: bold title, and the number shows when you hover
  hover <- function(p, title) {
    ggplotly(p, tooltip = 'text') %>%
      layout(title = list(text = paste0('<b>', title, '</b>'), x = 0.01, font = list(size = 20)),
             margin = list(t = 60),
             legend = list(orientation = 'h', x = 0.35, y = -0.12)) %>%
      config(displayModeBar = FALSE)
  }
  
  # 1. EMTs vs goal
  output$g_goal <- renderPlotly({
    d <- trend_data() %>% filter(!is.na(bls_emts))
    d$tip <- paste0(format(d$date, '%B'), ': ', d$bls_emts, ' EMTs')
    p <- ggplot(d, aes(date, bls_emts, text = tip, group = 1)) +
      geom_hline(yintercept = input$target, colour = 'red', linetype = 'dashed') +
      annotate('text', x = min(d$date), y = input$target + 2, label = paste('Goal:', input$target),
               colour = 'red', hjust = 0) +
      geom_line() +
      geom_point(size = 2) +
      geom_text(aes(y = bls_emts + 3.5, label = bls_emts), size = 3.5) +
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
      geom_col(fill = 'steelblue', width = 0.6) +
      geom_text(aes(y = value + max(value, na.rm = TRUE) * 0.05, label = value), size = 4.5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = NULL, y = 'People')
    hover(p, paste('Where members are in', format(r$date, '%B')))
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
      geom_col(fill = 'steelblue', width = 0.7) +
      geom_text(aes(y = total + max(total) * 0.11, label = lab), size = 3.8) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = NULL, y = 'People')
    hover(p, paste0('Pipeline, Jan to ', format(max(d$date), '%b')))
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
      scale_fill_manual(values = c(Joined = 'seagreen', Left = 'firebrick')) +
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
      geom_text(aes(y = total + max(total) * 0.05, label = total), size = 4.5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(x = NULL, y = 'People lost')
    hover(p, paste0('Where we lose people, Jan to ', format(max(d$date), '%b')))
  })
  output$stageplot<-renderPlotly({req(input$stage);validate(need(input$stage %in% names(labels),'Select a valid series'));d<-data();d$tip<-paste0(format(d$date,'%B'),': ',d[[input$stage]]);p<-ggplot(d,aes(date,.data[[input$stage]],text=tip,group=1))+geom_line(color='#176b79',linewidth=1.2)+geom_point(color='#176b79',size=2)+theme_minimal(base_size=12)+labs(x=NULL,y='Reported count');hover(p,unname(labels[[input$stage]]))})
  output$source<-renderDT(datatable(data(),options=list(scrollX=TRUE,pageLength=10)))
  output$template<-downloadHandler(filename=function()'bls_monthly_update_template.csv',content=function(file){d<-data();d$date<-format(d$date,'%Y-%m-%d');write.csv(d[,setdiff(names(base),'month')],file,row.names=FALSE,na='')})
  output$export<-downloadHandler(filename=function()'bls_funnel_forecast.csv',content=function(file){write.csv(model()$f,file,row.names=FALSE)})
}
shinyApp(ui,server)