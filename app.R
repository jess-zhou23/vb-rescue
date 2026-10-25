library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)

# Robust date parser: accepts ISO (2026-09-01, 2026/09/01) and US month/day/year (9/1/2026).
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
base <- read.csv('bls_2026.csv', stringsAsFactors=FALSE)
base$date <- parse_dates(base$date)
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
  fluidRow(column(4,div(class='metric',strong('Latest Active BLS Providers'),h2(textOutput('current',inline=TRUE)))),
           column(4,div(class='metric',strong('Status Quo Trend Target Date'),h2(textOutput('trend_eta',inline=TRUE)))),
           column(4,div(class='metric',strong('Funnel Scenario Target Date'),h2(textOutput('scenario_eta',inline=TRUE))))),
  tabsetPanel(
   tabPanel('Forecast',plotOutput('forecast',height='410px'),tableOutput('comparison'),p(class='note','Funnel forecasts are scenario calculations, not statistical prediction intervals. The 2025 ratios may mix recruitment cohorts.')),
   tabPanel('2026 trends',plotOutput('trends',height='440px'),selectInput('stage','Explore a membership or pipeline measure',choices=setNames(names(labels),unname(labels)),selected='bls_emts'),plotOutput('stageplot',height='290px')),
   tabPanel('2025 funnel & assumptions',h4('Historical 2025 funnel'),tableOutput('funnel_table'),h4('2025 reported pipeline losses'),p('47 approved applicants, 105 academy students or testing candidates, and 45 interns were lost from the recruitment pipeline. These are NOT active BLS workforce exits, and are not subtracted from membership. They may overlap with the funnel ratios.'),h4('Scenario calculation'),verbatimTextOutput('math_summary'),p(class='note','Annual counts are ratios of annual totals, not verified same-cohort conversion probabilities. Academy seat capacity is an upper bound, not a target. Application and graduation totals may contain non-BLS candidates; verify with the client before interpreting this as a BLS-only pipeline.'),h4('2026 source data'),DTOutput('source'))
  )
 ))
)
server <- function(input,output,session){
 data <- reactive({
  d<-base
  if(!is.null(input$upload)){
   u<-tryCatch(read.csv(input$upload$datapath),error=function(e)NULL)
   if(!is.null(u)&&all(c('date','bls_emts','new_bls')%in%names(u))){
    u$date<-parse_dates(u$date)
    u<-u[!is.na(u$date)&!is.na(u$bls_emts)&!is.na(u$new_bls),,drop=FALSE]
    if(nrow(u)){
     for(k in setdiff(names(d),names(u)))u[[k]]<-NA_real_
     u<-u[,names(d),drop=FALSE]
     d<-bind_rows(d[!d$date%in%u$date,],u)|>arrange(date)
    }
   }
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
 output$trends<-renderPlot({d<-data();p<-d|>select(date,bls_emts,new_bls,bls_interns,testing)|>pivot_longer(-date,names_to='series',values_to='count');ggplot(p,aes(date,count,color=series))+geom_line(linewidth=1)+geom_point()+facet_wrap(~series,scales='free_y',ncol=2,labeller=as_labeller(labels))+scale_color_manual(values=c(bls_emts='#e8796d',bls_interns='#83a928',new_bls='#5ab6bf',testing='#b67af0'))+theme_minimal(base_size=12)+theme(legend.position='none',strip.text=element_text(face='bold',size=11))+labs(x=NULL,y='Monthly reported count')})
 output$stageplot<-renderPlot({req(input$stage);validate(need(input$stage %in% names(labels),'Select a valid series'));ggplot(data(),aes(date,.data[[input$stage]]))+geom_line(color='#176b79',linewidth=1.2)+geom_point(color='#176b79',size=2)+theme_minimal(base_size=12)+labs(title=unname(labels[[input$stage]]),x=NULL,y='Reported count')})
 output$source<-renderDT(datatable(data(),options=list(scrollX=TRUE,pageLength=10)))
 output$template<-downloadHandler(filename=function()'bls_monthly_update_template.csv',content=function(file){write.csv(data()[,c('date','bls_emts','new_bls','pending_academy','awaiting_academy','testing','bls_interns')],file,row.names=FALSE,na='')})
 output$export<-downloadHandler(filename=function()'bls_funnel_forecast.csv',content=function(file){write.csv(model()$f,file,row.names=FALSE)})
}
shinyApp(ui,server)

