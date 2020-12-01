source('appTestingData.R')

## new method
#get dataset of unique benthic sites and ecoregion
# if data exists, show to user with suggested score, override if needed
# plotly some shit


# Bring in latest EDAS VSCI and (combined) VCPMI queries
VSCI <- read_excel('data/Family Metrics VSCI Calculation.xlsx')%>%
  filter(RepNum == 1 & Target_Count == 110 &
           CollDate >= assessmentPeriod[1] )
VCPMI <- read_excel('data/Family Metrics - CPMI Combined.xlsx')%>%
  filter(RepNum == 1 & Target_Count == 110 &
           CollDate >= assessmentPeriod[1] )

x2 <- filter(VSCI, StationID %in% x$FDT_STA_ID) 
x3 <- filter(VCPMI, StationID %in% x$FDT_STA_ID)

# Assessment Functions
benthicResultMetrics <- function(x, VSCI, VCPMI){
  out <- list()
  # VCPMI Ecoregion 63 + Chowan
  if(unique(x$US_L3CODE) %in% 63 & 
     unique(x$Basin) %in% 'Chowan and Dismal Swamp River Basin'){
    z <- filter(VCPMI, StationID %in% x$FDT_STA_ID) %>% mutate(SCI = 'VCPMI')}
  if(unique(x$US_L3CODE) %in% 65  & 
     !(unique(x$Basin) %in% 'Chowan and Dismal Swamp River Basin' ) ){
    z <- filter(VCPMI, StationID %in% x$FDT_STA_ID) %>% mutate(SCI = 'VCPMI')}
  if(unique(x$US_L3CODE) %in% c(45, 64, 66, 67, 69)){
    z <- filter(VSCI, StationID %in% x$FDT_STA_ID) %>% mutate(SCI = 'VSCI') }
    
  if(nrow(z) > 0){
    z1 <- mutate(z, Year = lubridate::year(CollDate))
    out$data <- z1
    spring <- filter(z1, Season %in% 'Spring' )
    fall <- filter(z1, Season %in% 'Fall' )
    # output list with all metrics
    nSamples <- nrow(z1)
    averageSCI <- format(mean(z1$`Fam SCI`), digits = 3)
    nSpringSample <- nrow(spring)
    nFallSample <- nrow(fall)
    minFamSCI <- format(min(z1$`Fam SCI`), digits = 3)
    maxFamSCI <- format(max(z1$`Fam SCI`), digits = 3)
    springAverage <- as.numeric(summarise(spring, springAverage = format(mean(`Fam SCI`), digits = 3)))
    fallAverage <- as.numeric(summarise(fall, fallAverage = format(mean(`Fam SCI`), digits = 3)))
    out$roundup <- tibble(StationID = unique(z1$StationID),  
                          `n Samples`=nSamples, `n Spring Samples`= nSpringSample, `n Fall Samples` = nFallSample,
                          `Average SCI` =averageSCI, `Spring Average SCI`=springAverage, `Fall Average SCI`= fallAverage,
                          `Minimum SCI` = minFamSCI, `Maximum SCI`= maxFamSCI)
    
    out$yearlyAverage <- z1 %>%
      group_by(Year) %>%
      summarise(yearAverage = format(mean(`Fam SCI`), digits = 3)) 
    
    
  } else {
    out$data <- NA
    
    out$roundup <- tibble(StationID = NA,  
                          `n Samples`=NA, `n Spring Samples`= NA, `n Fall Samples` = NA,
                          `Average SCI` =NA, `Spring Average SCI`=NA, `Fall Average SCI`= NA,
                          `Minimum SCI` = NA, `Maximum SCI`= NA)
    
    #out$nSamples <- NA
    #out$averageSCI <- NA
    #out$nSpringSample <- NA
    #out$nFallSample <-NA
    #out$minFamSCI <- NA
    #out$maxFamSCI <- NA
    
    out$yearlyAverage <- tibble(Year= NA, yearAverage=NA)
    
    #out$springAverage <- NA
    #out$fallAverage <- NA
  }
    return(out)
  
}

#metrics <- benthicResultMetrics(x, VSCI, VCPMI)


benthicAssessment <- function(x,conventionals_sf,VSCI,VCPMI){
  x <- filter(conventionals_sf, FDT_STA_ID %in% x$FDT_STA_ID)#'2-JKS033.06') #'2-JMS279.41')##
  if(nrow(x) >0){
    x2 <- benthicResultMetrics(x,VSCI,VCPMI)$data
    if (!any(is.na(x2))){return(data.frame(BENTHIC_STAT='Review'))
    }else{return(data.frame(BENTHIC_STAT=NA))}
  } else{return(data.frame(BENTHIC_STAT=NA))}
}
benthicAssessment(x,conventionals_sf,VSCI,VCPMI)

# Sampling Metrics functions







BenthicsPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('Benthics_oneStationSelectionUI')),
      #verbatimTextOutput(ns('test')),
      plotlyOutput(ns('Benthicsplotly')),
      br(), hr(), br(),
      h4('Stream Condition Index Sampling Metrics'),
      fluidRow(
        column(8, h5('Sampling and SCI metrics in Assessment Window: '), DT::dataTableOutput(ns('SCIroundup'))),
        column(4, h5('Yearly Average SCI in Assessment Window: '), DT::dataTableOutput(ns('yearlyAvgSCI')))),
      br(),
      h4('Raw Benthic Results'),
      DT::dataTableOutput(ns('rawBenthicsData')))
        #column(4, h5('Total Number of Samples in Assessment Window: '), textOutput(ns('nSamples')) ,
        #       h5('Total Number of Spring Samples in Assessment Window: '), textOutput(ns('nSpringSamples')),
        #       h5('Total Number of Fall Samples in Assessment Window: '), textOutput(ns('nFallSamples')))  ,
        #column(4, h5('Average SCI in Assessment Window: '), textOutput(ns('avgSCI')) ,
        #       h5('Minimum SCI in Assessment Window: '), textOutput(ns('minSCI')),
        #       h5('Maximum SCI in Assessment Window: '), textOutput(ns('maxSCI')))  ,
        #column(4, h5('Yearly Average SCI in Assessment Window: '), tableOutput(ns('yearlyAvgSCI')) ,
        #       h5('Spring Average SCI in Assessment Window: '), textOutput(ns('springAvgSCI')),
        #       h5('Fall SCI in Assessment Window: '), textOutput(ns('fallAvgSCI'))  )),
      #DT::dataTableOutput(ns('rawBenthicsData')))
  )
}


BenthicsPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove, conventionals_sf, VSCI, VCPMI){
  ns <- session$ns
  
  # Select One station for individual review
  output$Benthics_oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('Benthics_oneStationSelection'),strong('Select Station to Review'),choices=unique(AUdata())$FDT_STA_ID,width='300px')})
  
  output$Benthics_oneStationSelectionUI <- renderUI({
    req(#AUdata, 
      stationSelectedAbove)
    #print(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID)))
    selectInput(ns('Benthics_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  Benthics_oneStation <- reactive({
    req(ns(input$Benthics_oneStationSelection))
    benthicResultMetrics(filter(conventionals_sf,FDT_STA_ID %in% input$Benthics_oneStationSelection), VSCI, VCPMI) })
  
  #output$test <- renderPrint({
  #  req(ns(input$Benthics_oneStationSelection), Benthics_oneStation())
  #  Benthics_oneStation()$data
  #})

  output$Benthicsplotly <- renderPlotly({
    req(input$Benthics_oneStationSelection, Benthics_oneStation())
    dat <- Benthics_oneStation()$data
    dat$SampleDate <- as.POSIXct(dat$CollDate, format="%y-%m-%d")
    
    if(unique(dat$SCI) == "VSCI"){
      dat <- mutate(dat, bottom = 60)
      box1 <- data.frame(SampleDate = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(60, 100, 100, 60))
      box2 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0, 60, 60, 0))
    } else {
      dat <- mutate(dat, bottom = 40)
      box1 <- data.frame(SampleDate = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(40, 100, 100, 40))
      box2 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0, 40, 40, 0))
    }
    
    plot_ly(data=dat)%>%
      add_polygons(x = ~SampleDate, y = ~y, data = box1, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Pass Stream Condition Index')) %>%
      add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Fail Stream Condition Index')) %>%
      add_lines(data=dat, x=~SampleDate,y=~bottom, mode='line', line = list(color = 'black'),
                hoverinfo = "text", text= "SCI Standard", name="SCI Standard") %>%
      add_markers(data=dat, x= ~SampleDate, y= ~`Fam SCI`,mode = 'scatter', name=paste(unique(dat$SCI),"(unitless)"),marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste(unique(dat$SCI),format(`Fam SCI`,digits=3),"unitless")))%>%
      layout(showlegend=FALSE,
             yaxis=list(title=paste(unique(dat$SCI),"(unitless)")),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))
  })
  
  output$SCIroundup <- DT::renderDataTable({
    req(Benthics_oneStation())
    DT::datatable(Benthics_oneStation()$roundup, escape=F, rownames = F,
                  options= list(dom= 't' , scrollX = TRUE, pageLength = nrow(Benthics_oneStation()$roundup), scrollY = "150px"))})
  
  
  output$yearlyAvgSCI <- DT::renderDataTable({
    req(Benthics_oneStation())
    DT::datatable(Benthics_oneStation()$yearlyAverage, escape=F, rownames = F,
                  options= list(dom= 't' , pageLength = nrow(Benthics_oneStation()$yearlyAverage), scrollY = "150px"))})
  
  output$rawBenthicsData <- DT::renderDataTable({
    req(ns(input$Benthics_oneStationSelection), Benthics_oneStation())
    z <- as.tibble(Benthics_oneStation()$data)
    z$CollDate <- as.character(z$CollDate)
    print(str(z))
    DT::datatable(z, escape=F, rownames = F,
                  options= list(dom= 't' , scrollX = TRUE, pageLength = nrow(Benthics_oneStation()$data), scrollY = "300px"))%>%
      formatRound(columns=c("%Ephem", "%PT - Hydropsychidae", "Fam%Scrap", "%Chiro",  "Fam%2Dom", "FamHBI", "%Ephem Score", 
                            "%PT-H Score",  "Fam Richness Score",  "%Chironomidae Score", "Fam EPT Score", "Fam %Scraper Score",
                            "Fam %2Dom Score", "Fam %MFBI Score" , "Fam SCI" ), digits=1) 
  })
  
}
  

ui <- fluidPage(
  helpText('Review each site using the single site visualization section. There are no WQS for Specific Conductivity.'),
  BenthicsPlotlySingleStationUI('Benthics')
)

server <- function(input,output,session){
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  AUData <- reactive({filter(conventionals_HUC, ID305B_1 %in% 'VAW-I09R_JKS06A00' | 
                               ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
                               ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
      left_join(WQSvalues, by = 'CLASS')})
  
  callModule(BenthicsPlotlySingleStation,'Benthics', AUData, stationSelected, conventionals_sf, VSCI, VCPMI)
  
}

shinyApp(ui,server)


  



#output$nSamples <- renderPrint({
#  req(Benthics_oneStation()) 
#  cat(Benthics_oneStation()$nSamples)})
#output$nSpringSamples <- renderPrint({
#  req(Benthics_oneStation()) 
#  cat(Benthics_oneStation()$nSpringSample)})
#output$nFallSamples <- renderPrint({
#  req(Benthics_oneStation()) 
#  cat(Benthics_oneStation()$nFallSample)})
#output$avgSCI <- renderPrint({
#  req(Benthics_oneStation()) 
#  cat(format(Benthics_oneStation()$averageSCI,digits=3))})
#output$minSCI <- renderPrint({
#  req(Benthics_oneStation()) 
#  cat(format(Benthics_oneStation()$minFamSCI,digits=3))})
#output$maxSCI <- renderPrint({
#  req(Benthics_oneStation())
#  cat(format(Benthics_oneStation()$maxFamSCI,digits=3))})
#output$springAvgSCI <- renderPrint({
#  req(Benthics_oneStation()) 
#  cat(format(Benthics_oneStation()$springAverage,digits=3))})
#output$fallAvgSCI <- renderPrint({
#  req(Benthics_oneStation()) 
#  cat(format(Benthics_oneStation()$fallAverage,digits=3))})





