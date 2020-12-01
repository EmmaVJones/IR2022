


BenthicsPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      uiOutput(ns('Benthics_oneStationSelectionUI')),
      plotlyOutput(ns('Benthicsplotly')),
      br(), hr(), br(),
      h4('Stream Condition Index Sampling Metrics'),
      fluidRow(
        column(8, h5('Sampling and SCI metrics in Assessment Window: '), DT::dataTableOutput(ns('SCIroundup'))),
        column(4, h5('Yearly Average SCI in Assessment Window: '), DT::dataTableOutput(ns('yearlyAvgSCI')))),
      br(),br(),
      h4('Raw Benthic Results'),
      DT::dataTableOutput(ns('rawBenthicsData')))
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
    selectInput(ns('Benthics_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  Benthics_oneStation <- reactive({
    req(ns(input$Benthics_oneStationSelection))
    benthicResultMetrics(filter(conventionals_sf,FDT_STA_ID %in% input$Benthics_oneStationSelection), VSCI, VCPMI) })
  
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
    DT::datatable(z, escape=F, rownames = F,
                  options= list(dom= 't' , scrollX = TRUE, pageLength = nrow(Benthics_oneStation()$data), scrollY = "300px")) %>%
      formatRound(columns=c("%Ephem", "%PT - Hydropsychidae", "Fam%Scrap", "%Chiro",  "Fam%2Dom", "FamHBI", "%Ephem Score", 
                            "%PT-H Score",  "Fam Richness Score",  "%Chironomidae Score", "Fam EPT Score", "Fam %Scraper Score",
                            "Fam %2Dom Score", "Fam %MFBI Score" , "Fam SCI" ), digits=1) 
  })
  
}
