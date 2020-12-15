source('appTestingData.R')


BenthicsPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(2,uiOutput(ns('oneStationSelectionUI'))),
               column(2,br(),checkboxInput(ns('wadeableOnly'), 
                                           span('Only analyze wadeable method samples.',strong('The SCI methods are only validated on wadeable methods.'),
                                                'Boatable data should be viewed for informational purposed only.'), value = TRUE)),
               column(2, br(), checkboxInput(ns('rep1Only'), span('Only analyze replicate 1 samples.'), value = TRUE)),
               column(3, br(),DT::dataTableOutput(ns('ecoregionInfo'))),
               column(1),
               column(2,br(),uiOutput(ns('changeSCIUI')),
                      helpText('The default SCI method is selected for you using major basin and ecoregion information.'))),
      br(),
      helpText('All data presented in the interactive plot is raw data. Rounding rules are appropriately applied to the 
               assessment functions utilized by the application.'),
      #verbatimTextOutput(ns('test')),
      plotlyOutput(ns('plotly')),
      br(),hr(),br(),
      h4('Stream Condition Index Sampling Metrics'),
      fluidRow(
        column(8, h5('Sampling and SCI metrics in Assessment Window: '), DT::dataTableOutput(ns('SCIroundup'))),
        column(4, h5('Yearly Average SCI in Assessment Window: '), br(), DT::dataTableOutput(ns('yearlyAvgSCI')))),
      br(),
      h4('Raw Benthic Results'),
      DT::dataTableOutput(ns('rawBenthicsData')))
  )
}


BenthicsPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove, VSCIresults, VCPMI63results, VCPMI65results){
  ns <- session$ns
  
  # Select One station for individual review
  output$oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='200px', selected = stationSelectedAbove())})
  
  oneStation <- reactive({
    req(ns(input$oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$oneStationSelection) })
  
  # display Ecoregion and Basin information
  output$ecoregionInfo <- DT::renderDataTable({  req(oneStation())
    DT::datatable(dplyr::select(oneStation(), Basin = Basin_Code, `Level III Ecoregion` = EPA_ECO_US_L3CODE,
                                `Level III Ecoregion Name` = EPA_ECO_US_L3NAME), 
                  rownames = FALSE, options= list(dom= 't', pageLength = 1, scrollX = TRUE, scrollY = "60px", dom='t'),
                  selection = 'none') })
  
  
  # Option to change SCI used for modal
  output$changeSCIUI <- renderUI({
    req(oneStation())
    selectInput(ns('changeSCI'),strong('SCI For Analysis'),
                choices= c('VSCI', "VCPMI 63 + Chowan", "VCPMI 65 - Chowan"),
                width='400px', selected = SCIchooser(oneStation()))})
  
  Benthics_oneStation <- reactive({
    req(ns(input$changeSCI),oneStation(), ns(input$wadeableOnly))
    benthicResultsMetrics(oneStation(), 
                          switch(input$changeSCI,
                                 "VSCI" = VSCIresults,
                                 "VCPMI 63 + Chowan" = VCPMI63results, 
                                 "VCPMI 65 - Chowan" = VCPMI65results),
                          wadeableOnly = input$wadeableOnly,
                          rep1Only = input$rep1Only)})
  
  output$plotly <- renderPlotly({
    req(nrow(Benthics_oneStation()$data) > 0)
    dat <- Benthics_oneStation()$data
    dat$SampleDate <- as.POSIXct(dat$`Collection Date`, format="%y-%m-%d")
    
    # Fix look of single sample date
    if(length(unique(dat$SampleDate)) == 1){
      dat <- bind_rows(dat,
                       tibble(SampleDate = c(dat$SampleDate- days(5), dat$SampleDate + days(5)),
                              SCI = unique(dat$SCI),
                              `SCI Threshold` = unique(dat$`SCI Threshold`)))
    }
    
    
    if(unique(dat$SCI) == "VSCI"){
      box1 <- data.frame(SampleDate = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(60, 100, 100, 60))
      box2 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0, 60, 60, 0))
    } else {
      box1 <- data.frame(SampleDate = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(42, 100, 100, 42))
      box2 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0, 42, 42, 0))
    }
    
    plot_ly(data=dat)%>%
      add_polygons(x = ~SampleDate, y = ~y, data = box1, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Pass Stream Condition Index')) %>%
      add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
                   hoverinfo="text", name =paste('Fail Stream Condition Index')) %>%
      add_lines(data=dat, x=~SampleDate,y=~`SCI Threshold`, mode='line', line = list(color = 'black'),
                hoverinfo = "text", text= "SCI Criteria", name="SCI Criteria") %>%
      add_markers(data=dat, x= ~SampleDate, y= ~`SCI Score`,mode = 'scatter', name=paste(unique(dat$SCI),"(unitless)"),marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste(unique(dat$SCI),format(`SCI Score`,digits=3),"unitless"),
                                               paste('Method:', dat$Gradient)))%>%
      layout(showlegend=FALSE,
             yaxis=list(title=paste(unique(dat$SCI),"(unitless)")),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))
  })
  
  
  
  output$SCIroundup <- DT::renderDataTable({
    req(nrow(Benthics_oneStation()$roundup) > 0)
    DT::datatable(Benthics_oneStation()$roundup, escape=F, rownames = F,
                  options= list(dom= 't' , scrollX = TRUE, pageLength = nrow(Benthics_oneStation()$roundup), scrollY = "150px"),
                  selection = 'none')})
  
  
  output$yearlyAvgSCI <- DT::renderDataTable({
    req(nrow(Benthics_oneStation()$yearlyAverage) > 0)
    DT::datatable(Benthics_oneStation()$yearlyAverage, escape=F, rownames = F,
                  options= list(dom= 't' , pageLength = nrow(Benthics_oneStation()$yearlyAverage), scrollY = "150px"),
                  selection = 'none')})
  
  
  output$rawBenthicsData <- DT::renderDataTable({
    req(ns(input$Benthics_oneStationSelection), nrow(Benthics_oneStation()$data) > 0)
    z <- dplyr::select(Benthics_oneStation()$data, StationID:Season, BenSampID:`SCI Threshold`, everything())
    DT::datatable(z, escape=F, rownames = F,
                  options= list(dom= 't' , scrollX = TRUE, pageLength = nrow(z), scrollY = "300px"),
                  selection = 'none')#%>%
    #formatRound(columns=c("%Ephem", "%PT - Hydropsychidae", "Fam%Scrap", "%Chiro",  "Fam%2Dom", "FamHBI", "%Ephem Score", 
    #                     "%PT-H Score",  "Fam Richness Score",  "%Chironomidae Score", "Fam EPT Score", "Fam %Scraper Score",
    #                     "Fam %2Dom Score", "Fam %MFBI Score" , "Fam SCI" ), digits=1) 
  })
  
  #output$test <- renderPrint({
  #  #input$wadeableOnly
  #  Benthics_oneStation()
  #})
}



ui <- fluidPage(
  helpText('Review each site using the single site visualization section. There are no WQS for Specific Conductivity.'),
  BenthicsPlotlySingleStationUI('Benthics')
)

server <- function(input,output,session){
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  AUData <- reactive({filter_at(conventionals_HUC, vars(starts_with("ID305B")), any_vars(. %in% AUselection) ) })
  
  
  callModule(BenthicsPlotlySingleStation,'Benthics', AUData, stationSelected, VSCIresults, VCPMI63results, VCPMI65results)
  
}

shinyApp(ui,server)

