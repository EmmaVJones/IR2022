source('testingDataset.R')

#AUData <- filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
#                   ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
#                   ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
#  left_join(WQSvalues, by = 'CLASS')

x <-filter(conventionals_HUC, FDT_STA_ID %in% '4APKP-4-DRBA')# '4APKP-4-DRBA') '8-YRK022.70')#


x <- mutate(x, ENTEROCOCCI = `E._COLI_31648_NO/100mL`, ECOLI_RMK = RMK_31648)




EnteroPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(6, uiOutput(ns('Entero_oneStationSelectionUI'))),
               column(6,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      plotlyOutput(ns('Enteroplotly')),
      br(),hr(),br(),
      fluidRow(
        column(6, h5('All enterococci records that are above the criteria for the ',span(strong('selected site')),' are highlighted below.'),
               h4(strong('Old Standard (Monthly Geomean = 35 CFU / 100 mL)')),
               DT::dataTableOutput(ns('EnteroexceedancesOldStdTableSingleSitegeomean')),
               h4(strong('Old Standard (Single Sample Maximum = 104 CFU / 100 mL)')),
               DT::dataTableOutput(ns('EnteroexceedancesOldStdTableSingleSiteSTV')), br(), br(), hr(), br(), br(), 
               h4(strong('New Standard (STV= 130 CFU / 100 mL, geomean = 35 CFU / 100 mL with additional sampling requirements)')),
               helpText('The below table highlights all analyzed windows that have either STV violations OR geomean violations. Note
                        the number of samples in the window, STV Assessment, and Geomean Assessment columns for context. These violations
                        are important to understand the dataset, but assessment decision in the table to the right is where one should look
                        for assistance choosing which of the potential violations are driving the decision. Explore the dataset in 
                        90 day windows in the interactive graph below and the full dataset with assessment decisions paired with each window
                        in the bottom-most table.'),
               DT::dataTableOutput(ns('EnteroexceedancesNEWStdTableSingleSite'))),
        column(6, h5('Individual enterococci exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
               h4(strong('Old Standard (Single Sample Maximum = 104 CFU / 100 mL, geomean = 35 CFU / 100 mL)')), 
               DT::dataTableOutput(ns("EnteroOldStdTableSingleSite")), br(), br(), br(),   br(),br(),br(), br(),  br(), br(), br(), br(), br(), br(), br(), br(), br(), hr(), br(), br(),
               h4(strong('New Standard (STV= 130 CFU / 100 mL, geomean = 35 CFU / 100 mL with additional sampling requirements)')), 
               DT::dataTableOutput(ns("EnteroNEWStdTableSingleSite")),
               h4(strong('See below section for detailed analysis with new recreation standard.')))),
      hr(),br(),
      h4(strong('New Recreation Standard Analysis')),
      helpText('Review the 90 day windows (identified by each sample date) for STV and geomean exceedances.
               Comments are specific to each row of data. To view the dataset within each 90 day window, use
               the drop down box to select the start of the window in question.'),
      fluidRow(
        column(6, helpText('Below is the raw data associated with the ',span('selected site'),'.'), 
               DT::dataTableOutput(ns('rawData'))),
        column(6, uiOutput(ns('windowChoice')),
               plotlyOutput(ns('EnteroplotlyZoom')))),
      br(), br(),
      h5(strong('Analyzed Data (Each window with an individual assessment decision)')),
      DT::dataTableOutput(ns('analysisTable')))
    
    
  )
}





EnteroPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$Entero_oneStationSelectionUI <- renderUI({
    req(stationSelectedAbove)
    selectInput(ns('Entero_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})
  
  Entero_oneStation <- reactive({
    req(ns(input$Entero_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$Entero_oneStationSelection)  %>%
      filter(!is.na(ENTEROCOCCI))})
  
  # Button to visualize modal table of available parameter data
  observeEvent(input$reviewData,{
    showModal(modalDialog(
      title="Review Raw Data for Selected Station and Parameter",
      helpText('This table subsets the conventionals raw data by station selected in Single Station Visualization Section drop down and
               parameter currently reviewing. Scroll right to see the raw parameter values and any data collection comments. Data analyzed
               by app is highlighted in gray (all DEQ data and non agency/citizen monitoring Level III), data counted by app and noted in
               comment fields is highlighed in yellow (non agency/citizen monitoring Level II), and data NOT CONSIDERED in app is noted in
               orange (non agency/citizen monitoring Level I).'),
      DT::dataTableOutput(ns('parameterData')),
      easyClose = TRUE))  })
  
  # modal parameter data
  output$parameterData <- DT::renderDataTable({
    req(Entero_oneStation())
    parameterFilter <- dplyr::select(Entero_oneStation(), FDT_STA_ID:FDT_COMMENT, ENTEROCOCCI, RMK_31649)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t')) %>%
      formatStyle(c('ENTEROCOCCI','RMK_31649'), 'RMK_31649', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })

  
  output$Enteroplotly <- renderPlotly({
    req(input$Entero_oneStationSelection, Entero_oneStation())
    dat <- Entero_oneStation() %>%
      mutate(newSTV = 130, geomean = 35, oldSTV = 104)
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
    plot_ly(data=dat) %>%
      add_markers(x= ~SampleDate, y= ~ENTEROCOCCI,mode = 'scatter', name="Enterococci (CFU / 100 mL)", marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Enterococci: ",ENTEROCOCCI,"CFU / 100 mL")))%>%
      add_lines(data=dat, x=~SampleDate,y=~newSTV, mode='line', line = list(color = '#484a4c',dash = 'dot'),
                hoverinfo = "text", text= "New STV: 130 CFU / 100 mL", name="New STV: 130 CFU / 100 mL") %>%
      add_lines(data=dat, x=~SampleDate,y=~oldSTV, mode='line', line = list(color = 'black'),
                hoverinfo = "text", text= "Old SSM: 104 CFU / 100 mL", name="Old SSM: 104 CFU / 100 mL") %>%
      add_lines(data=dat, x=~SampleDate,y=~geomean, mode='line', line = list(color = 'black', dash= 'dash'),
                hoverinfo = "text", text= "Geomean: 35 CFU / 100 mL", name="Geomean: 35 CFU / 100 mL") %>%
      layout(showlegend=FALSE,
             yaxis=list(title="Enterococci (CFU / 100 mL)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) 
  })
  
  output$EnteroexceedancesOldStdTableSingleSitegeomean <- DT::renderDataTable({
    req(Entero_oneStation())
    z <- bacteria_ExceedancesGeomeanOLD(Entero_oneStation() %>% 
                                          dplyr::select(FDT_DATE_TIME2,ENTEROCOCCI)%>% # Just get relavent columns, 
                                          filter(!is.na(ENTEROCOCCI)), #get rid of NA's
                                        'ENTEROCOCCI', 35) %>%
      dplyr::select(FDT_DATE_TIME2, ENTEROCOCCI, sampleMonthYear, geoMeanCalendarMonth, limit, samplesPerMonth) %>%
      rename(FDT_DATE_TIME = FDT_DATE_TIME2) %>%# for user view consistency, same data, just different format for R purposes
      filter(samplesPerMonth > 4, geoMeanCalendarMonth > limit) # minimum sampling rule for geomean to apply
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t')) 
    
  })
  
  output$EnteroexceedancesOldStdTableSingleSiteSTV <- DT::renderDataTable({
    req(Entero_oneStation())
    z <- bacteria_ExceedancesSTV_OLD(Entero_oneStation() %>%
                                       dplyr::select(FDT_DATE_TIME2,ENTEROCOCCI)%>% # Just get relavent columns, 
                                       filter(!is.na(ENTEROCOCCI)) #get rid of NA's
                                     , 130 ) %>%
      filter(exceeds == T) %>%
      mutate(FDT_DATE_TIME = as.character(FDT_DATE_TIME2), ENTEROCOCCI = parameter) %>%
      dplyr::select(FDT_DATE_TIME, ENTEROCOCCI, limit, exceeds)
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t')) 
  })
  
  output$EnteroOldStdTableSingleSite <- DT::renderDataTable({
    req(Entero_oneStation())
    z1 <- filter(Entero_oneStation(), !(RMK_31649 %in% c('Level II', 'Level I')))
    if(nrow(z1) > 1){
      z <- bacteria_Assessment_OLD(z1,  'ENTEROCOCCI', 35, 104) #bacteria_Assessment_OLD(Entero_oneStation(), 'ENTEROCOCCI', 'RMK_31649', 126, 235) 
      if(nrow(z) > 0 ){
        z <- dplyr::select(z, `Assessment Method`,everything()) }
      DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'))
    } })
    
    #z <- bacteria_Assessment_OLD(Entero_oneStation(), 'ENTEROCOCCI', 35, 104) %>% dplyr::select(`Assessment Method`,everything())
    #DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'))  })
  
  ### New standard ----------------------------------------------------------------------------------
  newSTDbacteriaData <- reactive({
    req(Entero_oneStation())
    z <- citmonOutOfBacteria(Entero_oneStation(), ENTEROCOCCI, RMK_31649)
    if(nrow(z) > 1){
      conventionalsToBacteria(z, 'ENTEROCOCCI')
    }   }) 
    #conventionalsToBacteria(Entero_oneStation(), 'ENTEROCOCCI')})  
  
  output$EnteroexceedancesNEWStdTableSingleSite <- DT::renderDataTable({
    req(Entero_oneStation(),newSTDbacteriaData())
    z <- bacteriaExceedances_NewStd(newSTDbacteriaData(), 10, 130, 35) %>% 
      filter(`STV Exceedances In Window` > 0 | `Geomean In Window` > 35) %>%
      dplyr::select(-associatedData) # remove embedded tibble to make table work
    z$`Date Window Starts` <- as.character(z$`Date Window Starts`)
    z$`Date Window Ends` <- as.character(z$`Date Window Ends`)
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t')) 
  })
  
  
  output$EnteroNEWStdTableSingleSite <- DT::renderDataTable({
    req(Entero_oneStation(),newSTDbacteriaData())
    z <- bacteriaAssessmentDecision(newSTDbacteriaData(), 10, 130, 35)  %>%
      distinct(`Assessment Decision`) %>% # only grab 1 record
      mutate(`Assessment Method`= 'New Recreation Standard') %>%
      dplyr::select(`Assessment Method`, `Assessment Decision`) #only grab decision
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t')) 
  })
  
  
  output$windowChoice <- renderUI({
    req(Entero_oneStation(),newSTDbacteriaData())
    fluidRow(
      column(4, selectInput(ns('windowChoice_'),'Select 90 day window start date',
                            choices = unique(newSTDbacteriaData()$`Date Time`), width = '100%')),
      column(8, helpText('Orange line corresponds to the window geomean; wide black dashed line
                         corresponds to the geomean criteria; thin black dashed line corresponds
                         to the STV limit.')))})
  
  output$EnteroplotlyZoom <- renderPlotly({
    req(input$windowChoice_, Entero_oneStation(),newSTDbacteriaData())
    windowStart <- bacteriaExceedances_NewStd(newSTDbacteriaData(), 10, 130, 35) %>%
      filter(`Date Window Starts` == input$windowChoice_) #'2011-05-24')#
    
    windowData <- dplyr::select(windowStart, associatedData) %>%
      unnest() %>%
      rename(ENTEROCOCCI_geomean='E.COLI_geomean') %>% # fix column name that comes out of associatedData slot
      mutate(`Date Window Starts` = as.POSIXct(unique(windowStart$`Date Window Starts`, format="%m/%d/%y")),
             `Date Window Ends` = as.POSIXct(unique(windowStart$`Date Window Ends`, format="%m/%d/%y")),
             newSTV = 130, geomean = 35)
    windowData$`Date Time` <- as.POSIXct(strptime(windowData$`Date Time`, format="%Y-%m-%d"))#as.POSIXct(windowData$`Date Time`, format="%Y-%m-%d", tz='GMT') + as.difftime(1, units="days")
    
    plot_ly(data=windowData) %>%
      add_markers(x= ~`Date Time`, y= ~Value,mode = 'scatter', name="Enterococci (CFU / 100 mL)", marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",`Date Time`),
                                               paste("Enterococci: ",Value,"CFU / 100 mL"))) %>%
      add_lines(data=windowData, x=~`Date Time`, y=~ENTEROCOCCI_geomean, mode='line', line = list(color = 'orange', dash= 'dash'),
                hoverinfo = "text", text= ~paste("Window Geomean: ", format(ENTEROCOCCI_geomean,digits=3)," CFU / 100 mL", sep=''), 
                name="Window Geomean") %>%
      add_lines(data=windowData, x=~`Date Time`,y=~newSTV, mode='line', line = list(color = '#484a4c',dash = 'dot'),
                hoverinfo = "text", text= "New STV: 130 CFU / 100 mL", name="New STV: 130 CFU / 100 mL") %>%
      add_lines(data=windowData, x=~`Date Time`,y=~geomean, mode='line', line = list(color = 'black', dash= 'dash'),
                hoverinfo = "text", text= "Geomean: 35 CFU / 100 mL", name="Geomean: 35 CFU / 100 mL") %>%
      layout(showlegend=FALSE,
             yaxis=list(title="Enterococci (CFU / 100 mL)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) 
  })
  
  output$rawData <- DT::renderDataTable({
    req(input$windowChoice_, Entero_oneStation(),newSTDbacteriaData())
    z <- dplyr::select(Entero_oneStation(), FDT_STA_ID, FDT_DATE_TIME, ENTEROCOCCI ,RMK_31649)
    DT::datatable(z, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "400px", dom='t'))
  })
  
  output$analysisTable <- DT::renderDataTable({
    req(Entero_oneStation(),newSTDbacteriaData())
    z <- bacteriaExceedances_NewStd(newSTDbacteriaData(), 10, 130, 35) %>%
      dplyr::select(-associatedData) # remove embedded tibble to make table work
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "400px", dom='t'))
  })
  
}


ui <- fluidPage(
  helpText('Review each site using the single site visualization section, then 
           proceed to the bottom of the page to find exceedance rate for the entire assessment unit.',br(),
           span(strong('NOTE: The pH exceedance analysis results at the bottom of the page include data
                       from ALL stations within the assessment unit.'))),
  EnteroPlotlySingleStationUI('Entero')    )

server <- function(input,output,session){
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  AUData <- reactive({filter(conventionals_HUC, FDT_STA_ID %in% c('2-JKS018.68','4APKP-4-DRBA' )) %>%
      mutate( ENTEROCOCCI = `E._COLI_31648_NO/100mL`, RMK_31649 = RMK_31648) })
  
  callModule(EnteroPlotlySingleStation,'Entero', AUData, stationSelected)#input$stationSelection)
  
  
}

shinyApp(ui,server)
