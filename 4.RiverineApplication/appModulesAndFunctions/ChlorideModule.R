

ClPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(2,uiOutput(ns('oneStationSelectionUI'))),
               column(1),
               column(2,br(),checkboxInput(ns('displayBSAcolors'), 'Display Benthic Stressor Analysis Colors on Plot', value = TRUE)),
               column(1),
               column(2,br(), uiOutput(ns('changeWQSUI'))),
               column(1),
               column(2,br(),actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      helpText('All data presented in the interactive plot is raw data. Rounding rules are appropriately applied to the
               assessment functions utilized by the application.'),
      plotlyOutput(ns('plotly')),
      fluidRow(
        column(8, h5('All chloride records that are above the PWS criteria (where applicable) for the ',span(strong('selected site')),' are highlighted below.'),
               div(style = 'height:150px;overflow-y: scroll', dataTableOutput(ns('rangeTableSingleSite')))),
        column(4, h5('Individual chloride exceedance statistics for the ',span(strong('selected site')),' are highlighted below.
                     If no data is presented, then the PWS criteria is not applicable to the station.'),
               dataTableOutput(ns("stationExceedanceRate")))),
      br(),hr(),br(),
      h4('Freshwater Chloride Criteria'),
      helpText('Below are the results of the chloride freshwater acute and chronic criteria analysis. These results apply to all stations with
               CLASS II (Tidal Fresh Zone only) and III - VII. The acute and chronic criteria for each data window are presented on the plot below. 
               Turn the layers on/off to visualize the raw data averaged across each window.'),
      plotlyOutput(ns('freshwaterPlotly')),
      br(),
      fluidRow(
        column(8, h5('All Chloride data analyzed using acute and chronic criteria for the ',span(strong('selected site')),' are presented below. 
                     Any exceedances are highlighted in red.'),
               dataTableOutput(ns("stationFreshwaterAnalysis"))),
        column(4, h5('Individual Chloride exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
               helpText('The one hour and four day average concentration are not to be exceeded more than once every 3 years on the average, 
                        unless otherwise noted. This tool does not make that determination, but a summary of the total exceedances for each 
                        criteria are presented below.'),
               DT::dataTableOutput(ns('stationFreshwaterExceedanceRate'))))
    )
  )
}


ClPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  output$oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='200px', selected = stationSelectedAbove())})
  
  oneStation <- reactive({
    req(ns(input$oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$oneStationSelection) %>%
      filter(!is.na(CHLORIDE_mg_L)) %>%
      mutate(`Parameter Rounded to WQS Format` = round(CHLORIDE_mg_L, digits = 0),
             PWSlimit = 250)})
  
  # Option to change WQS used for modal
  output$changeWQSUI <- renderUI({
    req(oneStation())
    if(nrow(oneStation()) > 0){
      defaultPWS <- unique(oneStation()$PWS) %in% c("Yes")
    } else { defaultPWS <- FALSE}
    checkboxInput(ns('changeWQS'),'Apply Public Water Supply Water Quality Standards (Automatically selected if PWS standards apply to the selected station)', value = defaultPWS) })
  
  
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
  output$parameterData <- DT::renderDataTable({    req(oneStation())
    parameterFilter <- dplyr::select(oneStation(), FDT_STA_ID:FDT_COMMENT, CHLORIDE_mg_L, RMK_CHLORIDE, LEVEL_CHLORIDE)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t'),
                  selection = 'none') %>%
      formatStyle(c('CHLORIDE_mg_L','RMK_CHLORIDE', 'LEVEL_CHLORIDE'), 'LEVEL_CHLORIDE', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  output$plotly <- renderPlotly({
    req(input$oneStationSelection, oneStation())
    dat <- oneStation() 
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y")
    
    # Fix look of single measure
    if(nrow(dat) == 1){
      dat <- bind_rows(dat,
                       tibble(SampleDate = c(dat$SampleDate- days(5), dat$SampleDate + days(5)),
                              PWSlimit = c(250, 250)))
    }
    
    maxheight <- ifelse(max(dat$CHLORIDE_mg_L, na.rm=T) < 50, 55, max(dat$CHLORIDE_mg_L, na.rm=T)* 1.2)
    
    if(input$displayBSAcolors == TRUE){
      box1 <- data.frame(SampleDate = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(50, maxheight, maxheight, 50))
      box2 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(25, 50, 50, 25))
      box3 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(10, 25, 25, 10))
      box4 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0, 10, 10, 0))
      
      if(input$changeWQS == TRUE){
        plot_ly(data=dat)%>%
          add_polygons(x = ~SampleDate, y = ~y, data = box1, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
                       hoverinfo="text", name =paste('High Probability of Stress to Aquatic Life')) %>%
          add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                       hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
          add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                       hoverinfo="text", name =paste('Low Probability of Stress to Aquatic Life')) %>%
          add_polygons(data = box4, x = ~x, y = ~y, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
                       hoverinfo="text", name =paste('No Probability of Stress to Aquatic Life')) %>%
          add_lines(data=dat, x=~SampleDate,y=~PWSlimit, mode='line', line = list(color = 'black'),
                    hoverinfo = "text", text= "PWS Criteria (250 mg/L)", name="PWS Criteria (250 mg/L)") %>%
          add_markers(data=dat, x= ~SampleDate, y= ~CHLORIDE_mg_L,mode = 'scatter', name="Dissolved Chloride (mg/L)",marker = list(color= '#535559'),
                      hoverinfo="text",text=~paste(sep="<br>",
                                                   paste("Date: ",SampleDate),
                                                   paste("Depth: ",FDT_DEPTH, "m"),
                                                   paste("Dissolved Chloride: ",CHLORIDE_mg_L,"mg/L")))%>%
          layout(showlegend=FALSE,
                 yaxis=list(title="Dissolved Chloride (mg/L)"),
                 xaxis=list(title="Sample Date",tickfont = list(size = 10)))
      } else {
        plot_ly(data=dat)%>%
          add_polygons(x = ~SampleDate, y = ~y, data = box1, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
                       hoverinfo="text", name =paste('High Probability of Stress to Aquatic Life')) %>%
          add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                       hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
          add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                       hoverinfo="text", name =paste('Low Probability of Stress to Aquatic Life')) %>%
          add_polygons(data = box4, x = ~x, y = ~y, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
                       hoverinfo="text", name =paste('No Probability of Stress to Aquatic Life')) %>%
          add_markers(data=dat, x= ~SampleDate, y= ~CHLORIDE_mg_L, mode = 'scatter', name="Dissolved Chloride (mg/L)",marker = list(color= '#535559'),
                      hoverinfo="text",text=~paste(sep="<br>",
                                                   paste("Date: ",SampleDate),
                                                   paste("Depth: ",FDT_DEPTH, "m"),
                                                   paste("Dissolved Chloride: ",CHLORIDE_mg_L,"mg/L")))%>%
          layout(showlegend=FALSE,
                 yaxis=list(title="Dissolved Chloride (mg/L)"),
                 xaxis=list(title="Sample Date",tickfont = list(size = 10)))
        
      }
    } else {
      if(input$changeWQS == TRUE){
        plot_ly(data=dat)%>%
          add_lines(data=dat, x=~SampleDate,y=~PWSlimit, mode='line', line = list(color = 'black'),
                    hoverinfo = "text", text= "PWS Criteria (250 mg/L)", name="PWS Criteria (250 mg/L)") %>%
          add_markers(data=dat, x= ~SampleDate, y= ~CHLORIDE_mg_L,mode = 'scatter', name="Dissolved Chloride (mg/L)",marker = list(color= '#535559'),
                      hoverinfo="text",text=~paste(sep="<br>",
                                                   paste("Date: ",SampleDate),
                                                   paste("Depth: ",FDT_DEPTH, "m"),
                                                   paste("Dissolved Chloride: ",CHLORIDE_mg_L,"mg/L")))%>%
          layout(showlegend=FALSE,
                 yaxis=list(title="Dissolved Chloride (mg/L)"),
                 xaxis=list(title="Sample Date",tickfont = list(size = 10)))
      } else {
        plot_ly(data=dat)%>%
          add_markers(data=dat, x= ~SampleDate, y= ~CHLORIDE_mg_L,mode = 'scatter', name="Dissolved Chloride (mg/L)",marker = list(color= '#535559'),
                      hoverinfo="text",text=~paste(sep="<br>",
                                                   paste("Date: ",SampleDate),
                                                   paste("Depth: ",FDT_DEPTH, "m"),
                                                   paste("Dissolved Chloride: ",CHLORIDE_mg_L,"mg/L")))%>%
          layout(showlegend=FALSE,
                 yaxis=list(title="Dissolved Chloride (mg/L)"),
                 xaxis=list(title="Sample Date",tickfont = list(size = 10)))
        
      } }
  })
  
  
  output$rangeTableSingleSite <- renderDataTable({    req(oneStation())
    if(input$changeWQS == TRUE){
      z <- filter(oneStation(), `Parameter Rounded to WQS Format` > PWSlimit) %>%
        dplyr::select(FDT_DATE_TIME, CHLORIDE_mg_L, LEVEL_CHLORIDE, Criteria = PWSlimit, `Parameter Rounded to WQS Format`) 
    } else { z <- NULL}
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "150px", dom='t'),
              selection = 'none') })
  
  
  output$stationExceedanceRate <- renderDataTable({
    req(input$oneStationSelection, oneStation())
    if(input$changeWQS == TRUE){
      chloride <- dplyr::select(oneStation(), FDT_DATE_TIME, FDT_DEPTH, CHLORIDE_mg_L, LEVEL_CHLORIDE) %>%
        filter(!(LEVEL_CHLORIDE %in% c('Level II', 'Level I'))) %>% # get lower levels out
        filter(!is.na(CHLORIDE_mg_L)) %>% #get rid of NA's
        mutate(`Parameter Rounded to WQS Format` = round(CHLORIDE_mg_L, digits = 0),  # round to WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
               limit = 250) %>%
        rename(parameter = !!names(.[5])) %>% # rename columns to make functions easier to apply
        mutate(exceeds = ifelse(parameter > limit, T, F)) # Identify where above NH3 WQS limit
      z <- quickStats(chloride, 'PWS_Chloride') %>% dplyr::select(-PWS_Chloride_STAT) 
      datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "150px", dom='t'),
                selection = 'none') }}) 
  
  
  
  # Freshwater Chloride Analysis
  chlorideFreshwater <- reactive({req(nrow(oneStation()) > 0)
    chlorideFreshwaterAnalysis(oneStation())    })
  
  
  output$freshwaterPlotly <- renderPlotly({req(chlorideFreshwater(), nrow(oneStation()) > 0)
    stationData <- oneStation()
    stationData$SampleDate <- as.POSIXct(stationData$FDT_DATE_TIME, format="%m/%d/%y")
    
    plot_ly(data=stationData)%>%
      add_markers(data=stationData, x= ~SampleDate, y= ~CHLORIDE_mg_L, mode = 'scatter', name="Dissolved Chloride (mg/L)",marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Dissolved Chloride: ",CHLORIDE_mg_L,"mg/L")))%>%
      add_markers(data=chlorideFreshwater(), x= ~WindowDateTimeStart, y= ~Value, mode = 'scatter', name= ~paste0(`ValueType`, " Averaged Dissolved Chloride (mg/L)"),
                  marker = list(color= ~Exceedance), colors = c('#535559', 'red'),#marker = list(color= '#535559'),
                  symbol = ~`Criteria Type`, #symbols = c('diamond-dot','diamond'),# symbols = c('x','o'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Window Start: ",WindowDateTimeStart),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste0(`ValueType`, " Averaged Dissolved Chloride: ",Value,"mg/L"),
                                               paste0(`Criteria Type`," Dissolved Chloride Criteria: ",CriteriaValue,"mg/L")))%>%
      layout(showlegend=TRUE,
             yaxis=list(title="Dissolved Chloride (mg/L)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))
  })
  
  output$stationFreshwaterAnalysis <- renderDataTable({req(chlorideFreshwater())
    datatable(chlorideFreshwater(), rownames = FALSE, options= list(pageLength = nrow(chlorideFreshwater()), scrollX = TRUE, scrollY = "350px", dom='t'),
              selection = 'none') %>% 
      formatStyle('Exceedance', target = 'row', backgroundColor = styleEqual(c(0, 1), c(NA, 'red')))      })
  
  output$stationFreshwaterExceedanceRate <- renderDataTable({req(chlorideFreshwater())
    z <- filter(chlorideFreshwater(), Exceedance == 1) %>% 
      group_by(`Criteria Type`) %>% 
      summarise(`Total Exceedances in Dataset` = sum(Exceedance, na.rm = T))
    if(nrow(z) == 0){ # show them something just in case
      z <- tibble(`Criteria Type` = c('Chronic', 'Acute'), `Total Exceedances in Dataset` = c(0, 0))
    }
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "150px", dom='t'),
              selection = 'none')     })
  
  #output$test <- renderPrint({chlorideFreshwater()})
  
}

