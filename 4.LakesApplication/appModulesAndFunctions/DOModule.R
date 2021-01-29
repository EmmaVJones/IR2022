DOPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(4,uiOutput(ns('oneStationSelectionUI'))),
               column(3,actionButton(ns('zoomPlotDO'),"Zoomed Plot by Sample Date",class='btn-block')),
               column(4,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      helpText('All data presented in the interactive plot is raw data. Rounding rules are appropriately applied to the 
               assessment functions utilized by the application.'),
      plotlyOutput(ns('plotly')),
      br(),hr(),br(),
      fluidRow(
        column(7, h5('All DO records that are outside the criteria for the ',span(strong('selected site')),' are highlighted below.'),
               dataTableOutput(ns("minTableSingleSite"))),
        column(1),
        column(4, 
               h5('Individual DO exceedance statistics for the ',span(strong('selected site')),' are highlighted below.
                  These are the results reflected in the stations table above.'),
               dataTableOutput(ns("stationExceedanceRate")),
               br(),
               helpText('Analyzing the exceedance rate of just epilimnion samples can assist in determining if lake turnover 
                        may be the contributing to DO exceedances.'),
               h5('Individual DO exceedance statistics for the ',span(strong('selected site in the epilimnion')),
                  ' are highlighted below.'),
               dataTableOutput(ns("EPIstationExceedanceRate")))),
      helpText("The 2022 IR Guidance states: In cases where the applicable nutrient criteria are met for the man-made 
      lakes/reservoirs listed in Section 187 but there are seasonal exceedances of the dissolved oxygen criterion due to 
      Fall overturn that result in the lake/reservoir assessment unit to be listed as impaired, a Category 4C rationale may 
      be developed for EPA review. The rationale should include:"),
      helpText("    •	maps of the lake/reservoir assessment unit and surrounding land-use to evaluate any potential sources 
      in the watershed, and"),
      helpText("    •	dissolved oxygen/temperature data profiles from at least two monitoring years in line with the Fall 
      overturn phenomena. "),
      helpText("In cases where the applicable nutrient criteria are not met, but there are seasonal exceedances of the 
      dissolved oxygen criterion due to Fall overturn that result in the lake/reservoir assessment unit to be listed as 
      impaired, the lake or reservoir should be classified as Category 5C and recommended for a WQS review due to seasonal
      DO fluctuations."),
      br(),
      wellPanel(
        h4(strong('AU Assessment')),
        helpText("The 2022 IR Guidance states: 'In most cases, a single monitoring station should represent a lake/reservoir assessment unit. 
                 In cases where there are multiple stations in an assessment unit and it is determined that the water in that unit is
                 homogenous and not influenced by tributary contribution, the data may be pooled to determine DO exceedance rates in that AU'"),
        helpText(strong('This application provides an AU level assessment, but it is up to the assessor to determine if pooling should be used.')),
        fluidRow(
          column(7, h5('All DO records that are above the criteria for the ',span(strong('Assessment Unit')),' are highlighted below.'),
                 dataTableOutput(ns('rangeTableAU'))),
          column(1),
          column(4, h5('DO exceedance statistics for the ',span(strong('AU')),' are highlighted below.'),
                 dataTableOutput(ns("AUExceedanceRate")),
                 br(),
                 helpText('Analyzing the exceedance rate of just epilimnion samples can assist in determining if lake turnover 
                        may be the contributing to DO exceedances.'),
                 h5('DO exceedance statistics for the ',span(strong('AU in the epilimnion')),' are highlighted below.'),
                 dataTableOutput(ns("EPIAUExceedanceRate")))) )
    )
  )
}

DOPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='200px', selected = stationSelectedAbove())})
  
  oneStation <- reactive({req(ns(input$oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$oneStationSelection) %>%
      filter(!is.na(DO_mg_L))})
  
  
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
  output$parameterData <- DT::renderDataTable({  req(oneStation())
    parameterFilter <- dplyr::select(oneStation(), FDT_STA_ID:FDT_COMMENT, DO_mg_L, RMK_DO, LEVEL_DO, LakeStratification)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t'),
                  selection = 'none') %>%
      formatStyle(c('DO_mg_L','RMK_DO', 'LEVEL_DO'), 'LEVEL_DO',backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray')) })
  
  output$plotly <- renderPlotly({
    req(input$oneStationSelection, oneStation())
    dat <- mutate(oneStation(), bottom = `Dissolved Oxygen Min (mg/L)`,
                  LakeStratification = replace_na(LakeStratification,"NA")) %>%
      mutate(LakeStratification = factor(LakeStratification,levels=c("Epilimnion",'NA',"Hypolimnion")))#,ordered=T)
    # Fix look of single measure
    if(nrow(dat) == 1){
      print('yes')
      dat <- bind_rows(dat,
                       tibble(SampleDate = c(dat$SampleDate- days(5), dat$SampleDate + days(5))))    }
    
    plot_ly(data=dat)%>%
      add_lines(data=dat, x=~SampleDate,y=~bottom, mode='line', line = list(color = 'black'),
                hoverinfo = "text", text="DO Standard", name="DO Standard") %>%
      add_markers(data=dat, x= ~SampleDate, y= ~DO_mg_L,mode = 'scatter', name="DO (mg/L)", 
                  color=~LakeStratification,#marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("DO: ",DO_mg_L," (mg/L)"),
                                               paste("LakeStratification: ",LakeStratification)))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="DO (mg/L)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))   })
  
  
  #### DO MODAL ----------------------------------------------------------------------------------------------------------
  observeEvent(input$zoomPlotDO,{
    showModal(modalDialog(
      title="Zoomed Plot for Single Sample Date",
      uiOutput(ns('stationDateSelectionDO')),
      hr(),
      plotlyOutput(ns('DOplotlyByDate')),
      easyClose = TRUE))  })
  
  # DO date option dropdown, inside modal
  output$stationDateSelectionDO <- renderUI({    req(oneStation())
    selectInput(ns('DOdateSelection'),"Choose a Sample Date to Plot",choices = unique(oneStation()$SampleDate))  })
  
  oneSampleDate <- reactive({ req(input$DOdateSelection, oneStation())
    filter(oneStation(),SampleDate %in% as.Date(input$DOdateSelection)) %>%
      mutate(bottom = `Dissolved Oxygen Min (mg/L)`,
             LakeStratification = replace_na(LakeStratification,"NA")) %>%
      mutate(LakeStratification = factor(LakeStratification,levels=c("Epilimnion",'NA',"Hypolimnion"))) })#,ordered=T)  
  
  # Plotly DO by single sample date
  output$DOplotlyByDate <- renderPlotly({  req(oneSampleDate())
    dat <- oneSampleDate()
    
    plot_ly(data=dat)%>%
      add_lines(x=~FDT_TEMP_CELCIUS,y=~`Dissolved Oxygen Min (mg/L)`, mode='line',line = list(color = '#E50606'),
                hoverinfo = "text",text= paste("DO Standard:",unique(dat$`Dissolved Oxygen Min (mg/L)`),' (mg/L)',sep=''), name = 'DO Standard') %>%
      add_markers(x= ~FDT_TEMP_CELCIUS, y= ~DO_mg_L,mode = 'scatter', name="Dissolved Oxygen",
                  color=~LakeStratification, #colors=c('#BF382A', '#0C4B8E'),
                  hoverinfo="text",text=~paste(sep="<br>",paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Dissolved Oxygen:",DO_mg_L,"mg/L"),
                                               paste("LakeStratification: ",LakeStratification)))%>%
      layout(xaxis = list(autorange = "reversed"),
             showlegend=FALSE,
             yaxis=list(title="Dissolved Oxygen (mg/L)"),
             xaxis=list(title="Temperature (C)",
                        tickfont = list(size = 10)))
  })
  #### END MODAL -------------------------------------------------------------------------------------------
  
  output$minTableSingleSite <- renderDataTable({req(oneStation())
    z <- DOExceedances_Min(oneStation()) %>%
      rename("DO" = 'parameter', 'Criteria' = 'limit', 'Parameter Rounded to WQS Format' = 'parameterRound') %>%
      filter(exceeds == TRUE) %>%
      dplyr::select(-c(exceeds, FDT_STA_ID)) %>%
      dplyr::select(FDT_DATE_TIME, FDT_DEPTH, DO, LEVEL_DO, LakeStratification, everything()) %>%
      arrange(FDT_DATE_TIME, FDT_DEPTH)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "200px", dom='t'),
              selection = 'none')})
  
  output$stationExceedanceRate <- renderDataTable({req(input$oneStationSelection, oneStation())
    z <- DOExceedances_Min(oneStation()) %>% quickStats('DO') %>% dplyr::select(-DO_STAT)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "60px", dom='t'),
              selection = 'none') })
  
  output$EPIstationExceedanceRate <- renderDataTable({req(input$oneStationSelection, oneStation())
    z <- filter(oneStation(), LakeStratification %in% c("Epilimnion")) %>%
      dplyr::select(FDT_DATE_TIME, FDT_DEPTH, DO_mg_L, LEVEL_DO, `Dissolved Oxygen Min (mg/L)`)%>% # Just get relevant columns, 
      filter(!(LEVEL_DO %in% c('Level II', 'Level I'))) %>% # get lower levels out
      filter(!is.na(DO_mg_L)) %>% 
      rename(parameter = !!names(.[3]), limit = !!names(.[5])) %>% # rename columns to make functions easier to apply
      # Round to Even Rule
      mutate(parameterRound = signif(parameter, digits = 2), # two significant figures based on  https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section50/
             exceeds = ifelse(parameterRound < limit, T, F)) %>% # Identify where below min DO 
      quickStats('DO') %>% 
      dplyr::select(-DO_STAT)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "60px", dom='t'),
              selection = 'none') })
  
  
  ## AU results 
  output$rangeTableAU <-  renderDataTable({    req(oneStation())
    z <- DOExceedances_Min( filter(AUdata(), !is.na(DO_mg_L)) ) %>%
      rename("DO" = 'parameter', 'Criteria' = 'limit', 'Parameter Rounded to WQS Format' = 'parameterRound') %>%
      filter(exceeds == TRUE) %>%
      dplyr::select(-exceeds) %>%
      dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, DO, LEVEL_DO, LakeStratification, everything()) %>%
      arrange(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "150px", dom='t'),
              selection = 'none')})
  
  output$AUExceedanceRate <- renderDataTable({  req(ns(input$oneStationSelection), oneStation())
    z <- DOExceedances_Min( filter(AUdata(), !is.na(DO_mg_L)) ) %>% quickStats('DO') %>% dplyr::select(-DO_STAT)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "60px", dom='t'),
              selection = 'none') })
  
  output$EPIAUExceedanceRate <- renderDataTable({req(ns(input$oneStationSelection), oneStation())
    z <- filter(AUdata(), LakeStratification %in% c("Epilimnion")) %>%
      dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, DO_mg_L, LEVEL_DO, `Dissolved Oxygen Min (mg/L)`)%>% # Just get relevant columns, 
      filter(!(LEVEL_DO %in% c('Level II', 'Level I'))) %>% # get lower levels out
      filter(!is.na(DO_mg_L)) %>% 
      rename(parameter = !!names(.[4]), limit = !!names(.[6])) %>% # rename columns to make functions easier to apply
      # Round to Even Rule
      mutate(parameterRound = signif(parameter, digits = 2), # two significant figures based on  https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section50/
             exceeds = ifelse(parameterRound < limit, T, F)) %>% # Identify where below min DO 
      quickStats('DO') %>% 
      dplyr::select(-DO_STAT)
    
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "60px", dom='t'),
              selection = 'none') })
  
  
}
