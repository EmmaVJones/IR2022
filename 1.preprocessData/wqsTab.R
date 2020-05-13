

ui <- shinyUI(fluidPage(theme= "yeti.css",
                        navbarPage("Regional Assessment Metadata Validation",
                                   navbarMenu("Water Quality Standards QA",
                                              tabPanel("Watershed Selection",
                                                       sidebarPanel(
                                                         fluidRow(column(5, selectInput('WQSwaterbodyType','Waterbody Type', choices = unique(WQSlayerConversion$waterbodyType))),
                                                                  column(7, actionButton('WQSstart',HTML("Begin Review With <br/>Waterbody Selection <br/>(Clears Cached Results)"),
                                                                                         class='btn-block'))),
                                                         hr(),
                                                       conditionalPanel(condition = ("input.WQSstart"), # dynamicSelectInput did not work with progress bar for spatial file
                                                                        uiOutput('WQSDEQregionSelection_'),
                                                                        uiOutput('WQSsubbasinSelection_'),
                                                                        br(),
                                                                        uiOutput('WQSbegin_'))),
                                                       
                                                       mainPanel(
                                                         #verbatimTextOutput('test'),
                                                         leafletOutput('WQSVAmap'),
                                                         h5(strong('Preprocessing Data Recap for Selected Region/Subbasin/Type Combination')),
                                                         fluidRow(column(3, textOutput('singleSnapSummary1WQS')),
                                                                  column(3, textOutput('snapTooManySummary1WQS')),
                                                                  column(3, textOutput('noSnapSummary1WQS')),
                                                                  column(3, textOutput('regionalSitesSummary1WQS'))),
                                                         br())
                                              )))))

server <- shinyServer(function(input, output, session) {
  # empty reactive objects list
  WQSreactive_objects = reactiveValues() # for WQS
  
  ## Watershed Selection Tab WQS
  
  # Bring in WQS layer statewide
  WQSstatewide <- eventReactive(input$WQSstart, {
    typeName <- case_when(input$WQSwaterbodyType == 'Lacustrine' ~ 'lakes_reservoirs',
                          input$WQSwaterbodyType == 'Estuarine' ~ 'estuarinepolygons',
                          TRUE ~ as.character(input$WQSwaterbodyType))
    #withProgress(message = 'Reading in Large Spatial File',
     #            st_read('GIS/WQS_layers_05082020.gdb', layer = paste0(tolower(typeName),'_05082020') , fid_column_name = "OBJECTID") %>%
    #               st_transform(4326) )})
    withProgress(test) }) # riverine test
  
  # Update map Subbasin based on user selection
  output$WQSDEQregionSelection_ <- renderUI({
    req(WQSstatewide())
    op <- filter(basinAssessmentRegion, BASIN %in% unique(WQSstatewide()$BASIN)) %>%
      distinct(ASSESS_REG) %>% 
      pull()
    selectInput("WQSDEQregionSelection", "Select DEQ Assessment Region", multiple = FALSE,
                choices= op)  })
  
  output$WQSsubbasinSelection_ <- renderUI({
    req(WQSstatewide(), input$WQSDEQregionSelection)
    op <- filter(basinAssessmentRegion, BASIN %in% unique(WQSstatewide()$BASIN)) %>%
      filter(ASSESS_REG %in% input$WQSDEQregionSelection) %>%
      distinct(Basin_Code) %>% 
      pull()
    selectInput("WQSsubbasinSelection", "Select Subbasin", multiple = FALSE,
                choices= op)  })
  output$WQSbegin_ <- renderUI({
    req(WQSstatewide(), input$WQSDEQregionSelection, input$WQSsubbasinSelection)
    actionButton('WQSbegin', HTML("Begin Review With Subbasin Selection <br/>(Clears Cached Results)"),
                 class='btn-block')  })
    
  basinCodes <- reactive({
    req(WQSstatewide(), input$WQSDEQregionSelection, input$WQSsubbasinSelection)
    filter(basinAssessmentRegion, BASIN %in% unique(WQSstatewide()$BASIN)) %>%
      filter(ASSESS_REG %in% input$WQSDEQregionSelection) %>%
      filter(Basin_Code %in% input$WQSsubbasinSelection) %>%
      distinct(BASIN_CODE) %>% 
      pull() })

  WQSs <- eventReactive(input$WQSbegin, {
    req(WQSstatewide(), input$WQSDEQregionSelection, input$WQSsubbasinSelection)
    WQSstatewide() %>%
      # limit to just selected filters
      filter(BASIN %in% as.character(basinCodes()))      })
  
  ## Map output of selected subbasin
  output$WQSVAmap <- renderLeaflet({
    req(input$WQSbegin, WQSstatewide(), input$WQSDEQregionSelection, input$WQSsubbasinSelection)
    subbasins <- filter(basin7, BASIN_CODE %in% as.character(basinCodes()))
    
    m <- mapview( subbasins, label= subbasins$SUBBASIN, layer.name = 'Selected Subbasin',
                 popup= leafpop::popupTable(subbasins, zcol=c('BASIN_NAME', 'BASIN_CODE', 'SUBBASIN')))
    m@map %>% setView(st_bbox(subbasins)$xmax[[1]],st_bbox(subbasins)$ymax[[1]],zoom = 7)  })
  
  
  ### WQS reactive 
  observeEvent(input$WQSbegin, {
    # All sites limited to waterbody type and subbasin
    WQSreactive_objects$snap_input <- readRDS('data/processedWQS/WQStable.RDS') %>%
      filter(str_extract(WQS_ID, "^.{2}") %in% filter(WQSlayerConversion, waterbodyType %in% input$WQSwaterbodyType)$WQS_ID) %>%
      filter(gsub("_","",str_extract(WQS_ID, ".{3}_")) %in% 
               str_pad(unique(filter(basinAssessmentRegion, BASIN_CODE %in% basinCodes())$BASIN_CODE), 
                       width = 2, side = 'left', pad = '0')) %>%
      group_by(StationID) %>%
      mutate(n = n()) %>% ungroup()
    # Sites limited to just region of interest
    WQSreactive_objects$snap_input_Region <- WQSreactive_objects$snap_input %>%
      left_join(conventionals_D, by = 'StationID') %>%
      st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
               remove = T, # don't remove these lat/lon cols from df
               crs = 4326) %>%
      st_intersection(filter(assessmentRegions, ASSESS_REG %in% input$WQSDEQregionSelection)) %>%
      st_drop_geometry() %>% # back to tibble
      rename('Buffer Distance' = 'Buffer.Distance') %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`, n)
    # Make dataset of all sites for highlighting purposes, preliminary list
    WQSreactive_objects$sitesUnique <- WQSreactive_objects$snap_input %>%
      left_join(conventionals_D, by = 'StationID') %>%
      st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
               remove = T, # don't remove these lat/lon cols from df
               crs = 4326)
    # Make dataset of all WQS_IDs available for table purposes, this will hold corrected WQS_ID information after user review
    WQSreactive_objects$WQS_IDs <- WQSreactive_objects$snap_input 
    # Make dataset of multiple segments snapped to single site IN REGION
    WQSreactive_objects$tooMany <- filter(WQSreactive_objects$snap_input_Region, n > 1) %>%
      group_by(StationID) %>% mutate(colorFac = row_number()) %>% ungroup()
    # Make dataset of sites associated with too many segments IN REGION
    WQSreactive_objects$tooMany_sites <- filter(WQSreactive_objects$sitesUnique, StationID %in% WQSreactive_objects$tooMany$StationID) %>%
      left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID') %>%
      distinct(StationID, .keep_all = T) %>%
      dplyr::select(-c(WQS_ID, `Buffer Distance`, n))
    # Make dataset of sites that snapped to a single WQS and join WQS info  IN REGION
    WQSreactive_objects$snapSingle <- filter(WQSreactive_objects$sitesUnique, n == 1) %>%
      filter(StationID %in% WQSreactive_objects$snap_input_Region$StationID) %>% # limit assignment to just what falls in a region
      left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID')
    # Make dataset of sites associated with no segments IN REGION
    WQSreactive_objects$snapNone <- filter(WQSreactive_objects$sitesUnique, is.na(WQS_ID)) %>%
      filter(StationID %in% WQSreactive_objects$snap_input_Region$StationID) %>% # limit assignment to just what falls in a region
      left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID')
    # Make empty dataset of sites that assessors touched
    WQSreactive_objects$sitesAdjusted <-  WQSreactive_objects$sitesUnique[0,]
    # Make dataset for user to download
    WQSreactive_objects$finalWQS <- WQSreactive_objects$sitesUnique %>% st_drop_geometry()
  })
  
  # UI summaries of data pulled in to app, first and second tab
  output$singleSnapSummary1WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$snapSingle), ' stations that snapped to 1 AU segment in preprocessing.'))})
  output$singleSnapSummary2WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$snapSingle), ' stations that snapped to 1 AU segment in preprocessing.'))})
  output$snapTooManySummary1WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$tooMany_sites), ' stations that snapped to > 1 AU segment in preprocessing.'))})
  output$snapTooManySummary2WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$tooMany_sites), ' stations that snapped to > 1 AU segment in preprocessing.'))})
  output$noSnapSummary1WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$snapNone), ' stations that snapped to 0 AU segments in preprocessing.'))})
  output$noSnapSummary2WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$snapNone), ' stations that snapped to 0 AU segments in preprocessing.'))})
#  output$regionalSitesSummary1WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
#    cat(paste0('There are ', nrow(WQSreactive_objects$snap_input[['inputSites']]), ' stations in the selected Region/Basin.'))})
#  output$regionalSitesSummary2WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
#    cat(paste0('There are ', nrow(WQSreactive_objects$snap_input[['inputSites']]), ' stations in the selected Region/Basin.'))})
# not sure I'm going to do that for WQS  
  
  
  output$test <- renderPrint({
    print(WQSs())
  })
  

  
})

shinyApp(ui, server)

