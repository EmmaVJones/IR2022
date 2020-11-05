source('global.R')

#options(shiny.maxRequestSize=100*1024^2) 

# Pinned to server, do for each region
#regionalAUs <- st_read('userDataToUpload/AU_working/va_2020_aus_riverine_DRAFT_BRRO.shp') %>%
#  st_transform(4326)   # transform to WQS84 for spatial intersection 
#pin(regionalAUs, name = 'BRROworkingAUriverine', description = "BRRO working AU riverine", board = "rsconnect")


# Pull data from server
#conventionals <- pin_get("conventionals2022IRdraft", board = "rsconnect")
#vahu6 <- st_as_sf(pin_get("vahu6", board = "rsconnect")) # bring in as sf object


shinyServer(function(input, output, session) {
  
  # display the loading feature until data loads into app
  load_data()
  
  ## Data Upload Tab
  
  stationTable <- reactive({read_csv('userDataToUpload/processedStationData/stationsTable2022begin.csv')}) #for testing
    #req(input$stationsTable)
    #inFile <- input$stationsTable
    #read_csv(inFile$datapath) %>%
    #  #fix periods in column names from excel
    #  as_tibble() })
  
#  regionalAUs <- reactive({ #req(input$regionalAUshapefile)

# issues getting data from fileInput, skipping for now and thinking about pulling from server    
    #readOGR(input$regionalAUshapefile$datapath)
    #st_read(input$regionalAUshapefile$datapath)})
# Maybe make assessors give me stations to update on server this round??

  
  ## Watershed Selection Tab
  
  output$DEQregionSelectionUI <- renderUI({req(vahu6)
    selectInput("DEQregionSelection", "Select DEQ Assessment Region", choices = unique(vahu6$ASSESS_REG))})
  
  # Pull AU data from server
  regionalAUs <- reactive({ 
    req(input$pullAUs)
    withProgress(message = 'Reading in Large Spatial File',
                 #st_zm(st_as_sf(pin_get(paste0(input$DEQregionSelection, 'workingAUriverine'), board = 'rsconnect')) )) })
                 regionalAUsForTesting ) })
  
  # Query VAHUC6's By Selectize arguments
  the_data <- reactive({
    req(regionalAUs(), input$DEQregionSelection)
    filter(vahu6, ASSESS_REG %in% input$DEQregionSelection) %>%
      left_join(dplyr::select(subbasinToVAHU6, VAHU6, Basin, BASIN_CODE, Basin_Code))})
  basin_filter <- shiny::callModule(dynamicSelect, "basinSelection", the_data, "Basin_Code" )
  huc6_filter <- shiny::callModule(dynamicSelect, "HUC6Selection", basin_filter, "VAHU6" )
  
  # Spatially intersect chosen VAHU6 with regionalAUs
  AUs <- reactive({req(huc6_filter(), regionalAUs())
    suppressWarnings(st_intersection(regionalAUs(),  huc6_filter())) }) #filter(vahu6, VAHU6 %in% huc6_filter()$VAHU6)))})
  

  # Watershed Map
  output$VAmap <- renderLeaflet({
    req(basin_filter(), huc6_filter(), regionalAUs())
    m <- mapview(basin_filter(),label= basin_filter()$VAHU6, layer.name = 'Basin Chosen',
                 popup= leafpop::popupTable(basin_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG"))) + 
      mapview(huc6_filter(), color = 'yellow',lwd= 5, label= huc6_filter()$VAHU6, layer.name = c('Selected HUC6'),
              popup= leafpop::popupTable(huc6_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG")))
    m@map %>% setView(st_bbox(huc6_filter())$xmax[[1]],st_bbox(huc6_filter())$ymax[[1]],zoom = 9) })
  
  # Table of AUs within Selected VAHU6
  output$AUSummary <-  DT::renderDataTable({ req(AUs())
    DT::datatable(AUs() %>% st_drop_geometry(), rownames = FALSE, 
                  options= list(scrollX = TRUE, pageLength = nrow(AUs()), scrollY = "300px", dom='Bti'))   })

  # Table of Stations within Selected VAHU6
  output$stationSummary <- DT::renderDataTable({
    req(huc6_filter())
    z <- filter(conventionals, Huc6_Vahu6 %in% huc6_filter()$VAHU6) %>%
      distinct(FDT_STA_ID, .keep_all = TRUE) %>%
      select(FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:STA_CBP_NAME) %>% 
      mutate(`Analyzed By App` = ifelse(FDT_STA_ID %in% unique(stationTable()$STATION_ID), 'yes','no'))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "300px", dom='Bti')) %>%
      DT::formatStyle('Analyzed By App', target = 'row', backgroundColor = styleEqual(c('yes','no'), c('lightgray', 'yellow')))
  })
  
  
  
  #  output$test <- renderPrint({#req(regionalAUs)
  #    glimpse(AUs())})
  
  
  
})