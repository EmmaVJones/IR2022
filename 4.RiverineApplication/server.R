source('global.R')

regionalAUs <- st_read('C:/HardDriveBackup/GIS/Assessment/2020IR_draft/va_2020_aus_riverine.shp') %>%
  st_transform(4326)   # transform to WQS84 for spatial intersection 

shinyServer(function(input, output, session) {
  
  # display the loading feature until data loads into app
  load_data()
  
  ## Watershed Selection Tab
  
  # Query VAHUC6's By Selectize arguments
  the_data <- reactive({subbasinToVAHU6})
  region_filter <- shiny::callModule(dynamicSelect, "DEQregionSelection", the_data, "ASSESS_REG" )
  basin_filter <- shiny::callModule(dynamicSelect, "basinSelection", region_filter, "Basin_Code" )
  huc6_filter <- shiny::callModule(dynamicSelect, "HUC6Selection", basin_filter, "VAHU6" )
  AUs <- reactive({req(huc6_filter(), regionalAUs)
    suppressWarnings(st_intersection(st_zm(regionalAUs), filter(vahu6, VAHU6 %in% huc6_filter()$VAHU6)))})
  
  
  # Table of AUs within Selected VAHU6
  output$AUSummary <-  DT::renderDataTable({ req(regionalAUs,AUs())
    DT::datatable(AUs() %>% st_drop_geometry(), rownames = FALSE, 
                  options= list(scrollX = TRUE, pageLength = nrow(AUs()), scrollY = "300px", dom='Bt')) 
  })
})