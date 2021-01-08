
# Lake Map
output$VAmap <- renderLeaflet({ req(AUs(), lake_filter()) 
  lakeStations <- lake_filter() %>%
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
             remove = F, # don't remove these lat/lon cols from df
             crs = 4326) # add projection, needs to be geographic for now bc entering lat/lng
  CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE) %>%
    fitBounds(min(lakeStations$LONGITUDE), min(lakeStations$LATITUDE), max(lakeStations$LONGITUDE), max(lakeStations$LATITUDE))  %>% 
    addPolygons(data= AUs(), group = 'Selected Lake',
                popup=leafpop::popupTable(AUs(), zcol=c('Lake_Name',"ID305B","ASSESS_REG"))) %>%
    addCircleMarkers(data = lakeStations, color='black', fillColor='yellow', radius = 4,
                     fillOpacity = 0.5,opacity=0.8,weight = 1,stroke=T, group="Monitored Stations",
                     label = ~STATION_ID, layerId = ~STATION_ID) %>%
    addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                     overlayGroups = c('Monitored Stations', 'Selected Lake'),
                     options=layersControlOptions(collapsed=T),
                     position='topleft') })


# Table of AUs within Selected Lake
output$AUSummary <-  DT::renderDataTable({ req(lake_AUs())
  DT::datatable(lake_AUs() %>% st_drop_geometry(), rownames = FALSE, 
                options= list(scrollX = TRUE, pageLength = nrow(lake_AUs()), scrollY = "300px", dom='Bti'),
                selection = 'none')   })

# Table of Stations within Selected Lake
stationSummary <- reactive({req(lake_filter())
  filter(conventionals, FDT_STA_ID %in% lake_filter()$STATION_ID) %>%
    distinct(FDT_STA_ID, .keep_all = TRUE)  %>% 
    dplyr::select(FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:Data_Source, Latitude, Longitude) %>% 
    dplyr::select(-FDT_DATE_TIME) }) # drop date time bc confusing to users 

output$stationSummary <- DT::renderDataTable({req(stationSummary())
  DT::datatable(stationSummary(), rownames = FALSE, 
                options= list(scrollX = TRUE, pageLength = nrow(stationSummary()), scrollY = "300px", dom='Bti'),
                selection = 'none') %>%
    DT::formatStyle('Analyzed By App', target = 'row', backgroundColor = styleEqual(c('yes','no'), c('lightgray', 'yellow'))) })












ui <- shinyUI(fluidPage(
  uiOutput('DEQregionSelectionUI'),
  uiOutput('lakeSelection_'),
  verbatimTextOutput('test'),
  leafletOutput('VAmap')))

server <- shinyServer(function(input, output, session) {
  stationTable <- reactive({ stationTable1})
  regionalAUs <- reactive({regionalAUs1})
  output$DEQregionSelectionUI <- renderUI({selectInput("DEQregionSelection", "Select DEQ Assessment Region", choices = c('BRRO', 'PRO', 'TRO', 'SWRO', 'NRO', 'VRO'))})
  output$lakeSelection_ <- renderUI({req(regionalAUs(), input$DEQregionSelection)
    z <- filter(regionalAUs(), ASSESS_REG %in% input$DEQregionSelection & ASSESS_REG %in% input$DEQregionSelection)
    selectInput('lakeSelection', 'Select Lake', choices = sort(unique(z$Lake_Name)))
  })
  
  AUs <- reactive({req(input$lakeSelection, input$DEQregionSelection, regionalAUs())
    filter(regionalAUs(), Lake_Name %in% input$lakeSelection & ASSESS_REG %in% input$DEQregionSelection)})
  
  output$VAmap <- renderLeaflet({ req(AUs())
    AUs<- filter(regionalAUs(), Lake_Name %in% input$lakeSelection)
    mapview(AUs)
    #mapview(AUs, label= 'ID305B', layer.name = 'Lake Chosen', 
    #        popup= leafpop::popupTable(AUs, zcol=c('Lake_Name',"ID305B","ASSESS_REG")), legend= FALSE)
  })
  
  output$test <- renderPrint({class(AUs())})
  
})

shinyApp(ui, server)


