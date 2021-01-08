
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


