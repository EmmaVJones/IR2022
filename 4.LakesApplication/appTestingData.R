source('global.R')

# Pull data from server
conventionals <- pin_get("conventionals2022IRdraft", board = "rsconnect") %>%
  filter(FDT_DATE_TIME >= "2015-01-01 00:00:00 UTC" )
#vahu6 <- st_as_sf(pin_get("vahu6", board = "rsconnect")) # bring in as sf object
WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")
# placeholder for now, shouldn't be a spatial file
historicalStationsTable <- st_read('data/GIS/2020_wqms.shp') %>%
  st_drop_geometry()#read_csv('data/stationsTable2022begin.csv') # last cycle stations table (forced into new station table format)
WQMstationFull <- pin_get("WQM-Station-Full", board = "rsconnect")


# Bring in local data (for now)
ammoniaAnalysis <- readRDS('userDataToUpload/processedStationData/ammoniaAnalysis.RDS')
lakeNutStandards <- read_csv('data/9VAC25-260-187lakeNutrientStandards.csv')

# user brings in station table on first page
stationTable1 <- read_csv('userDataToUpload/processedStationData/stationTableResults.csv',
                         col_types = cols(COMMENTS = col_character())) %>%# force to character bc parsing can incorrectly guess logical based on top 1000 rows
  filter_at(vars(starts_with('TYPE')), any_vars(. == 'L')) %>% # keep only lake stations
  # add WQS information to stations
  left_join(WQSlookup, by = c('STATION_ID'='StationID')) %>%
  mutate(CLASS_BASIN = paste(CLASS,substr(BASIN, 1,1), sep="_")) %>%
  mutate(CLASS_BASIN = ifelse(CLASS_BASIN == 'II_7', "II_7", as.character(CLASS))) %>%
  # Fix for Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard)
  left_join(WQSvalues, by = 'CLASS_BASIN') %>%
  dplyr::select(-c(CLASS.y,CLASS_BASIN)) %>%
  rename('CLASS' = 'CLASS.x') %>%
  left_join(dplyr::select(WQMstationFull, WQM_STA_ID, EPA_ECO_US_L3CODE, EPA_ECO_US_L3NAME) %>%
              distinct(WQM_STA_ID, .keep_all = TRUE), by = c('STATION_ID' = 'WQM_STA_ID')) %>% # last cycle had code to fix Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard) but not sure if necessary
  lakeNameStandardization() %>% # standardize lake names
  left_join(lakeNutStandards, by = c('Lake_Name'))




# User clicks region and brings back entire state (this just expedites original app rendering and spreads
# out data requests)
regionalAUs1 <- st_zm(st_as_sf(pin_get('AUreservoir_EVJ', board = 'rsconnect'))) %>%
  lakeNameStandardization()


DEQregionSelection1 <- 'BRRO'
lakeSelection1 <- "Smith Mountain Lake"#"Falling Creek Reservoir"#"Hogan Lake"#"Smith Mountain Lake"
 # filter(regionalAUs, ASSESS_REG %in% DEQregionSelection) %>% 
#  distinct(Lake_Name) %>% arrange(Lake_Name) %>% pull()


AUs1 <- filter(regionalAUs1, Lake_Name %in% lakeSelection1 & ASSESS_REG %in% DEQregionSelection1)
#the_data <- filter(regionalAUs, ASSESS_REG %in% DEQregionSelection) 
#lake_AUs <- filter(the_data, Lake_Name %in% lakeSelection)
lake_filter1 <- filter_at(stationTable1, vars(starts_with('ID305B')), any_vars(. %in% AUs1$ID305B)) 
  
 



# Lake Map

z <- st_coordinates(sf::st_centroid(AUs1 %>% group_by(Lake_Name) %>% summarise()))
#bounds <- st_bbox(AUs) %>% as.numeric()
lakeStations <- lake_filter %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) # add projection, needs to be geographic for now bc entering lat/lng
CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE) %>%
  setView(z[1], z[2], zoom = 10) %>%
  #fitBounds(as.numeric(bounds$xmin), as.numeric(bounds$ymin), as.numeric(bounds$xmax), as.numeric(bounds$ymax)) %>%
  #fitBounds(min(lakeStations$LONGITUDE), min(lakeStations$LATITUDE), max(lakeStations$LONGITUDE), max(lakeStations$LATITUDE))  %>% 
  #addPolygons(data= filter(regions, ), group = 'Selected Lake'
  addPolygons(data= AUs1, group = 'Selected Lake',
              popup=leafpop::popupTable(AUs1, zcol=c('Lake_Name',"ID305B","ASSESS_REG"))) %>%
  #addCircleMarkers(data = lakeStations, color='black', fillColor='yellow', radius = 4,
  #                 fillOpacity = 0.5,opacity=0.8,weight = 1,stroke=T, group="Monitored Stations",
  #                 label = ~STATION_ID, layerId = ~STATION_ID) %>%
  addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                   overlayGroups = c('Monitored Stations', 'Selected Lake'),
                   options=layersControlOptions(collapsed=T),
                   position='topleft') 
              
            

leaflet() %>%
  addTiles() %>%
  addPolygons(data =AUs)
mapview(AUs, label= 'ID305B', layer.name = 'Lake Chosen', 
             popup= leafpop::popupTable(AUs, zcol=c('Lake_Name',"ID305B","ASSESS_REG")), legend= FALSE)


# Stations in Lake
stationSummary <- filter(conventionals, FDT_STA_ID %in% lake_filter$STATION_ID) %>%
    distinct(FDT_STA_ID, .keep_all = TRUE)  %>% 
    dplyr::select(FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:Data_Source, Latitude, Longitude) %>% 
    dplyr::select(-FDT_DATE_TIME)  # drop date time bc confusing to users
    