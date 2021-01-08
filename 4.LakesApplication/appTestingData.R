source('global.R')

# Pull data from server
conventionals <- pin_get("conventionals2022IRdraft", board = "rsconnect") %>%
  filter(FDT_DATE_TIME >= "2015-01-01 00:00:00 UTC" )
#vahu6 <- st_as_sf(pin_get("vahu6", board = "rsconnect")) # bring in as sf object
WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")
# placeholder for now, shouldn't be a spatial file
historicalStationsTable <-  st_read('data/GIS/va20ir_wqms.shp') %>%
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
#lakeStations1 <- lake_filter1 %>%
#  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
#           remove = F, # don't remove these lat/lon cols from df
#           crs = 4326) # add projection, needs to be geographic for now bc entering lat/lng
# 
# z <- suppressWarnings(st_coordinates(sf::st_centroid(AUs1 %>% group_by(Lake_Name) %>% summarise())))
# 
# CreateWebMap(maps = c("Topo","Imagery"), collapsed = TRUE) %>%
#   {if(nrow(AUs1)>1)
#     setView(., z[1], z[2], zoom = 10) 
#     else setView(., z[1], z[2], zoom = 12) } %>%
#   addPolygons(data= AUs1, group = 'Selected Lake',
#               popup=leafpop::popupTable(AUs1, zcol=c('Lake_Name',"ID305B","ASSESS_REG"))) %>%
#   {if(nrow(lakeStations1) > 0)
#     addCircleMarkers(., data = lakeStations1, color='black', fillColor='yellow', radius = 4,
#                      fillOpacity = 0.5,opacity=0.8,weight = 1,stroke=T, group="Monitored Stations",
#                      label = ~STATION_ID, layerId = ~STATION_ID) 
#     else . } %>%
#   addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
#                    overlayGroups = c('Monitored Stations', 'Selected Lake'),
#                    options=layersControlOptions(collapsed=T),
#                    position='topleft')
            
## Assessment Unit Review
#dplyr::select(lake_filter1, Lake_Name, VAHU6, Lakes_187B) %>%
#  group_by(Lake_Name) %>%
#  summarise(VAHU6 = toString(sort(unique(VAHU6))),
#            `Section 187` = toString(sort(unique(Lakes_187B))))

conventionalsLake1 <- filter(conventionals, FDT_STA_ID %in% lake_filter1$STATION_ID) %>%
  left_join(dplyr::select(stationTable1, STATION_ID:VAHU6,
                          WQS_ID:`Total Phosphorus (ug/L)`),
            #WQS_ID:`Max Temperature (C)`), 
            by = c('FDT_STA_ID' = 'STATION_ID')) %>%
  filter(!is.na(ID305B_1)) %>%
  pHSpecialStandardsCorrection() #correct pH to special standards where necessary

AUselectionOptions1 <- unique(dplyr::select(lake_filter1, ID305B_1:ID305B_10) %>% 
         mutate_at(vars(starts_with("ID305B")), as.character) %>%
         pivot_longer(ID305B_1:ID305B_10, names_to = 'ID305B', values_to = 'keep') %>%
         pull(keep) )
selectedAU1 <- AUselectionOptions1[!is.na(AUselectionOptions1) & !(AUselectionOptions1 %in% c("NA", "character(0)", "logical(0)"))][1]

stationSelectionOptions1 <- filter_at(lake_filter1, vars(starts_with("ID305B")), any_vars(. %in% selectedAU1)) %>%
  distinct(STATION_ID) %>%
  arrange(STATION_ID) %>%
  pull()
stationSelection1 <- stationSelectionOptions1[1]

stationInfo1 <- filter(stationTable1, STATION_ID == stationSelection1) %>% 
  select(STATION_ID:VAHU6, WQS_ID:`Total Phosphorus (ug/L)`)

point <- dplyr::select(stationInfo1,  STATION_ID, starts_with('ID305B'), LATITUDE, LONGITUDE ) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) # add projection, needs to be geographic for now bc entering lat/lng
segmentChoices <- dplyr::select(point, starts_with('ID305B')) %>% st_drop_geometry() %>% as.character()  
segment <- filter(regionalAUs1, ID305B %in% segmentChoices)
map1 <- mapview(segment,zcol = 'ID305B', label= segment$ID305B, layer.name = 'Assessment Unit (ID305B_1)',
                popup= leafpop::popupTable(segment, zcol=c("ID305B","Acres","CYCLE","WATER_NAME")), legend= FALSE) + 
  mapview(point, color = 'yellow', lwd = 5, label= point$STATION_ID, layer.name = c('Selected Station'),
          popup=NULL, legend= FALSE)
map1@map %>% setView(point$LONGITUDE, point$LATITUDE, zoom = 12)
