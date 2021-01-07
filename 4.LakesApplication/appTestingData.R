source('global.R')

# Pull data from server
conventionals <- pin_get("conventionals2022IRdraft", board = "rsconnect") %>%
  filter(FDT_DATE_TIME >= "2015-01-01 00:00:00 UTC" )
vahu6 <- st_as_sf(pin_get("vahu6", board = "rsconnect")) # bring in as sf object
WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")
# placeholder for now, shouldn't be a spatial file
historicalStationsTable <- st_read('data/GIS/2020_wqms.shp') %>%
  st_drop_geometry()#read_csv('data/stationsTable2022begin.csv') # last cycle stations table (forced into new station table format)
WQMstationFull <- pin_get("WQM-Station-Full", board = "rsconnect")


# Bring in local data (for now)
ammoniaAnalysis <- readRDS('userDataToUpload/processedStationData/ammoniaAnalysis.RDS')
lakeNutStandards <- read_csv('data/9VAC25-260-187lakeNutrientStandards.csv')


DEQregionSelection <- 'BRRO'
lakeSelection <- "Smith Mountain Lake"

regionalAUs <- st_zm(st_as_sf(pin_get('AUreservoir_EVJ', board = 'rsconnect')))

the_data <- filter(regionalAUs, ASSESS_REG %in% DEQregionSelection) 
lake_filter <- filter(the_data, WATER_NAME %in% lakeSelection)


stationTable <- read_csv('userDataToUpload/processedStationData/stationTableResults.csv',
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
  mutate(Lake_Name = case_when(WATER_NAME %in% c('Smith Lake (Aquia Reservoir)') ~ 'Aquia Reservoir (Smith Lake)',
                               WATER_NAME %in% c('Claytor Lake (New River)', 'Claytor Lake (Peak Creek)') ~ 'Claytor Lake',
                               WATER_NAME %in% c('Fairystone Lake (Goblin Town Creek)') ~ 'Fairystone Lake', 
                               WATER_NAME %in% c('Harwoods Mill Reservoir (PWS)') ~ 'Harwoods Mill Reservoir',
                               WATER_NAME %in% c('Lake Anna', 'Lake Anna/Contrary Creek', 'Lake Anna/Freshwater Creek', 
                                                 'Lake Anna/Gold Mine Creek', 'Lake Anna/Pamunkey Creek', 
                                                 'Lake Anna/Plentiful Creek', 'Terrys Run/Lake Anna') ~ 'Lake Anna',
                               WATER_NAME %in% c('Lake Cohoon (PWS)') ~ 'Lake Cohoon',          
                               WATER_NAME %in% c('Lake Kilby (PWS)') ~ 'Lake Kilby',
                               WATER_NAME %in% c('Lake Meade (PWS)') ~ 'Lake Meade',
                               WATER_NAME %in% c('Lake Moomaw (Jackson River)') ~ 'Lake Moomaw',
                               WATER_NAME %in% c('Lake Prince - Reservoir (PWS)') ~ 'Lake Prince - Reservoir',
                               WATER_NAME %in% c('Lake Smith (PWS)') ~ 'Lake Smith',
                               WATER_NAME %in% c('Lake Whitehurst (PWS)') ~ 'Lake Whitehurst',
                               WATER_NAME %in% c('Leesville Lake', 'Leesville Lake (Pigg R.)', 
                                                 'Leesville Lake Middle (Roanoke R.)') ~ 'Leesville Lake',
                               WATER_NAME %in% c('Lee Hall Reservoir- Upper, Middle','Lee Hall Reservoir-Lower') ~ 'Lee Hall Reservoir',
                               WATER_NAME %in% c('Little Creek Reservoir - (PWS)') ~ 'Little Creek Reservoir (VBC)',
                               WATER_NAME %in% c('Little Creek Reservoir (PWS)') ~ 'Little Creek Reservoir (JCC)',
                               WATER_NAME %in% c('Lone Star Lake F (PWS)') ~ 'Lone Star Lake F (Crystal Lake)', 
                               WATER_NAME %in% c('Lone Star Lake G (PWS)') ~ 'Lone Star Lake G (Crane Lake)', 
                               WATER_NAME %in% c('Lone Star Lake I (PWS)') ~ 'Lone Star Lake I (Butler Lake)', 
                               WATER_NAME %in% c('Martinsville (Beaver Creek) Reservoir') ~ 'Martinsville Reservoir (Beaver Creek Reservoir)',
                               WATER_NAME %in% c('Philpott Reservoir (Goblin Town Creek)', 
                                                 'Philpott Reservoir (Smith River)') ~ "Philpott Reservoir",
                               WATER_NAME %in% c('Roanoke River') ~ 'Lake Gaston',                         
                               WATER_NAME %in% c('S F Rivanna River Reservoir') ~ 'Rivanna Reservoir (South Fork Rivanna Reservoir)',
                               str_detect(WATER_NAME, 'Smith Mtn. Lake') ~ 'Smith Mountain Lake', 
                               WATER_NAME %in% c('Speights Run - Lake (PWS)') ~ 'Speights Run Lake',
                               WATER_NAME %in% c('Waller Mill Reservoir (PWS)') ~ 'Waller Mill Reservoir',
                               WATER_NAME %in% c('Unnamed pond near Tanyard Swamp') ~ 'Tanyard Swamp',
                               WATER_NAME %in% c('Unsegmented lakes in G03') ~ 'West Run',
                               TRUE ~ as.character(WATER_NAME))) %>%
  left_join(lakeNutStandards, by = c('Lake_Name'))

  
  
  
# Connect regionalAUs$WATER_NAME to WQS Standards

dplyr::select(stationTable, GNIS_Name, WATER_NAME) %>%
  distinct(GNIS_Name, WATER_NAME) %>%
  mutate(Lake_Name = NA) %>%
  arrange(WATER_NAME) %>%
  write_csv('lakeName.csv')




library(fuzzyjoin)
lakesWQS <- st_read('C:/HardDriveBackup/R/GitHub/IR2022/1.preprocessData/GIS/WQS_layers_05082020.gdb', layer = 'lakes_reservoirs_05082020' , fid_column_name = "OBJECTID") %>%
  st_transform(4326)

unique(lakesWQS$GNIS_Name)


#stringdist_left_join(dplyr::select(stations, 'Station_Id'), by = c('StationID' = 'Station_Id'), max_dist = 1) %>%
  
               
unique(stationTable$GNIS_Name)[! unique(stationTable$GNIS_Name) %in% regionalAUs$WATER_NAME]

unique(lakeNutStandards$`Man-made Lake or Reservoir Name`) %in% unique(stationTable$GNIS_Name)
unique(lakeNutStandards$`Man-made Lake or Reservoir Name`)[!unique(lakeNutStandards$`Man-made Lake or Reservoir Name`) %in% unique(stationTable$GNIS_Name)]

z <- dplyr::select(stationTable, STATION_ID, GNIS_Name, WATER_NAME) %>%
  left_join(dplyr::select(regionalAUs, WATER_NAME, LOCATION, ID305B), by = c('GNIS_Name' = 'WATER_NAME'))
