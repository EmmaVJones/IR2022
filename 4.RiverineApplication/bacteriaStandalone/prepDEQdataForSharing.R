# Data preprocessing to make DEQ conventionals publicly available
source('global.R')

conventionals <- pin_get("conventionals2022IRfinalWithSecchi", board = "rsconnect") 
WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")
WQMstationFull <- pin_get("WQM-Station-Full", board = "rsconnect")


stationTable <- read_csv('userDataToUpload/processedStationData/stationTableResults.csv',
                         col_types = cols(COMMENTS = col_character())) # force to character bc parsing can incorrectly guess logical based on top 1000 rows
# Remove stations that don't apply to application
lakeStations <- filter_at(stationTable, vars(starts_with('TYPE')), any_vars(. == 'L'))
estuarineStations <- filter(stationTable, str_detect(ID305B_1, 'E_'))
stationTable <- filter(stationTable, !STATION_ID %in% lakeStations$STATION_ID) %>%
  filter(!STATION_ID %in% estuarineStations$STATION_ID) %>%
  # add WQS information to stations
  left_join(WQSlookup, by = c('STATION_ID'='StationID')) %>%
  mutate(CLASS_BASIN = paste(CLASS,substr(BASIN, 1,1), sep="_")) %>%
  mutate(CLASS_BASIN = ifelse(CLASS_BASIN == 'II_7', "II_7", as.character(CLASS))) %>%
  # Fix for Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard)
  left_join(WQSvalues, by = 'CLASS_BASIN') %>%
  dplyr::select(-c(CLASS.y,CLASS_BASIN)) %>%
  rename('CLASS' = 'CLASS.x') %>%
  left_join(dplyr::select(WQMstationFull, WQM_STA_ID, EPA_ECO_US_L3CODE, EPA_ECO_US_L3NAME) %>%
              distinct(WQM_STA_ID, .keep_all = TRUE), by = c('STATION_ID' = 'WQM_STA_ID')) %>%
  # last cycle had code to fix Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard) but not sure if necessary
  mutate(lakeStation = FALSE)


# just give Roanoke data
stationTable <- filter(stationTable, VAHU6 %in% c('RU13', 'RU14'))

conventionals_HUC1 <- filter(conventionals, FDT_STA_ID %in% stationTable$STATION_ID) %>%
  left_join(dplyr::select(stationTable, STATION_ID:VAHU6,lakeStation,
                          WQS_ID:EPA_ECO_US_L3NAME),
            #WQS_ID:`Max Temperature (C)`), 
            by = c('FDT_STA_ID' = 'STATION_ID')) %>%
  filter(!is.na(ID305B_1)) %>%
  pHSpecialStandardsCorrection() %>% 
  
  # drop any stations without Ecoli data
  filter(!is.na(ECOLI)) %>% 
  
  # only send out necessary data
  dplyr::select(FDT_STA_ID:FDT_DEPTH, FDT_COMMENT, ECOLI:LEVEL_ECOLI, Data_Source:EPA_ECO_US_L3NAME)


saveRDS(conventionals_HUC1, 'data/conventionals_HUC.RDS')
