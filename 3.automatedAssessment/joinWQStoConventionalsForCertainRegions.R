# This script joins available WQS information to conventionals dataset for regions that are interested

# Starting with PRO
region <- "PRO"

library(tidyverse)
library(pins)
library(config)
# get configuration settings
conn <- config::get("connectionSettings")

# use API key to register board
board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))

conventionals <- pin_get("conventionals2022IRfinalWithSecchi", board = "rsconnect")
WQMstationFull <- pin_get("WQM-Station-Full", board = "rsconnect")
WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")

stationTable <- read_csv('processedStationData/stationsTable2022begin.csv') %>% 
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
  lakeNameStandardization() %>% # standardize lake names
  
  
  # extra special step
  mutate(Lake_Name = case_when(STATION_ID %in% c('2-TRH000.40') ~ 'Thrashers Creek Reservoir',
                               TRUE ~ as.character(Lake_Name))) %>%
  
  
  
  left_join(lakeNutStandards, by = c('Lake_Name')) %>%
  # lake drummond special standards
  mutate(`Chlorophyll a (ug/L)` = case_when(Lake_Name %in% c('Lake Drummond') ~ 35,
                                            TRUE ~ as.numeric(`Chlorophyll a (ug/L)`)),
         `Total Phosphorus (ug/L)` = case_when(Lake_Name %in% c('Lake Drummond') ~ 40,
                                               TRUE ~ as.numeric(`Total Phosphorus (ug/L)`)))
# limit to just region of interest
regionalStationTable <- filter(stationTable, REGION == region)
View( filter(stationTable, REGION == region) %>% group_by(STATION_ID) %>% mutate(n=n()) %>% filter(n>1))

# filter conventionals to just stations in region of interest and join WQS and other spatial info
conventionals_region <- filter(conventionals, FDT_STA_ID %in% regionalStationTable$STATION_ID) %>% 
  left_join(dplyr::select(regionalStationTable, STATION_ID, WQS_ID:`Total Phosphorus (ug/L)`),
            by = c('FDT_STA_ID' = 'STATION_ID'))


write.csv(conventionals_region, paste('dataForAssessors/',region,'IR2022conventionals_withWQS.csv'))

