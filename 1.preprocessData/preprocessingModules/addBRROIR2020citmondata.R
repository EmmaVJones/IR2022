# Bring in final 2020IR stations table and limit to just citmon and non agency sites
BRRO <- read_csv('data/final2020data/RegionalResultsRiverine_BRROCitMonNonAgencyFINAL.csv') %>%
  bind_rows(read_csv('data/final2020data/lakeStations2020_BRRO_citmonNonAgency.csv')) %>% 
  filter(STATION_TYPE_1 %in% c( "CMON", "NONA", "USPS") |
           STATION_TYPE_2 %in% c( "CMON", "NONA", "USPS") |
           STATION_TYPE_3 %in% c( "CMON", "NONA", "USPS"))

# Bring in final 2020 IR conventionals dataset that includes BRRO citmon/non agency data organized

ir2020 <- read_csv('data/final2020data/conventionals_final2020_citmonNonAgency.csv') %>% 
  # make old format match new
  mutate(Source_Sta_Id = as.character(NA),  Data_Source = 'DEQ',
         STORET_49573= NA, RMK_49573= NA, STORET_PCWLF= NA, RMK_PCWLF= NA, 
         STORET_PNWLF= NA, RMK_PNWLF= NA, STORET_530XX= NA, RMK_530XX= NA) %>%
  dplyr::select(names(conventionals)) %>% 
  # just get citmon and non agency data
  filter(FDT_STA_ID %in% BRRO$FDT_STA_ID) %>% 
  # and only in current IR window
  filter(as.Date(FDT_DATE_TIME) >= as.Date('2015-01-01') &
           as.Date(FDT_DATE_TIME) <= as.Date('2020-12-31'))
