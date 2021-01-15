# build lake station methods

# run Rmd to line 126

lakeStations <- filter(stationTable, str_detect(ID305B_1, 'L_') | str_detect(ID305B_2, 'L_') | str_detect(ID305B_3, 'L_') | str_detect(ID305B_4, 'L_') |
                         str_detect(ID305B_5, 'L_') | str_detect(ID305B_6, 'L_') | str_detect(ID305B_7, 'L_') | str_detect(ID305B_8, 'L_') | 
                         str_detect(ID305B_9, 'L_') | str_detect(ID305B_10, 'L_') ) 

stationTable1 <- stationTable # save a copy
stationTable <- lakeStations # only deal with lake stations for now

station <- '2-JKS046.40'
station <- '2-TRH000.40' # non 187 lake with WQS
station <- '2-JMS042.92' # for ammonia depth testing
#i = 18 # '2-JKS046.40'
#i = 5 # non 187 lake

# pull one station data
stationData <- filter(conventionals, FDT_STA_ID %in% station) %>% #stationTable$STATION_ID[i]) %>% #
  left_join(stationTable, by = c('FDT_STA_ID' = 'STATION_ID')) %>%
  pHSpecialStandardsCorrection() %>% # correct pH to special standards where necessary
  # special lake steps
  {if(station %in% lakeStations$STATION_ID) #if(stationTable$STATION_ID[i] %in% lakeStations$STATION_ID)
    mutate(., lakeStation = TRUE) %>%
      thermoclineDepth() # adds thermocline information and SampleDate
    else mutate(., lakeStation = FALSE)}


x <- stationData


View(dplyr::select(stationTable, STATION_ID, LACUSTRINE, REGION, WQS_ID:`Total Phosphorus (ug/L)`))
