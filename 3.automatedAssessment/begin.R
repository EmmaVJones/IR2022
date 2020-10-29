




basinCodesConversion <- read_csv('data/basinCodeConversion.csv') %>%
  filter(BASIN != 7) %>%
  bind_rows(data.frame(BASIN = '7D', Basin_Code = 'Small Coastal'))



subbasinOptionsByWQStype <- read_csv('data/subbasinOptionsByWQStype&Region.csv') %>%
  #  left_join(WQSlayerConversion, by = c('WQS_ID_Prefix' = 'WQS_ID', 'waterbodyType')) %>%
  left_join(basinCodesConversion, by = c('SubbasinOptions' = 'BASIN'))