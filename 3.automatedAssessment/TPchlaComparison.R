library(tidyverse)

newDat <- read.csv('processedStationData/stationTableResults.csv')
oldDat <- read.csv('processedStationData/stationTableStatewideExample (43).csv')


difference <- left_join(select(newDat, STATION_ID, REGION, ID305B_1, ID305B_2, ID305B_3, LACUSTRINE,
                               NUT_TP_EXC:NUT_CHLA_STAT) %>% 
                          mutate(NUT_TP_EXC_NEW = NUT_TP_EXC, NUT_TP_SAMP_NEW = NUT_TP_SAMP, NUT_TP_STAT_NEW = NUT_TP_STAT,
                                 NUT_CHLA_EXC_NEW = NUT_CHLA_EXC, NUT_CHLA_SAMP_NEW = NUT_CHLA_SAMP, NUT_CHLA_STAT_NEW = NUT_CHLA_STAT),
                        select(oldDat, STATION_ID,REGION, ID305B_1, ID305B_2, ID305B_3, LACUSTRINE,
                               NUT_TP_EXC:NUT_CHLA_STAT) %>% 
                          mutate(NUT_TP_EXC_OLD = NUT_TP_EXC, NUT_TP_SAMP_OLD = NUT_TP_SAMP, NUT_TP_STAT_OLD = NUT_TP_STAT,
                                 NUT_CHLA_EXC_OLD = NUT_CHLA_EXC, NUT_CHLA_SAMP_OLD = NUT_CHLA_SAMP, NUT_CHLA_STAT_OLD = NUT_CHLA_STAT),
                        by = c('STATION_ID', 'REGION','ID305B_1', 'ID305B_2', 'ID305B_3', 'LACUSTRINE')) %>% 
  mutate(diffTPexc = NUT_TP_EXC_NEW- NUT_TP_EXC_OLD,
         diffTPstatus = NUT_TP_STAT_NEW != NUT_TP_STAT_OLD,
         diffCHLAexc = NUT_CHLA_EXC_NEW- NUT_CHLA_EXC_OLD,
         diffCHLAstatus = NUT_CHLA_STAT_NEW != NUT_CHLA_STAT_OLD) %>% 
  select("STATION_ID",REGION, ID305B_1, ID305B_2, ID305B_3, LACUSTRINE,"NUT_TP_EXC_NEW","NUT_TP_SAMP_NEW", "NUT_TP_STAT_NEW"  ,"NUT_TP_EXC_OLD"  ,  "NUT_TP_SAMP_OLD"  , "NUT_TP_STAT_OLD" , "diffTPexc"  ,diffTPstatus,
  "NUT_CHLA_EXC_NEW" , "NUT_CHLA_SAMP_NEW" ,"NUT_CHLA_STAT_NEW" ,"NUT_CHLA_EXC_OLD" , "NUT_CHLA_SAMP_OLD","NUT_CHLA_STAT_OLD",   "diffCHLAexc", diffCHLAstatus) %>% 
  arrange(desc(diffTPstatus), REGION)
View(difference)
write.csv(difference %>% filter(diffTPstatus == TRUE), 'TP_CHLA_difference.csv', row.names = F, na='')
write.csv(difference, 'TP_CHLA_differenceALL.csv', row.names = F, na='')
