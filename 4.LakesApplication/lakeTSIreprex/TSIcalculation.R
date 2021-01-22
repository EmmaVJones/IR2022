### TSI equations
TSIcalculation <- function(x){
  if(unique(x$lakeStation) == TRUE){
    if(is.na(unique(x$Lakes_187B))){
      # first fill down secchi depth in case it isn't stored exactly at 0.3 meter
      secchiFix <- x %>%
        group_by(FDT_STA_ID, FDT_DATE_TIME) %>% 
        fill(SECCHI_DEPTH_M, .direction = "downup") %>%
        filter(FDT_DEPTH <= 0.3) %>%
        # remove all data except those from mid June through mid September (going with June 15 and Sept 15 since guidance does not specify)
        mutate(monthday = as.numeric(paste0(month(FDT_DATE_TIME), day(FDT_DATE_TIME)))) %>% # crafty way to get dates desired without specifying years
        filter(between(monthday, 615, 915 )) %>%
        dplyr::select(-monthday)
      
      # Calculate Secchi depth TSI
      SDdata <- filter(secchiFix, !is.na(SECCHI_DEPTH_M) ) %>% # drop missing data
        filter(!( LEVEL_SECCHI_DEPTH %in% c('Level II', 'Level I'))) %>% # get lower levels out (citizen monitoring)
        dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH,SECCHI_DEPTH_M) %>% # only keep fields of interest
        mutate(TSI_SD = 10*(6 - (log(SECCHI_DEPTH_M) / log(2))) ) # calculate individual secchi calc (save for later for visualization purposes)
      SD <- suppressWarnings(suppressMessages( # quiet things down
        SDdata %>%
          group_by(FDT_STA_ID) %>%
          summarise(meanSD = mean(SECCHI_DEPTH_M, na.rm = T), # take average of all secchi depths first
                    TSI_SD = 10*(6 - (log(meanSD) / log(2))) ) ))# log() is natural log in R
      
      # Calculate Chlorophyll a TSI
      chlaData <- filter(secchiFix, !is.na(CHLOROPHYLL_A_ug_L) ) %>% # drop missing data
        filter(!( LEVEL_CHLOROPHYLL_A %in% c('Level II', 'Level I'))) %>% # get lower levels out (citizen monitoring)
        dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, CHLOROPHYLL_A_ug_L) %>%  # only keep fields of interest
        mutate(TSI_chla = 10*(6 - (2.04 - 0.68 * (log( CHLOROPHYLL_A_ug_L))) / log(2))) 
      chla <- suppressWarnings(suppressMessages(
        chlaData %>% 
          group_by(FDT_STA_ID) %>%
          summarise(meanchla = mean(CHLOROPHYLL_A_ug_L, na.rm = T), # take average of all chl a first
                    TSI_chla = 10*(6 - (2.04 - 0.68 * (log(meanchla))) / log(2)))  ))# log() is natural log in R
      
      # Calculate Total Phosphorus TSI
      TPdata <- filter(secchiFix, !is.na(PHOSPHORUS_mg_L) ) %>%
        filter(!( LEVEL_PHOSPHORUS %in% c('Level II', 'Level I'))) %>% # get lower levels out
        dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, PHOSPHORUS_mg_L) %>%
        mutate(PHOSPHORUS_ug_L = PHOSPHORUS_mg_L * 1000,# first convert mg/L to ug/L
               TSI_TP = 10*(6 - (log( (48 / PHOSPHORUS_ug_L )) / log(2))) )   
      TP <- suppressWarnings(suppressMessages(
        TPdata %>%
          group_by(FDT_STA_ID) %>%
          summarise(meanTP = mean(PHOSPHORUS_ug_L, na.rm = T), # take average of all TP first
                    TSI_TP = 10*(6 - (log( (48 / meanTP)) / log(2))) ) ))# log() is natural log in R
      
      TSI <- full_join(SD, chla, by = c('FDT_STA_ID')) %>%
        full_join(TP, by = c('FDT_STA_ID')) %>%
        bind_cols(tibble(associatedData = list(full_join(SDdata, chlaData, by = c('FDT_STA_ID', 'FDT_DATE_TIME', 'FDT_DEPTH')) %>%
                                                 full_join(TPdata, by = c('FDT_STA_ID', 'FDT_DATE_TIME', 'FDT_DEPTH'))) ))
      
      return(TSI)
      
    } else {return(tibble(FDT_STA_ID = NA, meanSD = NA, TSI_SD = NA, meanchla = NA, TSI_chla = NA, meanTP = NA, TSI_TP = NA, associatedData = list(NA)))}
  } else {return(tibble(FDT_STA_ID = NA, meanSD = NA, TSI_SD = NA, meanchla = NA, TSI_chla = NA, meanTP = NA, TSI_TP = NA, associatedData = list(NA)))} 
}

# how to test on one station
#TSIcalculation(stationData1)

# How to make a fake dataset to test on more than one station
#x <- stationData1 %>%
#  bind_rows(mutate(stationData1, FDT_STA_ID = 'FAKE'))


# TSI assessment decision function
TSIassessment <- function(x){
  TSI <- TSIcalculation(x)
  
  if(nrow(TSI) > 0){
    if(nrow(TSI) > 1){ # if more than one station in AU, first average TSI results then assess
      return(
        TSI %>% # using mean to be consistent with mean in TSIcalculation()
          summarise(TSI_SD = as.numeric(signif(mean(TSI_SD, na.rm = T),digits = 2)), 
                    TSI_chla = as.numeric(signif(mean(TSI_chla, na.rm = T), digits = 2)),
                    TSI_TP = as.numeric(signif(mean(TSI_TP, na.rm = T), digits = 2))) %>%
          mutate(ID305B = unique(x$ID305B_1)) %>%
          dplyr::select(ID305B, everything()))
    } else {
      return(TSI %>%
               mutate(ID305B = unique(x$ID305B_1),
                      TSI_SD = as.numeric(signif(TSI_SD, digits = 2)),
                      TSI_chla = as.numeric(signif(TSI_chla, digits = 2)), 
                      TSI_TP = as.numeric(signif(TSI_TP, digits = 2))) %>%
               dplyr::select(ID305B,TSI_SD, TSI_chla, TSI_TP))}
  } else {return(tibble(ID305B = NA, TSI_SD = NA, TSI_chla = NA, TSI_TP = NA))}
}
# how to test
#TSIassessment(x)

