# Get actual WQS info from each layer and QA vs what human called it

equals <- function(x, y){
  suppressWarnings(if(is.na(all(as.character(x), as.character(y)))){return('same')})
  suppressWarnings(if(as.character(x) == as.character(y)){return('same')})
  suppressWarnings(if(as.character(x) != as.character(y)){return('flag')})
}

humanQA <- function(WQStable){
  nameConversion <- data.frame(abbrev = c('RL', 'EL', 'EP','LP'),
                               realName = c('riverine_05082020','estuarinelines_05082020','estuarinepolygons_05082020',
                                            'lakes_reservoirs_05082020'))
  
  toDrop <- data.frame(StationID = as.character(), WQS_ID = as.character())
  
  for(i in 1:nrow(nameConversion)){
    wqsLayer <- st_read('GIS/WQS_layers_05082020.gdb', 
                        layer = as.character(nameConversion$realName[i]) ,
                        fid_column_name = "OBJECTID") %>%
      st_drop_geometry()
    
    dealWithMe <- filter(WQStable, str_extract(WQS_ID, "^.{2}") == nameConversion$abbrev[i]) %>%
      left_join(wqsLayer, by = 'WQS_ID') %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`, SEC, CLASS, SPSTDS, PWS, contains("Trout"), Tier_III)
    
    WQStableQA <- left_join(dealWithMe, rename(existingData, "StationID" = 'FDT_STA_ID'), by = 'StationID') %>%
      mutate(SEC_flag = equals(SEC.x, SEC.y), 
             CLASS_flag = equals(CLASS.x, CLASS.y),
             SPSTDS_flag = equals(SPSTDS.x, SPSTDS.y),
             PWS_flag = equals(PWS.x, PWS.y),
             Tier_III_flag = equals(Tier_III.x, Tier_III.y) )
      
    if('Trout' %in% names(WQStable)){
      WQStable <- mutate(WQStable, Trout_flag = equals(Trout.x, Trout.y))
    }
    
    toDrop <- bind_rows(toDrop,
                       filter_all(WQStableQA, any_vars(str_detect(., pattern = "flag"))) %>%
                         dplyr::select(StationID, WQS_ID))
  }
  WQStable_QAed <- filter(WQStable, ! WQS_ID %in% toDrop$WQS_ID)
  
  return(WQStable_QAed)
  
}

  