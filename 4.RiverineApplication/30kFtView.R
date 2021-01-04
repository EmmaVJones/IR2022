source('appTestingData.R')


# parameter name crosswalk for prettier names later
parameterSTATcrosswalk <- tibble(Parameter = c("Temperature", 'Dissolved Oxygen', 'pH', 'E.coli', 'Enterococci', 'Ammonia', 'Water Column Metals',
                                               'Water Column Toxics', 'Sediment Metals', 'Sediment Toxics', 'Fish Tissue Metals', 'Fish Tissue Toxics',
                                               'Benthics', 'Total Phosphorus', 'Chlorophyll a'),
                                 ParameterSTAT = c("TEMP_STAT", "DO_STAT", "PH_STAT", "ECOLI_STAT", "ENTER_STAT", "AMMONIA_STAT", "WAT_MET_STAT", 
                                                   "WAT_TOX_STAT", "SED_MET_STAT", "SED_TOX_STAT", "FISH_MET_STAT", "FISH_TOX_STAT", "BENTHIC_STAT",
                                                   "NUT_TP_STAT", "NUT_CHLA_STAT"))

# Some data reorg to identify individual parameter issues and "rank" station (basically identifies worst status and colors
#  station that one color, user can dig in to station results to figure out what is causing the flag)
VAHU6stationSummary <- function(stationTable, VAHU6chosen, parameterSTATcrosswalk){
  vahu6StationSummary <- filter(stationTable, VAHU6 %in% VAHU6chosen) 
  
  if(nrow(vahu6StationSummary) > 0){
    vahu6StationSummary <- vahu6StationSummary %>%
      dplyr::select(STATION_ID, contains('_STAT')) %>% 
      group_by(STATION_ID) %>%
      pivot_longer(-STATION_ID, names_to = 'ParameterSTAT', values_to = 'Status') %>%
      mutate(individualColor = case_when(Status %in% c("10.5% Exceedance", 'IM') ~ 'red',
                                         Status %in% c('IN', 'Review') ~ 'yellow',
                                         Status %in% c("S") ~ 'green',
                                         is.na(Status) ~ 'gray'),
             # create a variable that ranks statuses to easily combine into single "score"
             individualScore = case_when(individualColor == 'red' ~ 1,
                                         individualColor == 'yellow' ~ 2,
                                         individualColor == 'green' ~ 3,
                                         individualColor == 'gray' ~ 4))
    
    overall <- vahu6StationSummary %>%
      group_by(STATION_ID, individualColor, individualScore) %>%
      dplyr::summarise(n = n()) %>%
      group_by(STATION_ID) %>%
      summarise(stationOverallScore = min(individualScore))
    
    return(left_join(vahu6StationSummary, overall, by = 'STATION_ID') %>%
             mutate(stationColor = case_when(stationOverallScore == 1 ~ 'red',
                                             stationOverallScore == 2 ~ 'yellow',
                                             stationOverallScore == 3 ~ 'green',
                                             stationOverallScore == 4 ~ 'gray')) %>%
             left_join(parameterSTATcrosswalk, by = 'ParameterSTAT') %>%
             left_join(dplyr::select(stationTable, STATION_ID, LATITUDE, LONGITUDE), by = 'STATION_ID') %>%
             ungroup() %>%
             st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
                      remove = T, # remove these lat/lon cols from df
                      crs = 4269) ) # add projection, needs to be geographic for now bc entering lat/lng
  } else {
    return(tibble(STATION_ID = NA, ParameterSTAT = NA, Status = NA, individualColor = NA, 
           individualScore = NA, stationOverallScore = NA, stationColor = NA, Parameter = NA))  }
}

x <- VAHU6stationSummary(stationTable, huc6_filter$VAHU6, parameterSTATcrosswalk) 




# now make a function to plot stations on a map and color by selected variable
if(nrow(x) > 0){
  mapview(x, zcol = 'stationColor', #color = ~stationColor,
          label= x$STATION_ID, layer.name = 'Overall Station Summary',
          popup= leafpop::popupTable(x, zcol=c('STATION_ID')), legend= TRUE) 
  
  # but probably need to make a real map
  
  m <- mapview(x, zcol = 'stationColor', #color = ~stationColor,
               label= x$STATION_ID, layer.name = 'Overall Station Summary',
               popup= leafpop::popupTable(x, zcol=c('STATION_ID')), legend= TRUE) + 
    mapview(huc6_filter(), color = 'yellow',lwd= 5, label= huc6_filter()$VAHU6, layer.name = c('Selected HUC6'),
            popup= leafpop::popupTable(huc6_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG")), legend= FALSE)
  m@map %>% setView(st_bbox(huc6_filter())$xmax[[1]],st_bbox(huc6_filter())$ymax[[1]],zoom = 9) 
  
}
