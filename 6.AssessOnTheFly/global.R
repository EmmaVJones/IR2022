httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))

library(tidyverse)
library(shiny)
library(shinybusy)
library(leaflet)
library(leaflet.extras)
library(inlmisc)
library(DT)
library(config)
library(sf)
library(plotly)
library(lubridate)
library(geojsonsf)
library(pins)
library(sqldf)
library(dbplyr)

# Server connection things
conn <- config::get("connectionSettings") # get configuration settings


board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))



# Pull latest assessment Run
statewideResults <- pin_get("ejones/statewideResults", board = "rsconnect")


summarizeRuns <- function(stationFieldData){
  stationFieldData %>% 
    mutate(SampleDate = as.Date(Fdt_Date_Time)) %>% 
    group_by(Fdt_Collector_Id, Fdt_Run_Id) %>% 
    summarise(`Times Completed` = length(unique(SampleDate)),
              `Dates Sampled` = paste0(unique(SampleDate), collapse = ', '),
              `Stations Per Run` = length(unique(Fdt_Sta_Id))) }


collectorHeatmap <- function(stationFieldData, stationGIS_View, assessmentLayer, collectorID){
  stations <- filter(stationFieldData, Fdt_Collector_Id %in% collectorID)
  if(nrow(stations) > 0){
    stations <- stations %>% 
      group_by(Fdt_Sta_Id) %>% 
      summarise(`Station Visited` = length(unique(Fdt_Date_Time))) %>% 
      left_join(stationGIS_View, by = c('Fdt_Sta_Id' = 'Station_Id')) %>% 
      st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
               remove = F, # don't remove these lat/lon cols from df
               crs = 4326)
    
    VAHU6 <- filter(assessmentLayer, VAHU6 %in% stations$Huc6_Vahu6)
    
    pal <- colorNumeric(
      palette = "Reds",
      domain = stations$`Station Visited`)
    
    CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE,
                 options= leafletOptions(zoomControl = TRUE,minZoom = 3, maxZoom = 20,
                                         preferCanvas = TRUE)) %>%
      #setView(-79.1, 37.7, zoom=7)  %>%
      addPolygons(data= VAHU6,  color = 'gray', weight = 1,
                  fillColor= 'yellow', fillOpacity = 0.5,stroke=0.1,
                  group="VAHU6 sampled",label = ~VAHU6) %>% 
      addCircleMarkers(data = stations, color = 'black', fillColor = ~pal(stations$`Station Visited`),
                       radius = 3, fillOpacity = 1, weight = 1,stroke=T, label = ~Fdt_Sta_Id, group = 'Stations Sampled',
                       popup = leafpop::popupTable(stations, zcol=c('Fdt_Sta_Id', 'Station Visited'))) %>% 
      addLegend(data = stations,'topright', pal = pal, values = ~`Station Visited`, title = 'Number of <br>Station Visits') %>% 
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c('Stations Sampled', "VAHU6 sampled"),
                       options=layersControlOptions(collapsed=T),
                       position='topleft') 
    
    
  } else { return(NULL)}
}


# parameter name crosswalk for prettier names later
parameterEXCcrosswalk <- tibble(Parameter = c("Temperature", 'Dissolved Oxygen', 'pH', 'E.coli STV', 'E.coli Geomean', 'Enterococci STV',  'Enterococci Geomean',
                                              'Ammonia', 'Water Column Metals',
                                              'Water Column Toxics', 'Sediment Metals', 'Sediment Toxics', 'Fish Tissue Metals', 'Fish Tissue Toxics',
                                              'Benthics', 'Total Phosphorus', 'Chlorophyll a'),
                                ParameterEXC = c("TEMP_EXC", "DO_EXC", "PH_EXC", "ECOLI_EXC", "ECOLI_GM_EXC ","ENTER_EXC", "ENTER_GM_EXC",
                                                 "AMMONIA_EXC", "WAT_MET_EXC", 
                                                 "WAT_TOX_EXC", "SED_MET_EXC", "SED_TOX_EXC", "FISH_MET_EXC", "FISH_TOX_EXC", "BENTHIC_EXC",
                                                 "NUT_TP_EXC", "NUT_CHLA_EXC"))
stationSummary <- function(stationTableResults, parameterSTATcrosswalk){
  #vahu6StationSummary <- filter(stationTable, VAHU6 %in% VAHU6chosen) 
  
  
  if(nrow(stationTableResults) > 0){
    stationTableResults1 <- stationTableResults %>%
      dplyr::select(STATION_ID, contains('_EXC')) %>% 
      group_by(STATION_ID) %>%
      pivot_longer(-STATION_ID, names_to = 'ParameterEXC', values_to = 'Exceedance') %>%
      mutate(individualColor = case_when(Exceedance > 1 ~ 'red',
                                         Exceedance == 1 ~ 'yellow',
                                         Exceedance == 0 ~ 'green',
                                         is.na(Exceedance) ~ 'gray'),
             # create a variable that ranks statuses to easily combine into single "score"
             individualScore = case_when(individualColor == 'red' ~ 1,
                                         individualColor == 'yellow' ~ 2,
                                         individualColor == 'green' ~ 3,
                                         individualColor == 'gray' ~ 4))
    # Gives one "rank" per station
    overall <- stationTableResults1 %>%
      group_by(STATION_ID, individualColor, individualScore) %>%
      dplyr::summarise(`n Parameters of lowest status` = n()) 
    # join number of ranks causing color info
    overall2 <- overall %>%
      group_by(STATION_ID) %>%
      summarise(stationOverallScore = min(individualScore)) %>%
      left_join(overall, by = c('STATION_ID','stationOverallScore' = 'individualScore')) %>%
      dplyr::select(-individualColor)
    
    return(left_join(stationTableResults1, overall2, by = 'STATION_ID') %>%
             mutate(stationColor = case_when(stationOverallScore == 1 ~ 'red',
                                             stationOverallScore == 2 ~ 'yellow',
                                             stationOverallScore == 3 ~ 'green',
                                             stationOverallScore == 4 ~ 'gray'),
                    stationOverallScore = as.factor(stationOverallScore), 
                    `Overall Station Result` = case_when(stationColor == 'red' ~ 'Station contains at least one parameter with 2 or more exceedances',
                                                         stationColor == 'yellow' ~ 'Station contains at least one parameter with one exceedance',
                                                         stationColor == 'green' ~ 'Station contains no exceedances',
                                                         stationColor == 'gray' ~ 'Station unassessed')) %>%
             left_join(parameterEXCcrosswalk, by = 'ParameterEXC' ) %>%
             left_join(dplyr::select(stationTableResults, STATION_ID, LATITUDE, LONGITUDE), by = 'STATION_ID') %>%
             ungroup() %>%
             st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
                      remove = T, # remove these lat/lon cols from df
                      crs = 4326) ) # add projection, needs to be geographic for now bc entering lat/lng
    
    # Gives ranked scale of n statuses
    #  overall <- vahu6StationSummary %>%
    #    group_by(STATION_ID, individualColor, individualScore) %>%
    #    dplyr::summarise(n = n()) %>%
    #    left_join(dplyr::select(stationTable, STATION_ID, LATITUDE, LONGITUDE), by = 'STATION_ID') %>%
    #    ungroup() %>%
    #    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
    #             remove = T, # remove these lat/lon cols from df
    #             crs = 4269)  # add projection, needs to be geographic for now bc entering lat/lng
    #  
    #return(overall)
  } else {
    return(tibble(STATION_ID = NA, ParameterEXC = NA, Status = NA, individualColor = NA, 
                  individualScore = NA, stationOverallScore = NA, stationColor = NA, Parameter = NA)) }
  #return(tibble(STATION_ID = NA, individualColor = NA, individualScore = NA, n = NA)) }
}

# assessmentSummary <- stationSummary(stationTableResults = left_join(assessmentResults$stationTableResults,
#                                                                     dplyr::select(stationGIS_View, STATION_ID = Station_Id, LATITUDE = Latitude, LONGITUDE = Longitude),
#                                                                     by = 'STATION_ID'),
#                                     parameterEXCcrosswalk)

# Station Status Map Function
indStatusMap <- function(parameter, status){
  pal <- colorFactor(
    palette = c('red', 'yellow','green', 'gray'),
    domain = c(1, 2, 3, 4))
  
  if(parameter == 'Overall Status'){
    CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE) %>%
      addCircleMarkers(data = status, color='black', fillColor=~pal(status$stationOverallScore), radius = 6,
                       fillOpacity = 0.5,opacity=1,weight = 2,stroke=T,group="Overall Station Status Summary",
                       label = ~STATION_ID, layerId = ~STATION_ID,
                       popup = leafpop::popupTable(status, zcol=c( "STATION_ID", "Overall Station Result", "n Parameters of lowest status"))  ) %>% 
      addLegend('topright', colors = c('red', 'yellow','green', 'gray'),
                labels = c('Station contains at least <br>one parameter with 2 or more exceedances',
                           'Station contains at least <br>one parameter with one exceedance',
                           'Station contains no exceedances',
                           'Station unassessed'), title = 'Legend') %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c('Overall Station Status Summary'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft') 
  } else {
    indParameter <- filter(status, Parameter %in% parameter)
    
    CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE) %>%
      addCircleMarkers(data = indParameter , color='black', fillColor=~pal(indParameter$individualScore), radius = 6,
                       fillOpacity = 0.5,opacity=1,weight = 2,stroke=T,group=paste(parameter, "Station Summary"),
                       label = ~STATION_ID, layerId = ~STATION_ID,
                       popup = leafpop::popupTable(indParameter , zcol=c( "STATION_ID", "Parameter", "Exceedance"))  ) %>% 
      addLegend('topright', colors = c('red', 'yellow','green', 'gray'),
                labels = c('Station has 2 or more exceedances',
                           'Station has one exceedance',
                           'Station has no exceedances', 
                           'Station unassessed'), title = 'Legend') %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c(paste(parameter, "Station Summary")),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')  }
}

# indStatusMap('Overall Status',assessmentSummary)
# indStatusMap('pH',assessmentSummary)


monthlyBreakdown <- function(stationFieldData, byWhat){
  if(byWhat == 'Run ID'){
    z <- stationFieldData %>% 
      mutate(SampleMonth = month(Fdt_Date_Time, label = T, abbr = F)) %>% 
      dplyr::select(SampleMonth, Fdt_Run_Id, Fdt_Collector_Id) %>% 
      arrange(SampleMonth, Fdt_Run_Id) %>% 
      group_by(Fdt_Run_Id, SampleMonth) %>% distinct() 
  } else {
    z <- stationFieldData %>% 
      mutate(SampleMonth = month(Fdt_Date_Time, label = T, abbr = F)) %>% 
      dplyr::select(SampleMonth, Fdt_Sta_Id, Fdt_Collector_Id) %>% 
      arrange(SampleMonth, Fdt_Sta_Id) %>% 
      group_by(Fdt_Sta_Id, SampleMonth) %>% distinct()  }
  
  return(z %>% pivot_wider(names_from = SampleMonth, values_from = Fdt_Collector_Id, names_sep = ', ') )
  
}
