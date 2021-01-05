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
    # Gives one "rank" per station
    overall <- vahu6StationSummary %>%
      group_by(STATION_ID, individualColor, individualScore) %>%
      dplyr::summarise(`n Parameters of lowest status` = n()) 
    # join number of ranks causing color info
    overall2 <- overall %>%
      group_by(STATION_ID) %>%
      summarise(stationOverallScore = min(individualScore)) %>%
      left_join(overall, by = c('STATION_ID','stationOverallScore' = 'individualScore')) %>%
      dplyr::select(-individualColor)
    
    return(left_join(vahu6StationSummary, overall2, by = 'STATION_ID') %>%
            mutate(stationColor = case_when(stationOverallScore == 1 ~ 'red',
                                            stationOverallScore == 2 ~ 'yellow',
                                            stationOverallScore == 3 ~ 'green',
                                            stationOverallScore == 4 ~ 'gray'),
                   stationOverallScore = as.factor(stationOverallScore), 
                   `Overall Station Result` = case_when(stationColor == 'red' ~ 'Station contains at least one parameter status of IM or 10.5% Exceedance',
                                                        stationColor == 'yellow' ~ 'Station contains at least one parameter status of IN or Review',
                                                        stationColor == 'green' ~ 'Station contains at least one parameter status of S and no IM, IN, 10.5% Exceedance, or Review',
                                                        stationColor == 'gray' ~ 'Station contains all NA statuses')) %>%
            left_join(parameterSTATcrosswalk, by = 'ParameterSTAT') %>%
            left_join(dplyr::select(stationTable, STATION_ID, LATITUDE, LONGITUDE), by = 'STATION_ID') %>%
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
    return(tibble(STATION_ID = NA, ParameterSTAT = NA, Status = NA, individualColor = NA, 
           individualScore = NA, stationOverallScore = NA, stationColor = NA, Parameter = NA)) }
    #return(tibble(STATION_ID = NA, individualColor = NA, individualScore = NA, n = NA)) }
}

x <- VAHU6stationSummary(stationTable,'JM02', parameterSTATcrosswalk) 



# now make a function to plot stations on a map and color by selected variable
if(nrow(x) > 0){
  pal <- colorFactor(
    palette = c('red', 'yellow','green', 'gray'),
    domain = c(1, 2, 3, 4))
 
  CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE) %>%
    addCircleMarkers(data = x, color='black', fillColor=~pal(x$stationOverallScore), radius = 6,
                     fillOpacity = 0.5,opacity=1,weight = 2,stroke=T,group="Overall Station Summary",
                     label = ~STATION_ID, layerId = ~STATION_ID,
                     popup = leafpop::popupTable(x, zcol=c( "STATION_ID", "Overall Station Result", "n Parameters of lowest status"))  ) %>% 
    addLegend('topright', colors = c('red', 'yellow','green', 'gray'),
              labels = c('Station contains at least one parameter status of IM or 10.5% Exceedance',
                         'Station contains at least one parameter status of IN or Review',
                         'Station contains at least one parameter status of S and no IM, IN, 10.5% Exceedance, or Review', 
                         'Station contains all NA statuses'), title = 'Legend') %>%
    addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                     overlayGroups = c('Overall Station Summary'),
                     options=layersControlOptions(collapsed=T),
                     position='topleft') 
}


  parameter <- 'Ammonia'
  indParameter <- filter(x, Parameter == parameter)
  
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
                labels = c('Station contains at least one parameter<br> status of IM or 10.5% Exceedance',
                           'Station contains at least one parameter<br> status of IN or Review',
                           'Station contains at least one parameter<br> status of S and no IM, IN, 10.5% Exceedance, or Review', 
                           'Station contains all NA statuses'), title = 'Legend') %>%
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
                       popup = leafpop::popupTable(indParameter , zcol=c( "STATION_ID", "Parameter", "Status"))  ) %>% 
      addLegend('topright', colors = c('red', 'yellow','green', 'gray'),
                labels = c('Station status of IM or 10.5% Exceedance',
                           'Station status of IN or Review',
                           'Station status of S', 
                           'Station status of NA'), title = 'Legend') %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c(paste(parameter, "Station Summary")),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')  }
}
  

indStatusMap('Overall Status', x)  

indStatusMap('Ammonia', x)  




ui <- fluidPage(
  helpText('Review each site using the single site visualization section, then 
           proceed to the bottom of the page to find exceedance rate for the entire assessment unit.',br(),
           span(strong('NOTE: The pH exceedance analysis results at the bottom of the page include data
                       from ALL stations within the assessment unit.'))),
  TPPlotlySingleStationUI('TP')     )


server <- function(input,output,session){
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  AUData <- reactive({filter_at(conventionals_HUC, vars(starts_with("ID305B")), any_vars(. %in% AUselection) ) })
  callModule(TPPlotlySingleStation,'TP', AUData, stationSelected)
  
}

shinyApp(ui,server)