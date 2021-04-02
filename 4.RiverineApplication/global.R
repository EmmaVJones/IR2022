library(tidyverse)
library(shiny)
library(shinyjs)
library(sf)
library(pins)
library(EnvStats)
library(lubridate)
library(config)
library(leaflet)
library(mapview)
library(inlmisc)
library(DT)
library(plotly)
library(readxl)
library(round)


# Bring in assessment Functions and app modules
source('appModulesAndFunctions/updatedBacteriaCriteria.R')
source('appModulesAndFunctions/multipleDependentSelectizeArguments.R')
source('appModulesAndFunctions/automatedAssessmentFunctions.R')

modulesToReadIn <- c('temperature','pH','DO','Ecoli', 'Enteroccoci','SpCond','salinity','TN','chlA','Enteroccoci', 'TP','sulfate','Ammonia', 
                     'Chloride', 'Nitrate','metals', 'fecalColiform','SSC','Benthics', 'toxics')
for (i in 1:length(modulesToReadIn)){
  source(paste('appModulesAndFunctions/',modulesToReadIn[i],'Module.R',sep=''))
}

# Server connection things
conn <- config::get("connectionSettings") # get configuration settings

# use API key to register board
board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))

template <- read_csv('userDataToUpload/processedStationData/stationTableResults.csv')
lastUpdated <- as.Date(file.info('userDataToUpload/processedStationData/stationTableResults.csv')$mtime)
# markPCB <- read_excel('data/2022 IR PCBDatapull_EVJ.xlsx', sheet = '2022IR Datapull EVJ')
# fishPCB <- read_excel('data/FishTissuePCBsMetals_EVJ.xlsx', sheet= 'PCBs')
# fishMetals <- read_excel('data/FishTissuePCBsMetals_EVJ.xlsx', sheet= 'Metals')



# Helpful lookup table to ease data filtering
subbasinToVAHU6 <- read_csv('data/subbasinToVAHU6conversion.csv') 


# this is a placeholder until final 2020 stations database is released
#historicalStationsTable <- st_read('data/GIS/2020_wqms.shp') %>%
#  st_drop_geometry()#read_csv('data/stationsTable2022begin.csv') # last cycle stations table (forced into new station table format)
historicalStationsTable2 <- read_excel('data/tbl_ir_mon_stations2018IRfinal.xlsx')



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
                labels = c('Station contains at least <br>one parameter status of <br>IM or 10.5% Exceedance',
                           'Station contains at least <br>one parameter status of <br>IN or Review',
                           'Station contains at least <br>one parameter status of S <br>and no IM, IN, <br>10.5% Exceedance, or Review', 
                           'Station contains all <br>NA statuses'), title = 'Legend') %>%
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


# Loading screen
load_data <- function() {
  Sys.sleep(2)
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}



withinAssessmentPeriod <- function(x){
  if((range(unique(x$FDT_DATE_TIME))[1] < assessmentPeriod[1]) | 
     (range(unique(x$FDT_DATE_TIME))[2] > assessmentPeriod[2])){
    return('Data included that falls outside of assessment period. Review input data.')
  }else{return('All input data falls within the assessment period.')}
}


# change WQS for a given module
changeWQSfunction <- function(stationData,  # single station dataset
                              inputCLASS_DESCRIPTION){ # from user module)
  WQSvalues <- bind_rows(WQSvalues, 
                         tribble(
                           ~`pH Min`, ~`pH Max`, ~CLASS_DESCRIPTION, 
                           6.5, 9.5, 'SPSTDS = 6.5-9.5'))
  if(inputCLASS_DESCRIPTION != unique(stationData$CLASS_DESCRIPTION)){
    changedWQS <- filter(WQSvalues, CLASS_DESCRIPTION %in% inputCLASS_DESCRIPTION)
    return(dplyr::select(stationData, -c(`Description Of Waters`:CLASS_DESCRIPTION)) %>%
             mutate(CLASS = changedWQS$CLASS, 
                    `Description Of Waters` = changedWQS$`Description Of Waters` ) %>%
             left_join(changedWQS, by = c('CLASS', 'Description Of Waters'))) 
  } else {return(stationData)} 
}




## Old bacteria methods just hanging on
 
bacteria_ExceedancesSTV_OLD <- function(x, STVlimit){                                    
  x %>% rename(parameter = !!names(.[2])) %>% # rename columns to make functions easier to apply
    mutate(limit = STVlimit, exceeds = ifelse(parameter > limit, T, F)) # Single Sample Maximum 
}

bacteria_ExceedancesGeomeanOLD <- function(x, bacteriaType, geomeanLimit){
  if(nrow(x) > 0){
    suppressWarnings(mutate(x, SampleDate = format(FDT_DATE_TIME,"%m/%d/%y"), # Separate sampling events by day
                            previousSample=lag(SampleDate,1),
                            previousSampleBacteria=lag(!! bacteriaType,1)) %>% # Line up previous sample with current sample line
                       rowwise() %>% 
                       mutate(sameSampleMonth= as.numeric(strsplit(SampleDate,'/')[[1]][1])  -  as.numeric(strsplit(previousSample,'/')[[1]][1])) %>% # See if sample months are the same, e.g. more than one sample per calendar month
                       filter(sameSampleMonth == 0 | is.na(sameSampleMonth)) %>% # keep only rows with multiple samples per calendar month  or no previous sample (NA) to then test for geomean
                       # USING CALENDAR MONTH BC THAT'S HOW WRITTEN IN GUIDANCE, rolling 4 wk windows would have been more appropriate
                       mutate(sampleMonthYear = paste(month(as.Date(SampleDate,"%m/%d/%y")),year(as.Date(SampleDate,"%m/%d/%y")),sep='/')) %>% # grab sample month and year to group_by() for next analysis
                       group_by(sampleMonthYear) %>%
                       mutate(geoMeanCalendarMonth =  EnvStats::geoMean(as.numeric(get(bacteriaType)), na.rm = TRUE), # Calculate geomean
                              limit = geomeanLimit, samplesPerMonth = n()))
  }
}
# How bacteria is assessed
bacteria_Assessment_OLD <- function(x, bacteriaType, geomeanLimit, STVlimit){
  if(nrow(x)>1){
    bacteria <- dplyr::select(x,FDT_DATE_TIME, !! bacteriaType)%>% # Just get relevant columns, 
      filter(!is.na(!!bacteriaType)) #get rid of NA's
    # Geomean Analysis (if enough n)
    if(nrow(bacteria)>0){
      bacteriaGeomean <- bacteria_ExceedancesGeomeanOLD(bacteria, bacteriaType, geomeanLimit) %>%     
        distinct(sampleMonthYear, .keep_all = T) %>%
        filter(samplesPerMonth > 4, geoMeanCalendarMonth > limit) %>% # minimum sampling rule for geomean to apply
        mutate(exceeds = TRUE) %>%
        dplyr::select(sampleMonthYear, geoMeanCalendarMonth, limit, exceeds, samplesPerMonth)
      geomeanResults <- quickStats(bacteriaGeomean, bacteriaType) %>%
        mutate(`Assessment Method` = 'Old Monthly Geomean')
      geomeanResults[,4] <- ifelse(is.na(geomeanResults[,4]),NA, dplyr::recode(geomeanResults[,4], 'Review' = paste('Review if ', bacteriaType,'_VIO > 1',sep='')))
      
      # Single Sample Maximum Analysis
      bacteriaSSM <- bacteria_ExceedancesSTV_OLD(bacteria, STVlimit) 
      SSMresults <- quickStats(bacteriaSSM, bacteriaType) %>% mutate(`Assessment Method` = 'Old Single Sample Maximum')
      return( rbind(geomeanResults, SSMresults) )
    }
  }
  
}







# Super Assessment function
assessmentDetermination <- function(parameterDF,parameterAssessmentDF,parameter,use){
  
  results <- data.frame(nSamples = nrow(parameterDF),nExceedance = nrow(parameterAssessmentDF))%>%
    mutate(exceedanceRate = (nExceedance/nSamples)*100)
  
  if(results$exceedanceRate > 10.5 & results$nSamples > 10){outcome <- paste('Water impaired for',parameter)}
  if(results$exceedanceRate < 10.5 & results$nSamples > 10){outcome <- paste('Water not impaired for',parameter)}
  if(results$nExceedance >= 2 & results$nSamples < 10){outcome <- paste('Water impaired for',parameter)}
  if(results$nExceedance < 2 & results$nSamples < 10){outcome <- paste('Water not impaired for',parameter)}
  
  results <- mutate(results,Assessment=outcome, Use= use)
  return(results)
}
#assessmentDetermination(temp,temp_Assess,"temperature","Aquatic Life")




## Benthics metrics
benthicResultsMetrics <- function(x, SCIresults, wadeableOnly, rep1Only){
  z <- filter(SCIresults, StationID %in% unique(x$FDT_STA_ID)) %>%
    filter(`Target Count` == 110) %>% # removing non rarified samples takes care of removing QA samples
    {if(wadeableOnly)
      filter(., Gradient != 'Boatable')
      else .} %>%
    {if(rep1Only)
      filter(., RepNum == 1)
      else .}
  out <- list()
  if(nrow(z) > 0){
    z1 <- mutate(z, Year = lubridate::year(`Collection Date`))
    spring <- filter(z1, Season %in% 'Spring' )
    fall <- filter(z1, Season %in% 'Fall' )
    # output list with all metrics
    out$data <- z1
    out$roundup <- z1 %>%
      group_by(StationID) %>%
      summarise(`n Samples` = n(),
                `Average SCI` = round(mean(`SCI Score`), digits = 0),
                `Minimum SCI` = round(min(`SCI Score`), digits = 0),
                `Maximum SCI` = round(max(`SCI Score`), digits = 0)) %>%
      bind_cols(spring %>%
                  summarise(`n Spring Samples` = n(),
                            `Spring Average SCI` = round(mean(`SCI Score`), digits = 0))) %>%
      bind_cols(fall %>%
                  summarise(`n Fall Samples` = n(),
                            `Fall Average SCI` = round(mean(`SCI Score`), digits = 0)))
    
    out$yearlyAverage <- z1 %>%
      group_by(Year) %>%
      summarise(`Yearly Average` = round(mean(`SCI Score`), digits = 0)) 
  } else {
    out$data <- NA
    out$roundup <- tibble(StationID = NA,  
                          `n Samples`=NA, `Average SCI` =NA, `Minimum SCI` = NA, `Maximum SCI`= NA,
                          `n Spring Samples`= NA, `Spring Average SCI`=NA, 
                          `n Fall Samples` = NA, `Fall Average SCI`= NA)
    out$yearlyAverage <- tibble(Year= NA, `Yearly Average`=NA)  }
  return(out)
}
#benthicResultsMetrics(x, VCPMI63results, TRUE, TRUE)

SCIchooser <- function(x){
  if(unique(x$EPA_ECO_US_L3CODE) %in% 63 | 
     str_detect(unique(x$Basin_Code), 'Chowan')){return('VCPMI 63 + Chowan')}
  if(unique(x$EPA_ECO_US_L3CODE)  %in% 65  & 
     !str_detect(unique(x$Basin_Code), 'Chowan')){return('VCPMI 65 - Chowan')}
  if(unique(x$EPA_ECO_US_L3CODE) %in% c(NA, 45, 64, 66, 67, 69)){return('VSCI')}
}
#SCIchooser(x)



## functions for Bioassessment use
# Template to standardize variables for DT habitat heatmap across high and low gradients
habitatTemplate <- tibble(StationID = NA, HabSampID = NA, `Collection Date` = NA, `HabSample Comment` = NA, `Total Habitat Score` = NA, `Bank Stability` = NA, 
                          `Channel Alteration` = NA, `Channel Flow Status` = NA, `Channel Sinuosity` = NA, Embeddedness = NA, 
                          `Epifaunal Substrate / Available Cover` = NA, `Pool Substrate Characterization` = NA, `Pool Variability` = NA, 
                          `Frequency of riffles (or bends)` = NA, `Riparian Vegetative Zone Width` = NA, `Sediment Deposition` = NA, 
                          `Vegetative Protection` = NA, `Velocity / Depth Regime` = NA)
SCItemplate <- tibble(StationID = NA, Sta_Desc = NA, BenSampID = NA, `Collection Date` = NA, RepNum = NA, `Family Total Taxa` = NA, `Family EPT Taxa` = NA,      
                      `%Ephem` = NA, `%PT - Hydropsychidae` = NA, `%FamilyScraper` = NA, `%Chiro` = NA, `Family %2 Dominant` = NA, `Family HBI` = NA, `%Ephem Score` = NA,         
                      `%PT-H Score` = NA, `Fam Richness Score` = NA, `%Chironomidae Score` = NA, `Fam EPT Score` = NA, `Fam %Scraper Score` = NA, `Fam %2Dom Score` = NA, `Fam %MFBI Score` = NA,      
                      `SCI Score` = NA, SCI = NA, `SCI Threshold` = NA, `Sample Comments` = NA, `Collected By` = NA, `Field Team` = NA, `Entered By` = NA,           
                      Taxonomist = NA, `Entered Date` = NA, Gradient = NA, `Target Count` = NA, Season = NA, `Family %5 Dominant` = NA, `%ClngP-HS` = NA,            
                      `Richness Score` = NA, `Richness Final` = NA, `HBI Score` = NA, `HBI Final` = NA, `EPT Score` = NA, `EPT Final` = NA, EPHEM = NA,                
                      `PT-H` = NA, `Pct5DOM` = NA, `PctClng-HS` = NA, `%Scrap` = NA, `%Intoler` = NA, PctScrap = NA, PctIntol = NA,             
                      US_L3CODE = NA, US_L3NAME = NA, HUC_12 = NA, VAHU6 = NA, Basin = NA, Basin_Code = NA)



## Reorganize habitat data efficiently
habitatConsolidation <- function( userStationChoice, habSamps, habValues){
  habSampsUserSelection <- filter(habSamps, StationID %in% userStationChoice) 
  habValuesUserSelection <- filter(habValues, HabSampID %in% habSampsUserSelection$HabSampID)
  totalHabitat <- habSampsUserSelection %>%
    group_by(HabSampID) %>%
    # get total habitat values
    left_join(totalHabScore(habValuesUserSelection), by = 'HabSampID') %>%
    mutate(Season = factor(Season,levels=c("Spring","Outside Sample Window","Fall"))) %>%
    dplyr::select(StationID, HabSampID, everything()) %>%
    arrange(`Collection Date`) %>%
    ungroup() 
  
  habitatCrosstab <- bind_rows(habitatTemplate,
                               left_join(habValuesUserSelection, 
                                         dplyr::select(habSampsUserSelection, HabSampID, StationID, `Collection Date`),
                                         by = 'HabSampID') %>%
                                 group_by(StationID, HabSampID, `Collection Date`) %>%
                                 arrange(HabParameterDescription) %>% ungroup() %>%
                                 pivot_wider(id_cols = c('StationID','HabSampID','Collection Date'), names_from = HabParameterDescription, values_from = HabValue) %>%
                                 left_join(dplyr::select(totalHabitat, HabSampID, `HabSample Comment`, `Total Habitat Score`), by = 'HabSampID') %>%
                                 dplyr::select(StationID, HabSampID, `Collection Date`, `HabSample Comment`, `Total Habitat Score`, everything()) ) %>%
    drop_na(StationID) %>%
    arrange(StationID, `Collection Date`) 
  return(habitatCrosstab)
}

totalHabScore <- function(habValues){
  habValues %>%
    group_by(HabSampID) %>%
    summarise(`Total Habitat Score` = sum(HabValue, na.rm = T))
}


# Raw Bug data results for Report
rawBugData <- function(SCI){
  bioResultsTableTemplate <- tibble(StationID = NA, `Collection Date` = NA, `Replicate Number` = NA, Gradient = NA, SCI = NA, 
                                    `Spring SCI Score` = NA, `Fall SCI Score` = NA)
  bind_rows(bioResultsTableTemplate, 
            SCI %>%
              group_by(StationID, `Collection Date`, SCI, RepNum, Gradient) %>%
              dplyr::select(StationID, `Collection Date`, `Replicate Number` = RepNum, Gradient, SCI, Season, `SCI Score`) %>%
              mutate(`Collection Date` = as.Date(`Collection Date`),#, format = '%M-%D-%Y'),
                     Season = paste0(Season, ' SCI Score')) %>%
              pivot_wider(names_from = Season, values_from = `SCI Score`) ) %>%
    drop_na(StationID) %>%
    arrange(`Collection Date`, `Replicate Number`)
}

# SCI Statistics for report
SCIstatistics <- function(SCI1){
  suppressMessages(suppressWarnings(
    SCI1 %>%
      # IR window Average
      group_by(StationID, SCI) %>%
      summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits = 3),
                `n Samples` = n()) %>% 
      mutate(Window = paste0('IR ', assessmentCycle, ' (6 year) Average'))  %>%
      dplyr::select(SCI, Window, `SCI Average`, `n Samples`) %>%
      # Two Year Average
      bind_rows(SCI1 %>%
                  filter(year(`Collection Date`) %in% c(2019, 2020)) %>%
                  group_by(StationID, SCI) %>%
                  summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits=3),
                            `n Samples` = n()) %>% ungroup() %>%
                  mutate(Window = as.character('2019-2020 Average')) %>% 
                  dplyr::select(StationID, SCI, Window, everything())) %>%
      # Two Year Spring Average
      bind_rows(SCI1 %>%
                  filter(year(`Collection Date`) %in% c(2019, 2020) & Season == 'Spring') %>%
                  group_by(StationID, SCI) %>%
                  summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits=3),
                            `n Samples` = n()) %>% ungroup() %>%
                  mutate(Window = as.character('2019-2020 Spring Average')) %>% 
                  dplyr::select(StationID, SCI, Window, everything())) %>%
      # Two Year Fall Average
      bind_rows(SCI1 %>%
                  filter(year(`Collection Date`) %in% c(2019, 2020) & Season == 'Fall') %>%
                  group_by(StationID, SCI) %>%
                  summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits=3),
                            `n Samples` = n()) %>% ungroup() %>%
                  mutate(Window = as.character('2019-2020 Fall Average')) %>% 
                  dplyr::select(StationID, SCI, Window, everything())) %>%
      # Add seasonal averages
      bind_rows(SCI1 %>%
                  group_by(StationID, SCI, Season) %>%
                  mutate(Season = paste0('IR ', assessmentCycle, ' (6 year) ', Season,' Average')) %>%
                  summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits = 3),
                            `n Samples` = n()) %>%
                  rename('Window' = 'Season') %>% ungroup()) %>%
      mutate(Window = factor(Window, levels = c('2019-2020 Average', '2019-2020 Spring Average', '2019-2020 Fall Average', 'IR 2022 (6 year) Average', 
                                                'IR 2022 (6 year) Spring Average', 'IR 2022 (6 year) Fall Average'))) %>%
      arrange(StationID, SCI, Window)  %>% ungroup() ) )
}


# SCI plot for report
SCIresultsPlot <- function(SCI, assessmentMethod){
  if(unique(assessmentMethod) == 'VSCI'){
    mutate(SCI, `Collection Date` = as.Date(`Collection Date`)) %>% 
      ggplot(aes(x = `Collection Date`, y = `SCI Score`, fill=Season)) +
      geom_col()+
      scale_fill_manual("Season", values = c("Fall" = "black", "Spring" = "dark grey"))+
      labs(x="Collection Year", y="VSCI Score") +
      scale_y_continuous(#name="VSCI", 
        breaks=seq(0, 100, 10),limits=c(0,100)) +
      scale_x_date(date_labels = '%Y') +
      geom_hline(yintercept=60, color="red", size=1)+
      theme(axis.text.x=element_text(angle=45,hjust=1))
  } else {
    mutate(SCI, `Collection Date` = as.Date(`Collection Date`)) %>% 
      ggplot(aes(x = `Collection Date`, y = `SCI Score`, fill=Season)) +
      geom_col()+
      scale_fill_manual("Season", values = c("Fall" = "black", "Spring" = "dark grey"))+
      labs(x="Collection Year", y="VCPMI Score") +
      scale_y_continuous(#name="VSCI", 
        breaks=seq(0, 100, 10),limits=c(0,100)) +
      scale_x_date(date_labels = '%Y') +
      geom_hline(yintercept=40, color="red", size=1)+
      theme(axis.text.x=element_text(angle=45,hjust=1))
  }
}

# SCI metrics table for report
SCImetricsTable <- function(SCI){
  SCI %>%
    mutate(`Collection Date` = as.Date(`Collection Date`)) %>% 
    group_by(StationID, `Collection Date`, SCI, RepNum) %>%
    dplyr::select(StationID, `Collection Date`, `Replicate Number` = RepNum, Season, SCI, `SCI Score`,`Family Total Taxa`:`Fam %MFBI Score`, 
                  `Family %5 Dominant`:PctIntol) %>%
    #clean up empty columns with a quick pivot longer (with drop na) and then back to wide
    pivot_longer(cols = `Family Total Taxa`:PctIntol, names_to = 'metric', values_to = 'metricVal', values_drop_na = TRUE) %>%
    pivot_wider(names_from = metric, values_from = metricVal) %>% ungroup() %>% 
    arrange(`Collection Date`, `Replicate Number`)
}


## Habitat plot for report
habitatPlot <- function(habitat){
  if(nrow(habitat) > 0){
    minDate <- as.Date(as.character("2015-01-01") , origin ="%Y-%m-%d")
    maxDate <- as.Date(as.character("2020-12-31"), origin ="%Y-%m-%d")# add min and max dates to make rectagle plotting easier, starting at 6 month buffer by can play with
    
    habitat %>%
      mutate(`Collection Date` = as.Date(`Collection Date`)) %>% 
      ggplot(aes(x = `Collection Date`, y = `Total Habitat Score`))+
      #geom_bar(stat="identity")
      annotate("rect", xmin=minDate, xmax=maxDate, ymin=150 ,  ymax=Inf, alpha=1, fill="#0072B2")+ 
      annotate("rect",xmin=minDate, xmax=maxDate, ymin=130, ymax=150, alpha=1, fill="#009E73" ) +
      annotate("rect",xmin=minDate, xmax=maxDate, ymin=100, ymax=130, alpha=1, fill="#F0E442") +
      annotate("rect",xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=100, alpha=1, fill="firebrick" ) +
      geom_bar(stat="identity", width = 75)+
      theme(axis.text=element_text(size=14, face="bold"),
            axis.title=element_text(size=14, face="bold"),
            legend.position = "none") +
      scale_y_continuous(name="Total Habitat Score", breaks=seq(0, 200, 25),limits=c(0,200)) +
      scale_x_date(date_breaks='1 year', date_labels =  "%Y")+
      theme(axis.text.x=element_text(angle=45,hjust=1))  }
}

# Habitat Table for final Report
habitatDTcoloredTable <- function(habitat){
  if(nrow(habitat) > 0){
    habitatTable <- habitat %>%
      mutate(`Collection Date` = as.Date(`Collection Date`)) %>% 
      dplyr::select(-HabSampID) %>%
      #clean up empty columns with a quick pivot longer (with drop na) and then back to wide
      pivot_longer(cols = `Bank Stability`:`Velocity / Depth Regime`, names_to = 'metric', values_to = 'metricVal', values_drop_na = TRUE) %>%
      pivot_wider(names_from = metric, values_from = metricVal) %>% ungroup() %>% 
      arrange(`Collection Date`)
    
    habBreaks<-seq(0,20, 1)
    habClrs<-c('firebrick', 'firebrick','firebrick','firebrick','firebrick','firebrick', "#F0E442","#F0E442","#F0E442","#F0E442","#F0E442", 
               "#009E73","#009E73","#009E73","#009E73","#009E73", "#0072B2","#0072B2","#0072B2","#0072B2","#0072B2")
    
    DT::datatable(habitatTable, escape=F, rownames = F, options=list(pageLength=nrow(habitatTable),dom= 'Bt', scrollX=TRUE)) %>% 
      formatStyle('Total Habitat Score', backgroundColor = "lightgray") %>%
      formatStyle(names(habitatTable)[5:length(habitatTable)],  backgroundColor = styleEqual(habBreaks, habClrs), alpha=0.1,
                  textAlign = 'center')  }
}
