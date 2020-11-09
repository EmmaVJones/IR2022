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
library(DT)
library(plotly)


# Bring in assessment Functions and app modules
source('appModulesAndFunctions/updatedBacteriaCriteria.R')
source('appModulesAndFunctions/multipleDependentSelectizeArguments.R')
source('appModulesAndFunctions/automatedAssessmentFunctions.R')

modulesToReadIn <- c('temperature','pH')#,'DO','SpCond','Salinity','TN','Ecoli','chlA','Enteroccoci', 'TP','sulfate',
                     #'Ammonia', 'Chloride', 'Nitrate','metals', 'fecalColiform','SSC','Benthics')
for (i in 1:length(modulesToReadIn)){
  source(paste('appModulesAndFunctions/',modulesToReadIn[i],'Module.R',sep=''))
}

# Server connection things
conn <- config::get("connectionSettings") # get configuration settings

# use API key to register board
board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))

# Pull data from server
#conventionals <- pin_get("conventionals2022IRdraft", board = "rsconnect")
#vahu6 <- st_as_sf(pin_get("vahu6", board = "rsconnect")) # bring in as sf object
#conventionals_distinct <- pin_get("conventionals-distinct-draft", board = "rsconnect")
#stations2020IR <- pin_get("stations2020IR-sf-draft", board = "rsconnect")
#VSCIresults <- pin_get("VSCIresults", board = "rsconnect")
#VCPMI63results <- pin_get("VCPMI63results", board = "rsconnect")
#VCPMI65results <- pin_get("VCPMI65results", board = "rsconnect")
#WCmetals <- pin_get("WCmetals-2020IRfinal",  board = "rsconnect")
#Smetals <- pin_get("Smetals-2020IRfinal",  board = "rsconnect")



# Helpful lookup table to ease data filtering
subbasinToVAHU6 <- read_csv('data/subbasinToVAHU6conversion.csv')
historicalStationsTable <- read_csv('data/stationsTable2022begin.csv') # last cycle stations table (forced into new station table format)


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





#### pH Assessment Functions ---------------------------------------------------------------------------------------------------

pH_rangeAssessment <- function(x){
  pH <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,FDT_FIELD_PH,FDT_FIELD_PH_RMK,`pH Min`,`pH Max`)%>% # Just get relevant columns, 
    filter(!(FDT_FIELD_PH_RMK %in% c('Level II', 'Level I'))) %>% # get lower levels out
    filter(!is.na(FDT_FIELD_PH))%>% #get rid of NA's
    rowwise()%>% mutate(interval=findInterval(FDT_FIELD_PH,c(`pH Min`,`pH Max`)))%>% # Identify where pH outside of assessment range
    ungroup()%>%
    mutate(pHrange=ifelse(interval==1,T,F))%>% # Highlight where pH doesn't fall into assessment range
    filter(pHrange==FALSE)%>% # Only return pH measures outside of assessement range
    dplyr::select(-c(interval,pHrange)) # Don't show user interval column, could be confusing to them, T/F in pHrange column sufficient
  return(pH)
}


exceedance_pH <- function(x){
  pH <- dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME,FDT_DEPTH,FDT_FIELD_PH,FDT_FIELD_PH_RMK,`pH Min`,`pH Max`)%>% # Just get relevant columns, 
    filter(!(FDT_FIELD_PH_RMK %in% c('Level II', 'Level I'))) %>% # get lower levels out
    filter(!is.na(FDT_FIELD_PH)) #get rid of NA's
  pH_rangeAssess <- pH_rangeAssessment(x)
  pH_results <- assessmentDetermination(pH, pH_rangeAssess,"pH","Aquatic Life")
  return(pH_results)
}

