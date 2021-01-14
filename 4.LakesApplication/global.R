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


# Bring in assessment Functions and app modules
source('appModulesAndFunctions/updatedBacteriaCriteria.R')
source('appModulesAndFunctions/multipleDependentSelectizeArguments.R')
source('appModulesAndFunctions/automatedAssessmentFunctions.R')

modulesToReadIn <- c('thermocline','temperature','DO','pH', 'Ecoli', 'Ecoli_AU','chlA')#'TN','chlA','TP','sulfate','Ammonia', 
#                     'Chloride', 'Nitrate','metals', 
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

# Helpful lookup table to ease data filtering
subbasinToVAHU6 <- read_csv('data/subbasinToVAHU6conversion.csv') 


# this is a placeholder until final 2020 stations database is released
#historicalStationsTable <- st_read('data/GIS/2020_wqms.shp') %>%
#  st_drop_geometry()#read_csv('data/stationsTable2022begin.csv') # last cycle stations table (forced into new station table format)
historicalStationsTable2 <- read_excel('data/tbl_ir_mon_stations2018IRfinal.xlsx')




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


