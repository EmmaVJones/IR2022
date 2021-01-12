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

modulesToReadIn <- c('thermocline')#,'temperature','pH','DO','Ecoli', 'Enteroccoci','SpCond','salinity','TN','chlA','Enteroccoci', 'TP','sulfate','Ammonia', 
#                     'Chloride', 'Nitrate','metals', 'fecalColiform','SSC','Benthics')
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

