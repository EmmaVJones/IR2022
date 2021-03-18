library(tidyverse)
library(readxl)
library(sf)
library(shiny)
library(shinyBS)
library(mapview)
library(leaflet)
library(inlmisc)
library(DT)
library(writexl)
library(shinycssloaders)
library(shinyjqui)

# WQS layer type to WQS_ID conversion
#st_layers('GIS/WQS_layers_05082020.gdb')
WQSlayerConversion <- tibble(waterbodyType = c('Riverine','Lacustrine','Estuarine','Estuarine'),
                             WQS_ID = c('RL','LP','EP','EL')) 




basinCodesConversion <- read_csv('data/basinCodeConversion.csv') %>%
  filter(BASIN != 7) %>%
  bind_rows(data.frame(BASIN = '7D', Basin_Code = 'Small Coastal'))


# Attach SUBBASIN info to appropriate assessment Region
#basinAssessmentRegion <- st_intersection(basin7, assessmentRegions) %>%
#  st_drop_geometry() %>%
#  left_join(mutate(basinCodesConversion, BASIN_CODE = BASIN), by="BASIN_CODE")
#write.csv(basinAssessmentRegion, 'data/basinAssessmentRegion.csv')
#basinAssessmentRegion <- read_csv('data/basinAssessmentReg_clb.csv') %>% # Cleo QAed verison
#  filter(VAHU6_NOTE !=  "NOT IN THIS REGION") %>%
#  left_join(mutate(basinCodesConversion, BASIN_CODE = BASIN), by="BASIN_CODE")

subbasinOptionsByWQStype <- read_csv('data/subbasinOptionsByWQStype&Region.csv') %>%
#  left_join(WQSlayerConversion, by = c('WQS_ID_Prefix' = 'WQS_ID', 'waterbodyType')) %>%
  left_join(basinCodesConversion, by = c('SubbasinOptions' = 'BASIN')) #%>%
# 7- is left out now but need more help from estuarine folks to sort this out properly
  # bind_rows(tibble(waterbodyType = c('Estuarine', 'Estuarine','Estuarine', 'Estuarine', 'Estuarine', 'Estuarine'), 
  #                  SubbasinOptions = c('7-', '7-', '7-', '7-', '7-', '7-'), 
  #                  AssessmentRegion = c('CO', 'CO', 'TRO', 'TRO','PRO','PRO'), 
  #                  WQS_ID_Prefix = c('EP', "EL", 'EP', "EL", 'EP', "EL"), 
  #                  Basin_Code = c('Small Coastal', 'Small Coastal', 'Small Coastal', 'Small Coastal', 'Small Coastal', 'Small Coastal'))) %>%
  # arrange(SubbasinOptions)




# Persistent data storage on server
#outputDir <- "WQSlookupTable" # location on server to save data


saveData <- function(data,outputDir) {
  #data <- t(data)
  # Create a unique file name
  if(outputDir == 'WQSlookupTable'){
    fileName <- sprintf("%s_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S"), 'WQSlookup')
  } else {
    fileName <- sprintf("%s_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S"), 'AUlookup')
  }
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName), 
    row.names = FALSE, quote = TRUE
  )
}

loadData <- function(outputDir) {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  # Concatenate all data together into one data.frame
  if(outputDir == 'WQSlookupTable'){
    data <- lapply(files, read_csv) 
    data <- do.call(rbind, data) %>%
      distinct(StationID, WQS_ID, .keep_all = T)
  } else {
    data <- lapply(files, read_csv) # read_csv produces parsing errors
    data <- do.call(rbind, data) %>%
      distinct(FDT_STA_ID, ID305B_1,  .keep_all = T)
  }
 
  data
}


# Weblink to internal DEQ app constants
webLinkpart1 <- "<b><a href='https://gis.deq.virginia.gov/GISStaffApplication/?query=WQM%20Stations%20(All%20stations%20with%20full%20attributes),STATION_ID,"
webLinkpart2 <- "&showLayers=DEQInternalDataViewer_1723;WATER%20LAYERS;WQM%20Stations%20(All%20stations%20with%20full%20attributes);"
webLinkpart3 <- "&level=14' target='_blank'>Open Link In New Tab</a></b>" 
