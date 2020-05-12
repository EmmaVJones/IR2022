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

source('appModules/multipleDependentSelectizeArguments.R')
source('snappingFunctions/snapOrganizationFunctions_messAround.R') # turn this back on when messing with AU side of things again


#snapList_AU <- readRDS("data/preAnalyzedRegionalAUdata/BRRO/Riverine/James River Basin.RDS")
#snapList_AU <- readRDS("data/preAnalyzedRegionalAUdata/BRRO/Lacustrine/Roanoke River Basin.RDS")
#tooMany <- snapCheck(snapList_AU[['sf_output']])

#AUs1 <- st_read(paste0('data/processedGIS/va_2018_aus_', 'riverine','_', 
#                       'BRRO', "_", 'James River Basin', '.shp')) %>%
#  st_transform(4326)

#conventionals_D <- st_read(paste0('data/conventionals_D_James River Basin.shp')) 

basinCodesConversion <- read_csv('data/basinCodeConversion.csv') %>%
  filter(BASIN != 7) %>%
  bind_rows(data.frame(BASIN = '7D', Basin_Code = 'Small Coastal'))


# Attach SUBBASIN info to appropriate assessment Region
#basinAssessmentRegion <- st_intersection(basin7, assessmentRegions) %>%
#  st_drop_geometry() %>%
#  left_join(mutate(basinCodesConversion, BASIN_CODE = BASIN), by="BASIN_CODE")
#write.csv(basinAssessmentRegion, 'data/basinAssessmentRegion.csv')
basinAssessmentRegion <- read_csv('data/basinAssessmentReg_clb.csv') %>% # Cleo QAed verison
  filter(VAHU6_NOTE !=  "NOT IN THIS REGION") %>%
  left_join(mutate(basinCodesConversion, BASIN_CODE = BASIN), by="BASIN_CODE")


# WQS layer type to WQS_ID conversion
#st_layers('GIS/WQS_layers_05082020.gdb')
WQSlayerConversion <- tibble(waterbodyType = c('Riverine','Lacustrine','Estuarine','Estuarine'),
                             WQS_ID = c('RL','LP','EL','EP')) 
