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

source('appModules/multipleDependentSelectizeArguments.R')
source('snappingFunctions/snapOrganizationFunctions_messAround.R')

#snapList_AU <- readRDS("data/preAnalyzedRegionalAUdata/BRRO/Riverine/James River Basin.RDS")
#snapList_AU <- readRDS("data/preAnalyzedRegionalAUdata/BRRO/Lacustrine/Roanoke River Basin.RDS")
#tooMany <- snapCheck(snapList_AU[['sf_output']])

##riverineAUs <-  st_read('C:/HardDriveBackup/GIS/Assessment/va_2018_aus_riverine.shp') %>%
##  st_transform(st_crs(assessmentLayer))

#riverineAUs <- st_read(paste0('C:/HardDriveBackup/GIS/Assessment/va_2018_aus_',
#                              tolower('Riverine'),'.shp')) %>%
#  st_transform(4326)  
#AUs1 <- suppressWarnings(st_intersection(st_zm(riverineAUs), 
#                                         filter(assessmentLayer, ASSESS_REG == "BRRO") %>%
#                                           filter(Basin == 'James River Basin')))

#AUs1 <- st_read(paste0('data/processedGIS/va_2018_aus_', 'riverine','_', 
#                       'BRRO', "_", 'James River Basin', '.shp')) %>%
#  st_transform(4326)



#assessmentType_sf <- st_read(paste0('C:/HardDriveBackup/GIS/Assessment/va_2018_aus_',
#                 tolower('Reservoir_Basins'),'.shp')) %>%
#    st_transform(4326)
#AUs1 <- filter(assessmentType_sf, Basin == 'Roanoke River Basin' & ASSESS_REG == 'BRRO')
## change format of what I bring in? make aus by basin to make files smaller??? potentially by region, too?
##AUs1 <- suppressWarnings(st_intersection(st_zm(assessmentType_sf), filter(assessmentLayer, Basin == 'Roanoke River Basin')))

                         