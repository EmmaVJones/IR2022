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

#source('appModules/multipleDependentSelectizeArguments.R')
#source('snappingFunctions/snapOrganizationFunctions_messAround.R')

#snapList_AU <- readRDS("data/preAnalyzedRegionalAUdata/BRRO/Riverine/James River Basin_snapList.RDS")
##readRDS("data/preAnalyzedRegionalAUdata/BRRO/Riverine/New River Basin_snapList.RDS")
#tooMany <- snapCheck(snapList_AU[['sf_output']])

# All conventionals sites
#conventionals_D <- st_read('GIS/conventionals_D.shp')

#assessmentRegions <- st_read( 'GIS/AssessmentRegions_simple.shp')
#assessmentLayer <- st_read('GIS/AssessmentRegions_VA84_basins.shp') %>%
#  st_transform( st_crs(4326)) 
##riverineAUs <-  st_read('C:/HardDriveBackup/GIS/Assessment/va_2018_aus_riverine.shp') %>%
##  st_transform(st_crs(assessmentLayer))

#riverineAUs <- st_read(paste0('C:/HardDriveBackup/GIS/Assessment/va_2018_aus_',
#                              tolower('Riverine'),'.shp')) %>%
#  st_transform(4326)  
#AUs1 <- suppressWarnings(st_intersection(st_zm(riverineAUs), 
#                                         filter(assessmentLayer, ASSESS_REG == "BRRO") %>%
#                                           filter(Basin == 'James River Basin')))