

library(tidyverse)
library(sf)
library(readxl)
library(miniUI)
library(shiny)
library(leaflet)
library(mapview)

source('snappingFunctions/snapFunctions.R') # snapping functions
source('snappingFunctions/snapOrganizationFunctions_v2.R') # functions to do stuff with snapping functions


# use a custom snapping tool to snap and organize 
riverineAUs <-  st_read('C:/HardDriveBackup/GIS/Assessment/va_2018_aus_riverine.shp') %>%
  st_transform(crs = 102003)  # transform to Albers for spatial intersection


irData_missing_NoAUtest <- readRDS('irData_missing_NoAUtest.RDS')

Regional_Sites_AU <- snapAndOrganizeAU(irData_missing_NoAUtest, riverineAUs,  bufferDistances = seq(10,50,by=10))
