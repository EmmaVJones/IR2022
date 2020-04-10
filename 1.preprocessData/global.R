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

#AUs1 <- st_read(paste0('data/processedGIS/va_2018_aus_', 'riverine','_', 
#                       'BRRO', "_", 'James River Basin', '.shp')) %>%
#  st_transform(4326)

#conventionals_D <- st_read(paste0('data/conventionals_D_James River Basin.shp')) 
