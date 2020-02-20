library(tidyverse)
library(readxl)
library(sf)
library(shiny)
library(mapview)
library(leaflet)
library(inlmisc)

cit <- read_csv('C:/HardDriveBackup/R/GitHub/Rivers-StreamsAssessment/CitizenNonAgency/2020IR Citizen Ambient4.14.19 (2).csv') %>%
  dplyr::select(Group_Station_ID:`DEQ QA Comments`) %>% # drop junk columns
  filter(!is.na(Latitude)|!is.na(Longitude)) %>% # drop sites without location information
  distinct(Group_Station_ID, FDT_STA_ID, Latitude, Longitude, .keep_all =T)  %>% #distinct by location and name
  dplyr::select(Group_Station_ID,FDT_STA_ID:FDT_SPG_CODE, Latitude:STA_CBP_NAME) %>%# drop data to avoid confusion
  mutate(UID = row_number()) %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) # add coordinate reference system, needs to be geographic for now bc entering lat/lng, 


CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE) %>%
  setView(-78, 37.5, zoom=6) %>%
  addCircleMarkers(data=cit,color='yellow', fillColor='blue', radius = 5,
                   fillOpacity = 0.5,opacity=1,weight = 2,stroke=T,group="sites",
                   label = ~FDT_STA_ID,
                   popup = leafpop::popupTable(cit, zcol=c( "UID","FDT_STA_ID","Huc6_Vahu6" ))) %>%
                   #popup=~paste(sep='',
                  #              paste(strong('StationID : '), FDT_STA_ID),
                  #              paste(strong('Location :'), STA_DESC),
                  #              paste(strong('VAHU6 : '), Huc6_Vahu6))) %>%
  #addPolylines(data=riverineAUs, group = 'Riverine AUs', label = ~riverineAUs$ID305B) %>% hideGroup('Riverine AUs') %>%
  inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
  inlmisc::AddSearchButton(group = "sites", zoom = 15,propertyName = "label",
                           textPlaceholder = "Search stations") %>%
  addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                   overlayGroups = c('sites','Riverine AUs'),
                   options=layersControlOptions(collapsed=T),
                   position='topleft')
