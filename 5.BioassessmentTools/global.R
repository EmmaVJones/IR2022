library(tidyverse)
library(shinydashboard)
library(sf)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(inlmisc)
library(DT)
library(plotly)
library(lubridate)
library(pool)
library(pins)
library(config)

#####################################   UPDATE EACH NEW TOOL REBUILD #############################################
# Establish Assessment Period 
assessmentPeriod <- as.POSIXct(c("2015-01-01 00:00:00 UTC","2020-12-31 23:59:59 UTC"),tz='UTC')
assessmentCycle <- '2022'
##################################################################################################################

source('helperFunctions/VSCI_metrics_GENUS.R')
source('helperFunctions/VCPMI_metrics_GENUS.R')

# Register RStudio Connect, don't need to do multiple times
#board_register("rsconnect", server = "http://deq-rstudio-prod.cov.virginia.gov:3939")

# get configuration settings
conn <- config::get("connectionSettings")

board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))



# Retrieve Pins
#WQM_Station_Full <- pin_get("ejones/WQM-Station-Full", board = "rsconnect")
#Wqm_Stations_View <- pin_get("ejones/WQM-Stations-View", board = "rsconnect")
#WQM_Stations <- pin_get("ejones/WQM-Sta-GIS-View", board = "rsconnect")

# limit data to assessment window From the start
benSamps <- pin_get("ejones/benSamps", board = "rsconnect") %>%
  filter(between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2])) %>%# limit data to assessment window
  filter(RepNum %in% c('1', '2')) %>% # drop QA and wonky rep numbers
  filter(`Target Count` == 110) # only assess rarified data
habSamps <- pin_get("ejones/habSamps", board = "rsconnect") %>%
  filter(between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2]))# limit data to assessment window
Wqm_Stations_View <- pin_get("ejones/WQM-Stations-View", board = "rsconnect") %>%
  filter(Sta_Id %in% benSamps$StationID)
WQM_Stations <- pin_get("ejones/WQM-Sta-GIS-View", board = "rsconnect") %>%
  filter(Station_Id %in% benSamps$StationID)
#WQM_Station_Full <- pin_get("ejones/WQM-Station-Full", board = "rsconnect") %>%
#  filter(WQM_STA_ID %in% benSamps$StationID) %>%
#  distinct(WQM_STA_ID, .keep_all = T) %>%
benSampsStations <- st_as_sf(pin_get("ejones/benSampsStations", board = "rsconnect")) %>%
  filter(StationID %in% benSamps$StationID)
benSamps <- left_join(benSamps, benSampsStations, by = 'StationID') %>% # update with spatial, assess reg, vahu6, basin/subbasin, & ecoregion info
  dplyr::select(StationID, Sta_Desc, everything()) %>% 
  arrange(StationID)


VSCIresults <- pin_get("ejones/VSCIresults", board = "rsconnect") %>%
  filter(BenSampID %in% benSamps$BenSampID)
VCPMI63results <- pin_get("ejones/VCPMI63results", board = "rsconnect") %>%
  filter(BenSampID %in% benSamps$BenSampID)
VCPMI65results <- pin_get("ejones/VCPMI65results", board = "rsconnect") %>%
  filter(BenSampID %in% benSamps$BenSampID)
benthics <- pin_get("ejones/benthics", board = "rsconnect") %>%
  filter(BenSampID %in% benSamps$BenSampID)
habValues <- pin_get("ejones/habValues", board = "rsconnect") %>%
  filter(HabSampID %in% habSamps$HabSampID)
habObs <- pin_get("ejones/habObs", board = "rsconnect") %>%
  filter(HabSampID %in% habSamps$HabSampID)
#masterTaxaGenus <- pin_get("ejones/masterTaxaGenus", board = "rsconnect")



totalHabScore <- function(habValues){
  habValues %>%
    group_by(HabSampID) %>%
    summarise(`Total Habitat Score` = sum(HabValue, na.rm = T))
}

totalHabScoreAverages <- function(habValues_totHab){
  habValues_totHab %>%
    group_by(StationID) %>%
    summarise(`Total Habitat Average` = format(mean(`Total Habitat Score`, na.rm = T), digits = 3),
              `n Samples` = n()) %>% 
    mutate(Window = 'User Selected Window') %>%
    dplyr::select(StationID, Window, `Total Habitat Average`, `n Samples`) %>%
    bind_rows(habValues_totHab %>%
                group_by(StationID, Season) %>%
                summarise(`Total Habitat Average` = format(mean(`Total Habitat Score`, na.rm = T), digits = 3),
                          `n Samples` = n()) %>%
                rename('Window' = 'Season') %>% ungroup()) %>%
    bind_rows(habValues_totHab %>%
                mutate(Window = year(`Collection Date`)) %>%
                group_by(StationID, Window) %>%
                summarise(`Total Habitat Average` = format(mean(`Total Habitat Score`, na.rm = T), digits=3),
                          `n Samples` = n()) %>% ungroup() %>%
                mutate(Window = as.character(Window)))
}
