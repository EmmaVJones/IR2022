httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))

library(tidyverse)
library(shiny)
library(shinybusy)
library(leaflet)
library(leaflet.extras)
library(inlmisc)
library(DT)
library(config)
library(sf)
library(plotly)
library(lubridate)
library(pool)
library(geojsonsf)
library(pins)
library(sqldf)
library(dbplyr)

# Server connection things
conn <- config::get("connectionSettings") # get configuration settings


board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))

## For testing: connect to ODS production
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "ODBC Driver 11 for SQL Server",#"SQL Server Native Client 11.0",
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  trusted_connection = "yes"
)

# For deployment on the R server: Set up pool connection to production environment
# pool <- dbPool(
#   drv = odbc::odbc(),
#   Driver = "SQLServer",   # note the LACK OF space between SQL and Server ( how RStudio named driver)
#   # Production Environment
#   Server= "DEQ-SQLODS-PROD,50000",
#   dbname = "ODS",
#   UID = conn$UID_prod,
#   PWD = conn$PWD_prod,
#   #UID = Sys.getenv("userid_production"), # need to change in Connect {vars}
#   #PWD = Sys.getenv("pwd_production")   # need to change in Connect {vars}
#   # Test environment
#   #Server= "WSQ04151,50000",
#   #dbname = "ODS_test",
#   #UID = Sys.getenv("userid"),  # need to change in Connect {vars}
#   #PWD = Sys.getenv("pwd"),  # need to change in Connect {vars}
#   trusted_connection = "yes"
# )

onStop(function() {
  poolClose(pool)
})


# bring in methods and data for automated assessment to work
source('methods/conventionalsFunction.R')
source('methods/updatedBacteriaCriteria.R')
source('methods/3.automatedAssessment_global.R')
source('methods/automatedAssessmentFunctions.R')
source('methods/assessmentFuntion.R')
lakeNutStandards <- read_csv('data/9VAC25-260-187lakeNutrientStandards.csv')
conventionalsTemplate <- pin_get("conventionals2022IRfinalWithSecchi", board = "rsconnect")[0,]

stationOptions <- pin_get('ejones/WQM-Sta-GIS-View-Stations', board= 'rsconnect')
programCodes <- pool %>% tbl(in_schema("wqm", "Wqm_Survey_Pgm_Cds_Codes_Wqm_View")) %>% as_tibble()
labMediaCodes <- pool %>% tbl(in_schema("wqm", "Wqm_Lab_Catalogs_View")) %>% as_tibble()

WQM_Station_Full <- pin_get("ejones/WQM-Station-Full", board = "rsconnect")
Wqm_Stations_View <- pin_get("ejones/WQM-Stations-View", board = "rsconnect")
LRBS <- pin_get("ejones/LRBS", board = 'rsconnect')
WQMstationSpatial <- pin_get("WQM-Stations-Spatial", board = "rsconnect")



# # Pull WQM stations based on spatial and analyte info
WQM_Stations_Filter_function <- function(queryType, pool, WQM_Stations_Spatial, VAHU6Filter, subbasinFilter, assessmentRegionFilter,
                                         ecoregionFilter,  ecoregionLevel4Filter, countyFilter, dateRange_multistation, analyte_Filter, programCodeFilter, 
                                         labGroupCodeFilter, runIDfilter, manualSelection, wildcardSelection){
  # preliminary stations before daterange filter
  if(queryType == 'Spatial Filters' ){
    preliminaryStations <- WQM_Stations_Spatial %>%
      # go small to large spatial filters
      {if(!is.null(VAHU6Filter))
        filter(., VAHU6 %in% VAHU6Filter)
        #st_intersection(., filter(assessmentLayer, VAHU6 %in% VAHU6Filter))
        else .} %>%
      {if(is.null(VAHU6Filter) & !is.null(subbasinFilter))
        filter(., Basin_Name %in% subbasinFilter)
        #st_intersection(., filter(subbasins, SUBBASIN %in% subbasinFilter))
        else .} %>%
      {if(is.null(VAHU6Filter) & !is.null(assessmentRegionFilter)) # don't need assessment region filter if VAHU6 available
        filter(., ASSESS_REG %in% assessmentRegionFilter)
        #st_intersection(., filter(assessmentRegions, ASSESS_REG %in% assessmentRegionFilter))
        else .} %>%
      {if(!is.null(ecoregionFilter))
        filter(., US_L3NAME %in% ecoregionFilter)
        #st_intersection(., filter(ecoregion, US_L3NAME %in% ecoregionFilter))
        else .} %>% 
      {if(!is.null(ecoregionLevel4Filter))
        filter(., US_L4NAME %in% ecoregionLevel4Filter)
        else .} %>%
      {if(!is.null(countyFilter))
        filter(., CountyCityName %in% countyFilter)
        else .}}
  if(str_detect(queryType, 'Manually Specify')){
    preliminaryStations <-  filter(WQM_Stations_Spatial, StationID %in% as.character(manualSelection)) %>% 
      {if(!is.null(ecoregionFilter))
        filter(., US_L3NAME %in% ecoregionFilter)
        #st_intersection(., filter(ecoregion, US_L3NAME %in% ecoregionFilter))
        else .} %>% 
      {if(!is.null(ecoregionLevel4Filter))
        filter(., US_L4NAME %in% ecoregionLevel4Filter)
        else .} %>%
      {if(!is.null(countyFilter))
        filter(., CountyCityName %in% countyFilter)
        else .} }
  if(queryType == 'Wildcard Selection' ){
    preliminaryStations <- sqldf(paste0('SELECT * FROM WQM_Stations_Spatial WHERE StationID like "',
                                        wildcardSelection, '"')) %>% 
      {if(!is.null(ecoregionFilter))
        filter(., US_L3NAME %in% ecoregionFilter)
        #st_intersection(., filter(ecoregion, US_L3NAME %in% ecoregionFilter))
        else .} %>% 
      {if(!is.null(ecoregionLevel4Filter))
        filter(., US_L4NAME %in% ecoregionLevel4Filter)
        else .} %>%
      {if(!is.null(countyFilter))
        filter(., CountyCityName %in% countyFilter)
        else .} }
  
  
  # add daterange filter based on preliminary station results
  if(nrow(preliminaryStations) > 0){
    if(!is.null(dateRange_multistation)){
      stationField <- pool %>% tbl(in_schema("wqm", "Wqm_Field_Data_View")) %>%
        filter(Fdt_Sta_Id %in% !! preliminaryStations$StationID &
                 between(as.Date(Fdt_Date_Time), !! dateRange_multistation[1], !! dateRange_multistation[2]) ) %>% # & # x >= left & x <= right
        #Ssc_Description != "INVALID DATA SET QUALITY ASSURANCE FAILURE") %>%
        # dplyr::select(Fdt_Sta_Id, Fdt_Id) %>% # save time by only bringing back station names
        as_tibble() %>% 
        {if(!is.null(programCodeFilter))
          filter(., Fdt_Spg_Code %in% programCodeFilter)
          else .}
      # wildcard runIDfilter if needed
      if(!is.null(runIDfilter) & runIDfilter != ""){
        stationField <- sqldf(paste0('SELECT * FROM stationField WHERE Fdt_Run_Id like "',
                                     runIDfilter, '"'))      }
      
      # filter by lab group code before bringing in analyte data
      if(nrow(stationField) > 0 & !is.null(labGroupCodeFilter) ){
        sampleView <- pool %>% tbl(in_schema("wqm", "Wqm_Samples_View")) %>%
          filter(Sam_Fdt_Id %in% !! stationField$Fdt_Id &
                   Sam_Mrs_Lcc_Parm_Group_Cd %in% !! labGroupCodeFilter) %>% 
          as_tibble()   
        stationField <- filter(stationField, Fdt_Id %in% sampleView$Sam_Fdt_Id)  } # update stationField to just stations that have lab codes needed
      
      preliminaryStations <- filter(preliminaryStations, StationID %in% stationField$Fdt_Sta_Id)  }
  } else {
    return(preliminaryStations)
  }
  
  if(!is.null(analyte_Filter)){
    stationAnalyte <- pool %>% tbl(in_schema("wqm", "Wqm_Analytes_View")) %>%
      filter(Ana_Sam_Fdt_Id %in% !!  stationField$Fdt_Id &
               #between(as.Date(Ana_Received_Date), !! dateRange_multistation[1], !! dateRange_multistation[2]) & # x >= left & x <= right
               Pg_Parm_Name %in% analyte_Filter) %>%
      dplyr::select(Ana_Sam_Fdt_Id) %>% # save time by only bringing back station names
      as_tibble() %>%
      # need to join back to field data to get station name
      left_join(stationField, by = c("Ana_Sam_Fdt_Id" = "Fdt_Id")) %>%
      distinct(Fdt_Sta_Id)
    preliminaryStations <- filter(preliminaryStations, StationID %in% stationAnalyte$Fdt_Sta_Id)  }
  
  return(preliminaryStations) }


# Quick Station Sampling Summary Information
stationSummarySampingMetrics <- function(stationInfo_sf, singleOrMulti){
  stationInfo_sf %>%
    group_by(STATION_ID) %>%
    {if(singleOrMulti == 'single')
      mutate(., `Years Sampled` = WQM_YRS_YEAR)
      else mutate(., `Years Sampled` = year(WQM_YRS_YEAR)) } %>% 
    dplyr::select(STATION_ID, WQM_YRS_SPG_CODE,WQM_YRS_YEAR,`Years Sampled`,WQM_SPG_DESCRIPTION) %>%
    st_drop_geometry() %>%
    group_by(STATION_ID, `Years Sampled`) %>%
    distinct(WQM_YRS_SPG_CODE, .keep_all = T) %>% # drop repetitive codes for each year
    summarise(`Sample Codes` = paste0(WQM_YRS_SPG_CODE, collapse = ' | '))
} 

