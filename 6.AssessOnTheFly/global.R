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



# Conventionals Function
automatedAssessmentFunction <- function(stationTable, conventionals, lakeStations, lacustrineDesignation, VSCIresults){
  stationTableResults <- tibble()
  # save ammonia results (based on default assessment information) for use in app to speed rendering
  ammoniaAnalysis <- tibble()
  
  for(i in 1:nrow(stationTable)){
    #i = 1
    print(paste('Assessing station', i, 'of', nrow(stationTable), sep=' '))
    
    # pull one station data
    stationData <- filter(conventionals, FDT_STA_ID %in% stationTable$STATION_ID[i]) %>%
      left_join(stationTable, by = c('FDT_STA_ID' = 'STATION_ID')) %>%
      pHSpecialStandardsCorrection() %>% # correct pH to special standards where necessary
      # special lake steps
      {if(stationTable$STATION_ID[i] %in% lakeStations$STATION_ID)
        suppressWarnings(suppressMessages(
          mutate(., lakeStation = TRUE) %>%
            thermoclineDepth())) # adds thermocline information and SampleDate
        else mutate(., lakeStation = FALSE) } %>% 
      # manually add lacustrine designation per user input
      mutate(LACUSTRINE = ifelse(stationTable$STATION_ID[i] %in% lacustrineDesignation$STATION_ID, TRUE, FALSE))
    
    
    # If data exists for station, run it
    if(nrow(stationData) > 0){
      # Date last sampled
      dateLastSampled <- as.character(max(stationData$FDT_DATE_TIME)) } else {dateLastSampled <- 'No data in conventionals data pull'}
    
    
    # Ammonia special section
    ammoniaAnalysisStation <- freshwaterNH3limit(stationData, trout = ifelse(unique(stationData$CLASS) %in% c('V','VI'), TRUE, FALSE),
                                                 mussels = TRUE, earlyLife = TRUE) 
    # https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section155/ states the assumption is that
    #  waters are to be assessed with the assumption that mussels and early life stages of fish should be present
    # trout presence is determined by WQS class, this can be changed in the app but is forced to be what the station
    # is attributed to in the automated assessment scripts
    
    
    if(nrow(stationData) > 0){
      # PWS stuff
      if(is.na(unique(stationData$PWS))  ){
        PWSconcat <- tibble(#STATION_ID = unique(stationData$FDT_STA_ID),
          PWS= NA)
      } else {
        PWSconcat <- cbind(#tibble(STATION_ID = unique(stationData$FDT_STA_ID)),
          assessPWS(stationData, NITRATE_mg_L, LEVEL_NITRATE, 10, 'PWS_Nitrate'),
          assessPWS(stationData, CHLORIDE_mg_L, LEVEL_CHLORIDE, 250, 'PWS_Chloride'),
          assessPWS(stationData, SULFATE_TOTAL_mg_L, LEVEL_SULFATE_TOTAL, 250, 'PWS_Total_Sulfate')) %>%
          dplyr::select(-ends_with('exceedanceRate')) }
      
      # chloride assessment if data exists
      if(nrow(filter(stationData, !is.na(CHLORIDE_mg_L))) > 0){
        chlorideFreshwater <- chlorideFreshwaterSummary(suppressMessages(chlorideFreshwaterAnalysis(stationData)))
      } else {chlorideFreshwater <- tibble(CHL_EXC = NA, CHL_STAT= NA)}
      
      # Nutrients based on station type
      # Nutrient: TP (lakes have real standards; riverine no longer uses 0.2 mg/L as an observed effect for Aquatic life use but will use it as flag for this)
      if(unique(stationData$lakeStation) == TRUE){
        TP <- TP_Assessment(stationData) 
      } else {
        TP <- countNutrients(stationData, PHOSPHORUS_mg_L, LEVEL_PHOSPHORUS, 0.2) %>% quickStats('NUT_TP') %>%   # using 0.2mg/L as flag for this "assessment"
          mutate(NUT_TP_STAT = ifelse(NUT_TP_STAT != "S", "Review", NA)) } # flag OE but don't show a real assessment decision
      
      # Nutrients: Chl a (lakes)
      if(unique(stationData$lakeStation) == TRUE){
        chla <- chlA_Assessment(stationData)
        #tibble(NUT_CHLA_EXC = NA, NUT_CHLA_SAMP = NA, NUT_CHLA_STAT = NA) # placeholder for now
      } else {
        chla <- countNutrients(stationData, CHLOROPHYLL_A_ug_L, LEVEL_CHLOROPHYLL_A, NA) %>% quickStats('NUT_CHLA') %>%
          mutate(NUT_CHLA_STAT = NA) } # don't show a real assessment decision
      
      # run DO daily average status for riverine and tuck results into comments later
      if(unique(stationData$lakeStation) == TRUE){
        DO_Daily_Avg_STAT <- ''
      } else {
        DO_Daily_Avg_STAT <- paste0('DO_Daily_Avg_STAT: ', 
                                    DO_Assessment_DailyAvg(stationData) %>% 
                                      quickStats('DO_Daily_Avg') %>% 
                                      dplyr::select(DO_Daily_Avg_STAT) %>% pull())}
      
      
      results <- cbind(
        dplyr::select(stationData, STATION_ID = FDT_STA_ID, Sta_Desc:BASIN_CODE, Basin_Code= Basin_Code.y, CountyCityName:Location) %>% 
          distinct(STATION_ID, .keep_all = T),
        tempExceedances(stationData) %>% quickStats('TEMP'),
        DOExceedances_Min(stationData) %>% quickStats('DO'), 
        # this will be removed for lake stations later since it does not apply
        pHExceedances(stationData) %>% quickStats('PH'),
        bacteriaAssessmentDecisionClass(stationData),
        ammoniaDecision(list(acute = freshwaterNH3Assessment(ammoniaAnalysisStation, 'acute'),
                             chronic = freshwaterNH3Assessment(ammoniaAnalysisStation, 'chronic'),
                             fourDay = freshwaterNH3Assessment(ammoniaAnalysisStation, 'four-day'))), 
        
        # Will add in metals assessment at a later date
        
        # PCB and fish info can only be incorporated when data migrated into CEDS
        
        # Benthics, just a flag that benthic data exists
        benthicAssessment(stationData, VSCIresults),
        
        # Nutrient Assessment done above by waterbody type
        TP,
        chla) %>%
        # COMMENTS
        mutate(COMMENTS = paste0(DO_Daily_Avg_STAT, 
                                 ' | AMMONIA Comment: ', `Assessment Decision`,
                                 BACTERIA_COMMENTS = paste0(' | E.coli Comment: ', ECOLI_STATECOLI_VERBOSE,
                                                            ' | Enterococci Comment: ', ENTER_STATENTER_VERBOSE)) ) %>%
        dplyr::select(-ends_with(c('exceedanceRate', 'VERBOSE', 'Assessment Decision', 'StationID'))) %>%  # to match Bulk Upload template but helpful to keep visible til now for testing
        mutate(`Date Last Sampled` = dateLastSampled) 
    } else {# pull what you can from last cycle and flag as carry over
      results <- filter(stationTable, STATION_ID == stationTable$STATION_ID[i]) %>%
        dplyr::select(STATION_ID , Sta_Desc:BASIN_CODE, Basin_Code= Basin_Code.y, CountyCityName:Location) %>% 
        distinct(STATION_ID, .keep_all = T) %>% 
        mutate(COMMENTS = 'This station has no data in current window.') %>%
        mutate(`Date Last Sampled` = dateLastSampled)
    }
    stationTableResults <- bind_rows(stationTableResults, results) %>% 
      relocate(c( BACTERIADECISION, BACTERIASTATS), .after = `Date Last Sampled`)
    ammoniaAnalysis <- bind_rows(ammoniaAnalysis, tibble(StationID = unique(stationData$FDT_STA_ID), AmmoniaAnalysis = list(ammoniaAnalysisStation)))
  }
  
  return(list(stationTableResults = stationTableResults, ammoniaAnalysis = ammoniaAnalysis))
}

