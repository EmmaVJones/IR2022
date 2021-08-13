source('global.R')

assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
ecoregion <- st_read('data/GIS/vaECOREGIONlevel3__proj84.shp')
ecoregionLevel4 <- st_read('data/GIS/vaECOREGIONlevel4__proj84.shp')
county <- st_read('data/GIS/VACountyBoundaries.shp')
assessmentLayer <- st_read('data/GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326))
subbasins <- st_read('data/GIS/DEQ_VAHUSB_subbasins_EVJ.shp') %>%
  rename('SUBBASIN' = 'SUBBASIN_1') %>%
  mutate(SUBBASIN = ifelse(is.na(SUBBASIN), as.character(BASIN_NAME), as.character(SUBBASIN))) %>%
  mutate(ProbBasin = case_when(SUBBASIN == 'Big Sandy River' ~ 'Big Sandy',
                               SUBBASIN == 'Chowan River' ~ 'Chowan',
                               SUBBASIN %in% c('James River - Lower', "James River - Middle", "James River - Upper") ~ 'James',
                               SUBBASIN == 'New River' ~ 'New',
                               SUBBASIN == 'Potomac River' ~ 'Potomac',
                               SUBBASIN == 'Shenandoah River' ~ 'Shenandoah',
                               SUBBASIN == 'Rappahannock River' ~ 'Rappahannock',
                               SUBBASIN == 'Roanoke River' ~ 'Roanoke',
                               SUBBASIN == 'Clinch and Powell Rivers' ~ 'Clinch',
                               SUBBASIN == 'Holston River' ~ 'Holston',
                               SUBBASIN == 'York River' ~ 'York',
                               TRUE ~ as.character(NA)),
         ProbSuperBasin = case_when(SUBBASIN %in% c('Big Sandy River','Holston River','Clinch and Powell Rivers') ~ 'Tennessee',
                                    SUBBASIN %in% c('Potomac River', 'Shenandoah River') ~ 'Potomac-Shenandoah',
                                    SUBBASIN %in% c('Rappahannock River', 'York River') ~ 'Rappahannock-York',
                                    TRUE ~ as.character(NA)))

subbasinVAHU6crosswalk <- read_csv('data/basinAssessmentReg_clb_EVJ.csv') %>%
  filter(!is.na(SubbasinVAHU6code)) %>%
  mutate(SUBBASIN = ifelse(is.na(SUBBASIN), BASIN_NAME, SUBBASIN)) #%>%
#dplyr::select(SUBBASIN, SubbasinVAHU6code)

# labCommentCodes <- pool %>% tbl( "Wqm_Comment_Cds_Codes_Wqm_View") %>%
#   as_tibble()
# pin(labCommentCodes, description = 'Lab Comment Codes', board = 'rsconnect')
labCommentCodes <- pin_get("labCommentCodes", board = 'rsconnect')

WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")
WQM_Stations_Spatial <- pin_get("ejones/WQM-Stations-Spatial", board = "rsconnect") %>%
  rename("Basin_Name" = "Basin_Code") # can't have same name different case when using sqldf
WQM_Stations_Full <- st_as_sf(pin_get('ejones/WQM-Station-Full', board = 'rsconnect'))





## For testing: connect to ODS production
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "ODBC Driver 11 for SQL Server",#"SQL Server Native Client 11.0",
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  trusted_connection = "yes"
)

# analyte options
Wqm_Parameter_Grp_Cds_Codes_Wqm_View <- pool %>% tbl(in_schema("wqm", 'Wqm_Parameter_Grp_Cds_Codes_Wqm_View')) %>%
  filter(Pg_Parm_Name != "STORET STORAGE TRANSACTION DATE YR/MO/DAY") %>%
  distinct(Pg_Parm_Name) %>% arrange(Pg_Parm_Name) %>% as_tibble() %>% drop_na()



# General user inputs

runIDfilter <-""# NULL isn't what happens in the app# "W19%"#
labGroupCodeFilter <- NULL#'TNUTL'
programCodeFilter <- NULL#'HF'#c('AW','TR')
countyFilter <- NULL#"Roanoke City"#
ecoregionFilter <- NULL#"Middle Atlantic Coastal Plain"#NULL#"Blue Ridge"#unique(ecoregion$US_L3NAME)
ecoregionLevel4Filter <- NULL
dateRange_multistation <- c(as.Date('2020-01-01'), as.Date('2021-12-31'))#as.Date(Sys.Date()- 7))
## pull based on parameter
analyte_Filter <- NULL#
#c('SODIUM (NA), ATM DEP, WET, DISS, MG/L', 'SODIUM, DISSOLVED (MG/L AS NA)', 'SODIUM, TOTAL (MG/L AS NA)', 'SODIUM-TOTAL  UG/L (AS NA)')

# manually specify troubleshooting
manualSelection1 <- '1BDUR000.11'#c('2-JKS028.69', '2-JKS023.61')#4AROA000.00'#c('1BSMT001.53','1BSMT006.62','1BSMT009.08')#1AFOU002.06')
#WQM_Stations_Filter <- filter(WQM_Stations_Spatial, StationID %in% as.character(manualSelection1))  
WQM_Stations_Filter <- WQM_Stations_Filter_function('Manually Specify Stations (takes a few seconds for the station text box to appear)', 
                                                    pool, WQM_Stations_Spatial, VAHU6Filter = NULL, subbasinFilter = NULL, assessmentRegionFilter = NULL,
                                                    ecoregionFilter = ecoregionFilter, ecoregionLevel4Filter = ecoregionLevel4Filter, countyFilter = countyFilter, dateRange_multistation, analyte_Filter, 
                                                    programCodeFilter = programCodeFilter, labGroupCodeFilter = labGroupCodeFilter, runIDfilter = runIDfilter,
                                                    manualSelection = manualSelection1, wildcardSelection = NULL)

# wildcard troubleshooting
wildcardText1 <- '2-JKS0%'#'4aroa%'#'2-JKS02%'#'3-RPP10%'
# wildcardResults <- sqldf(paste0('SELECT * FROM WQM_Stations_Spatial WHERE StationID like "',
#                                 wildcardText1, '"'))
WQM_Stations_Filter <- WQM_Stations_Filter_function('Wildcard Selection', 
                                                    pool, WQM_Stations_Spatial, VAHU6Filter = NULL, subbasinFilter = NULL, assessmentRegionFilter = NULL,
                                                    ecoregionFilter = ecoregionFilter, ecoregionLevel4Filter = ecoregionLevel4Filter, countyFilter, dateRange_multistation, analyte_Filter= NULL, 
                                                    programCodeFilter = programCodeFilter, labGroupCodeFilter = labGroupCodeFilter, runIDfilter = runIDfilter, 
                                                    manualSelection = NULL, wildcardSelection = wildcardText1)


# Spatial filters troubleshooting
### begin
assessmentRegionFilter <- c("BRRO")#c("BRRO")#NULL#c("PRO")#unique(subbasins$ASSESS_REG)
subbasinFilter <- "James-Upper"#NULL#"Appomattox"#"James-Upper"# c("James-Middle",'Potomac-Lower')#NULL# c("James River - Middle",'Potomac River')#NULL#"James River - Lower"
# filter(WQM_Stations_Spatial, ASSESS_REG %in% assessmentRegionFilter) %>%
#   distinct(Basin_Name) %>%  pull()
VAHU6Filter <- NULL#'JU11'#NULL 
# filter(WQM_Stations_Spatial, ASSESS_REG %in% assessmentRegionFilter) %>%
#   filter(Basin_Name %in% subbasinFilter) %>% 
#   distinct(VAHU6) %>%  pull()



WQM_Stations_Filter <- WQM_Stations_Filter_function('Spatial Filters', pool, WQM_Stations_Spatial, VAHU6Filter, subbasinFilter, assessmentRegionFilter,
                                                    ecoregionFilter, ecoregionLevel4Filter, countyFilter, dateRange_multistation, analyte_Filter, 
                                                    programCodeFilter = programCodeFilter, labGroupCodeFilter = labGroupCodeFilter,runIDfilter= runIDfilter,
                                                    manualSelection = NULL, wildcardSelection = NULL)


### end filter options


multistationInfoFin <- left_join(Wqm_Stations_View %>%  # need to repull data instead of calling stationInfo bc app crashes
                                   filter(Sta_Id %in% WQM_Stations_Filter$StationID) %>%
                                   as_tibble() %>%
                                   # add link to data and add link to internal GIS web app with WQS layer on there
                                   mutate(`CEDS Station View Link` = paste0("<b><a href='https://ceds.deq.virginia.gov/ui#wqmStations/",
                                                                            Sta_Id,"'",
                                                                            " target= '_blank'> View Monitoring Station in CEDS</a></b>"),
                                          `DEQ GIS Web App Link` =  paste0("<b><a href='https://gis.deq.virginia.gov/GISStaffApplication/?query=WQM%20Stations%20(All%20stations%20with%20full%20attributes),STATION_ID,",
                                                                           Sta_Id,
                                                                           "&showLayers=DEQInternalDataViewer_1723;WATER%20LAYERS;WQM%20Stations%20(All%20stations%20with%20full%20attributes);",
                                                                           ";2020%20Draft%20ADB%20WQA%20Layers;2020%20Rivers%20(Any%20Use)&level=14' target='_blank'>View Monitoring Station in DEQ Staff App</a></b>" )) %>%
                                   dplyr::select(Sta_Id, Sta_Desc, `CEDS Station View Link`, `DEQ GIS Web App Link`, everything()),
                                 ########filter(WQM_Station_View, Sta_Id %in% toupper(input$station)), # need to filter instead of calling stationInfo bc app crashes
                                 dplyr::select(WQM_Station_Full,
                                               STATION_ID, Latitude, Longitude, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE,
                                               EPA_ECO_US_L3NAME, EPA_ECO_US_L4CODE, EPA_ECO_US_L4NAME, BASINS_HUC_8_NAME,
                                               BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS,
                                               WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III, WQM_YRS_YEAR, WQM_YRS_SPG_CODE),
                                 by = c('Sta_Id' = 'STATION_ID')) %>%
  left_join(dplyr::select(WQM_Stations_Spatial, StationID, ASSESS_REG, CountyCityName), by = c('Sta_Id' = 'StationID')) %>%
  dplyr::select(Sta_Id, Sta_Desc, `CEDS Station View Link`, `DEQ GIS Web App Link`, Latitude, Longitude, WQM_STA_STRAHER_ORDER,
                ASSESS_REG, CountyCityName, EPA_ECO_US_L3CODE, EPA_ECO_US_L3NAME, EPA_ECO_US_L4CODE, EPA_ECO_US_L4NAME,
                BASINS_HUC_8_NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS,
                WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III, everything())


# Pull as many details about station from REST service (if available). Work around provided in case station isn't on REST server
## Pull station info from REST service
WQM_Station_Full_REST <- filter(WQM_Stations_Full, WQM_STA_ID %in% WQM_Stations_Filter$StationID)
#WQM_Station_Full_REST <- WQM_Station_Full_REST_request(pool, station, subbasinVAHU6crosswalk, subbasins, ecoregion)

# Quick Station Sampling Information
stationInfoSampleMetrics <- stationSummarySampingMetrics(WQM_Station_Full_REST, 'multi')


### Field Data Information

multistationFieldData <- pool %>% tbl(in_schema("wqm", "Wqm_Field_Data_View")) %>%
  filter(Fdt_Sta_Id %in% !! WQM_Stations_Filter$StationID &
           between(as.Date(Fdt_Date_Time), !! dateRange_multistation[1], !! dateRange_multistation[2])) %>% # & # x >= left & x <= right
  #Ssc_Description != "INVALID DATA SET QUALITY ASSURANCE FAILURE") %>%  # don't drop QA failure on SQL part bc also drops any is.na(Ssc_Description)
  filter(Ssc_Description != "INVALID DATA SET QUALITY ASSURANCE FAILURE") %>% 
  as_tibble()  


### Analyte information

multistationAnalyteData <- pool %>% tbl(in_schema("wqm", "Wqm_Analytes_View")) %>%
  filter(Ana_Sam_Fdt_Id %in% !! multistationFieldData$Fdt_Id &
           #between(as.Date(Ana_Received_Date), !! dateRange_multistation[1], !! dateRange_multistation[2]) & # x >= left & x <= right
           Pg_Parm_Name != "STORET STORAGE TRANSACTION DATE YR/MO/DAY") %>% 
  as_tibble() %>%
  left_join(dplyr::select(multistationFieldData, Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time), by = c("Ana_Sam_Fdt_Id" = "Fdt_Id"))



# User filters
multistationDateRangeFilter <-  c(as.Date('2020-01-01'), as.Date(Sys.Date()))#as.Date('2011-01-01'), as.Date('2011-12-31'))#c(as.Date('2015-02-24'), as.Date(Sys.Date()))#
multistationLabCodesDropped <- c('QF')#sort(unique(stationAnalyteData$Ana_Com_Code))
multistationRepFilter <- c('R')
multistationDepthFilter <- T

multistationFieldDataUserFilter <- filter(multistationFieldData, between(as.Date(Fdt_Date_Time), multistationDateRangeFilter[1], multistationDateRangeFilter[2]) ) %>%
  {if(multistationDepthFilter == TRUE)
    filter(., Fdt_Depth <= 0.3)
    else . }

multistationAnalyteDataUserFilter <- filter(multistationAnalyteData, between(as.Date(Fdt_Date_Time), multistationDateRangeFilter[1], multistationDateRangeFilter[2]) )  %>% 
  filter(Ana_Sam_Mrs_Container_Id_Desc %in% multistationRepFilter) %>% 
  filter(! Ana_Com_Code %in% multistationLabCodesDropped)

VSCIresults <- pin_get("VSCIresults", board = "rsconnect") %>%
  filter( between(`Collection Date`, multistationDateRangeFilter[1], multistationDateRangeFilter[2]) )

multistationInfo <- pool %>% tbl(in_schema("wqm",  "Wqm_Stations_View")) %>%
  filter(Sta_Id %in% !! WQM_Stations_Filter$StationID) %>%
  as_tibble()

multistationGIS_View <-  pool %>% tbl(in_schema("wqm",  "Wqm_Sta_GIS_View")) %>%
  filter(Station_Id %in% !! WQM_Stations_Filter$StationID) %>%
  as_tibble()

conventionals <- conventionalsSummary(conventionals= pin_get("conventionals2022IRfinalWithSecchi", board = "rsconnect")[0,],
                                      stationFieldDataUserFilter= multistationFieldDataUserFilter,
                                      stationAnalyteDataUserFilter = multistationAnalyteDataUserFilter,
                                      multistationInfo,
                                      multistationGIS_View,
                                      dropCodes = c('QF'))%>% 
  arrange(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH) 


stationTable <- left_join(tibble(STATION_ID = WQM_Stations_Filter$StationID),
                          WQSlookup, by = c('STATION_ID'='StationID')) %>%
  mutate(CLASS_BASIN = paste(CLASS,substr(BASIN, 1,1), sep="_")) %>%
  mutate(CLASS_BASIN = ifelse(CLASS_BASIN == 'II_7', "II_7", as.character(CLASS))) %>%
  # Fix for Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard)
  left_join(WQSvalues, by = 'CLASS_BASIN') %>%
  dplyr::select(-c(CLASS.y,CLASS_BASIN)) %>%
  rename('CLASS' = 'CLASS.x') %>%
  left_join(WQMstationSpatial %>% distinct(StationID, .keep_all = TRUE), by = c('STATION_ID' = 'StationID')) %>%
  # last cycle had code to fix Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard) but not sure if necessary
  lakeNameStandardization() %>% # standardize lake names
  
  
  # extra special step
  mutate(Lake_Name = case_when(STATION_ID %in% c('2-TRH000.40') ~ 'Thrashers Creek Reservoir',
                               STATION_ID %in% c('2-LSL000.16') ~ 'Lone Star Lake F (Crystal Lake)',
                               STATION_ID %in% c('2-LSL000.04') ~ 'Lone Star Lake G (Crane Lake)',
                               STATION_ID %in% c('2-LSL000.20') ~ 'Lone Star Lake I (Butler Lake)',
                               STATION_ID %in% c('2-NWB002.93','2-NWB004.67', '2-NWB006.06') ~ 'Western Branch Reservoir',
                               STATION_ID %in% c('2-LDJ000.60') ~ 'Lake Nottoway (Lee Lake)',
                               TRUE ~ as.character(Lake_Name))) %>%
  left_join(lakeNutStandards %>% 
              mutate(Lakes_187B = 'y'),  # special step to make sure the WQS designation for 187 are correct even when not
            by = c('Lake_Name')) %>%
  # lake drummond special standards
  mutate(Lakes_187B = ifelse(is.na(Lakes_187B.y ), Lakes_187B.x, Lakes_187B.y), 
         `Chlorophyll a (ug/L)` = case_when(Lake_Name %in% c('Lake Drummond') ~ 35,
                                            TRUE ~ as.numeric(`Chlorophyll a (ug/L)`)),
         `Total Phosphorus (ug/L)` = case_when(Lake_Name %in% c('Lake Drummond') ~ 40,
                                               TRUE ~ as.numeric(`Total Phosphorus (ug/L)`))) %>% 
  dplyr::select(STATION_ID:StreamType, Lakes_187B, `Description Of Waters`:`Total Phosphorus (ug/L)`)

stationTableUI <- dplyr::select(stationTable, STATION_ID, Lakes_187B, CLASS) %>% 
  mutate(`Lake Station` = ifelse(Lakes_187B == 'y', T, F),
         `Lacustrine Zone` = F) %>% 
  dplyr::select(STATION_ID, `Lake Station`, `Lacustrine Zone`, everything())

glimpse(stationTableUI)




assessmentResults <- automatedAssessmentFunction(stationTable, conventionals,
                                                 lakeStations = filter(stationTable, STATION_ID %in% c('2-JKS053.48', '2-JKS044.60', '2-JKS044.60','2-JKS048.90')),
                                                 lacustrineDesignation = filter(stationTable, STATION_ID %in% c('2-JKS044.60', '2-JKS044.60')),
                                                 VSCIresults = VSCIresults)




AUdata <- left_join( conventionals, dplyr::select(stationTable, STATION_ID:VAHU6,
                                                  WQS_ID:US_L3NAME),
                   by = c('FDT_STA_ID' = 'STATION_ID')) %>%
  pHSpecialStandardsCorrection()
