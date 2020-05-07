# Smash together all preprocessing data from regions who returned it for 2020 IR

# updated WQS for fixing OBJECTID's and adding edit fields, just as tables, no need for spatial info now
riverine <- st_read('GIS/WQS_layers_04282020.gdb', layer = 'riverine_04282020' , fid_column_name = "OBJECTID") %>%
  st_drop_geometry() %>%
  mutate(OBJECTID= as.character(OBJECTID))
lacustrine <- st_read('GIS/WQS_layers_04282020.gdb', layer = 'lakes_reservoirs_04282020' , fid_column_name = "OBJECTID") %>%
  st_drop_geometry()
#estuarineLines <- st_read('GIS/WQS_layers_04282020.gdb', layer = 'estuarinelines_04282020' , fid_column_name = "OBJECTID") %>%
#st_drop_geometry()
#estuarinePoly <- st_read('GIS/WQS_layers_04282020.gdb', layer = 'estuarinepolygons_04282020' , fid_column_name = "OBJECTID") %>%
#st_drop_geometry()

## BRRO
BRROs <- read_csv('C:/HardDriveBackup/R/GitHub/Rivers-StreamsAssessment/R&S_app_v4/processedStationData/RegionalResultsRiverine_BRROCitMonNonAgencyFINAL.csv') %>%
  dplyr::select(-c(FDT_DATE_TIME)) %>% # remove confusing columns
  rename('Shape_Length' = 'Shape_Leng')
BRROsWQS <- dplyr::select(BRROs, FDT_STA_ID, Point.Unique.Identifier:Shape_Length, -c(OBJECTID, Edit_Date, Backlog)) %>% # drop columns we know we want from new layer
  left_join(riverine, by = c("GNIS_ID", "GNIS_Name", "FType", "FCode", "BASIN", "WATER_NAME",
                                  "SEC", "CLASS","SPSTDS"))
              
              #c("GNIS_ID", "GNIS_Name", "FType", "FCode", "BASIN", "WATER_NAME",
                          #   "SEC", "CLASS","SPSTDS")) %>%
  group_by(FDT_STA_ID) %>%
  mutate(n= n())

 "Basin_Code", "PWS", "Trout", 'StreamType', 'Tier_III'

BRROsWQSfine <- filter(BRROsWQS, n == 1)  
# These sites will need to be manually verified again after WQS update
BRROsWQSissues <- filter(BRROsWQS, n>1) %>%
  distinct(FDT_STA_ID)
BRROsWQSfine <- dplyr::select(BRROsWQSfine, FDT_STA_ID, Point.Unique.Identifier, `Buffer.Distance`, OBJECTID, names(riverine))
# Join updated WQS back to AU information
BRROsFin <- dplyr::select(BRROs, -c(Point.Unique.Identifier:Backlog)) %>%
  left_join(BRROsWQSfine, by = 'FDT_STA_ID')

  
BRROl <- read_csv('C:/HardDriveBackup/R/GitHub/LakesAssessment2020/app2020/LakeAssessmentApp_citmon/processedStationData/lakeStations2020_BRRO_citmonNonAgency.csv')


  dplyr::select(FDT_STA_ID:VAHU6, Point.Unique.Identifier:Assess_TYPE) %>%
  mutate(Huc6_Huc_8 = paste0(0, Huc6_Huc_8)) %>%




RegionalResultsCombinedBRRO <- bind_rows(BRROs, BRROl)

## SWRO
RegionalResultsCombinedSWRO <- read_csv('C:/HardDriveBackup/R/GitHub/Rivers-StreamsAssessment/R&S_app_v4/processedStationData/RegionalResultsRiverine_SWRO_updated.csv') %>%
  bind_rows(read_csv('C:/HardDriveBackup/R/GitHub/LakesAssessment2020/app2020/LakeAssessmentApp_v2/processedStationData/final2020data/lakeStations2020_SWRO.csv')) 


%>%
  dplyr::select(FDT_STA_ID:VAHU6, Point.Unique.Identifier:Assess_TYPE) %>%
  mutate(Huc6_Huc_8 = paste0(0, Huc6_Huc_8))


## TRO


## PRO