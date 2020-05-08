# Smash together all preprocessing data from regions who returned it for 2020 IR

## BRRO
BRRO <- bind_rows(
  read_csv('C:/HardDriveBackup/R/GitHub/Rivers-StreamsAssessment/R&S_app_v4/processedStationData/RegionalResultsRiverine_BRROCitMonNonAgencyFINAL.csv') %>%
    dplyr::select(FDT_STA_ID, ID305B_1, ID305B_2, ID305B_3, SEC, CLASS, SPSTDS, PWS, Trout,Tier_III),  # only keep columns for QA
  read_csv('C:/HardDriveBackup/R/GitHub/LakesAssessment2020/app2020/LakeAssessmentApp_citmon/processedStationData/lakeStations2020_BRRO_citmonNonAgency.csv') %>%
    dplyr::select(FDT_STA_ID, ID305B_1, ID305B_2, ID305B_3, SEC, CLASS, SPSTDS, PWS, Trout,Tier_III)) # only keep columns for QA

  
  
  
## SWRO
SWRO <- bind_rows(
  read_csv('C:/HardDriveBackup/R/GitHub/Rivers-StreamsAssessment/R&S_app_v4/processedStationData/RegionalResultsRiverine_SWRO_updated.csv') %>%
    dplyr::select(FDT_STA_ID, ID305B_1, ID305B_2, ID305B_3, SEC, CLASS, SPSTDS, PWS, Trout,Tier_III),  # only keep columns for QA
  read_csv('C:/HardDriveBackup/R/GitHub/LakesAssessment2020/app2020/LakeAssessmentApp_v2/processedStationData/final2020data/lakeStations2020_SWRO.csv') %>%
    dplyr::select(FDT_STA_ID, ID305B_1, ID305B_2, ID305B_3, SEC, CLASS, SPSTDS, PWS, Trout,Tier_III))  # only keep columns for QA
  

## TRO
TRO <- bind_rows(
  read_csv('C:/HardDriveBackup/R/GitHub/LakesAssessment2020/app2020/LakeAssessmentApp_citmon/processedStationData/final2020data/lakeStations2020_TRO.csv')%>%
    dplyr::select(FDT_STA_ID, ID305B_1, ID305B_2, ID305B_3, SEC, CLASS, SPSTDS, PWS, Trout,Tier_III),  # only keep columns for QA
  read_csv('C:/HardDriveBackup/R/GitHub/Rivers-StreamsAssessment/preprocessingRegionalData/processedStationData/final2020data/RegionalResultsRiverine_TROFINAL.csv')%>%
    dplyr::select(FDT_STA_ID, ID305B_1, ID305B_2, ID305B_3, SEC, CLASS, SPSTDS, PWS, Trout,Tier_III))  # only keep columns for QA


## PRO
PRO <- bind_rows(
  read_csv('C:/HardDriveBackup/R/GitHub/LakesAssessment2020/app2020/LakeAssessmentApp_citmon/processedStationData/final2020data/RegionalResultsLake_PROFINAL.csv') %>%
    dplyr::select(FDT_STA_ID, ID305B_1, ID305B_2, ID305B_3, SEC, CLASS, SPSTDS, PWS, Trout,Tier_III),  # only keep columns for QA
  read_csv('C:/HardDriveBackup/R/GitHub/Rivers-StreamsAssessment/R&S_app_v4/processedStationData/RegionalResultsRiverine_PROFINAL.csv') %>%
    dplyr::select(FDT_STA_ID, ID305B_1, ID305B_2, ID305B_3, SEC, CLASS, SPSTDS, PWS, Trout,Tier_III))  # only keep columns for QA


# Combine everyone that has data
existingData <- bind_rows(BRRO, PRO, SWRO, TRO)

rm(BRRO);rm(PRO);rm(TRO);rm(SWRO)
