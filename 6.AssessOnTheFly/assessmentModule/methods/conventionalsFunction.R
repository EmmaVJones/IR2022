# Conventionals function to replace SAS method for assessments and
# create standardized dataset for many other application purposes

# Built in R 3.6.2
# source as needed for other applications

# multiple parameter clean up function
# newMethod <-  stationAnalyteDataUserFilter0 %>% 
#   
#   # filter(Ana_Sam_Fdt_Id %in% '2880155') %>% 
#   # 
#   # dplyr::select(Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc, Ana_Sam_Mrs_Lcc_Parm_Group_Cd, Pg_Storet_Code, ParameterName, Ana_Uncensored_Value, Ana_Uncensored_Val_Comment) %>% 
#   group_by(Ana_Sam_Fdt_Id, Pg_Storet_Code) %>% 
#   mutate(n = n()) %>% #filter(Ana_Sam_Fdt_Id == '3001273') 
#   arrange(desc(n), Ana_Sam_Fdt_Id, Pg_Storet_Code) 

fixMultipleParameterResultsFunction <- function(newMethod){
  potentialIssues <- filter(newMethod, n > 1)
  if(nrow(potentialIssues) > 0){
    nonIssues <- filter(newMethod, n == 1)
    #i = unique(potentialIssues$Ana_Sam_Fdt_Id)[1]
    for(i in unique(potentialIssues$Ana_Sam_Fdt_Id)){
      sampleIssue <- filter(potentialIssues, Ana_Sam_Fdt_Id == i)
      
      parameterOut <- sampleIssue[0,] %>% dplyr::select(-c(Ana_Uncensored_Val_Comment, n))
      for(k in unique(sampleIssue$ParameterName)){
        parameter <- filter(sampleIssue, ParameterName == k)
        # catch in case all nondetect
        if(all(parameter$Ana_Uncensored_Val_Comment %in% c('T', 'QQ'))){
          parameter <- suppressWarnings(suppressMessages(
            parameter %>% 
              group_by(Fdt_Sta_Id, Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc, Pg_Storet_Code, ParameterName) %>%
              summarise(Ana_Uncensored_Value = max(Ana_Uncensored_Value)) ))
          parameterOut <- bind_rows(parameterOut, parameter)
        } else {
          parameter <- parameter %>% 
            {if(any(parameter$Ana_Uncensored_Val_Comment %in% c('T', 'QQ')))
              filter(., ! Ana_Uncensored_Val_Comment %in% c('T', 'QQ'))
              else . } %>% 
            ungroup() %>% 
            group_by(Fdt_Sta_Id, Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc, Pg_Storet_Code, ParameterName) %>%
            summarise(Ana_Uncensored_Value = mean(Ana_Uncensored_Value))
          parameterOut <- bind_rows(parameterOut, parameter) }
      }
      
      fixedSample <- suppressWarnings(suppressMessages(
        parameterOut %>% 
          left_join(sampleIssue %>% 
                      ungroup() %>% 
                      group_by(Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc, Pg_Storet_Code, ParameterName) %>% 
                      summarise(Ana_Uncensored_Val_Comment = paste0(Ana_Uncensored_Val_Comment, collapse = ' | ')),
                    by = c('Ana_Sam_Fdt_Id', 'Ana_Sam_Mrs_Container_Id_Desc', 'Pg_Storet_Code', 'ParameterName') )
      ))
      nonIssues <- bind_rows(nonIssues, fixedSample) 
    }
  } else {
    nonIssues <- newMethod
  }
  return(nonIssues %>% arrange(Ana_Id))
}


# Conventionals Summary
conventionalsSummary <- function(conventionals, stationFieldDataUserFilter, stationAnalyteDataUserFilter, stationInfo, stationGIS_View, dropCodes){
  # make template to provide columns that may not exist for coalesce step
  parameterTemplate <- tibble(#Ana_Id = as.character(NA), 
    Ana_Sam_Fdt_Id = as.character(NA), Ana_Sam_Mrs_Container_Id_Desc = as.character(NA), 
    `TOTAL_SUSPENDED_SOLIDS_00530_mg_L` = as.numeric(NA), `NITROGEN_TOTAL_00600_mg_L` = as.numeric(NA), 
    `NITROGEN_AMMONIA_DISSOLVED_00608_mg_L` = as.numeric(NA), `NITROGEN_AMMONIA_TOTAL_00610_mg_L` = as.numeric(NA), 
    `NITROGEN_NITRITE_DISSOLVED_00613_mg_L` = as.numeric(NA), `NITROGEN_NITRITE_TOTAL_00615_mg_L` = as.numeric(NA), 
    `NITROGEN_NITRATE_DISSOLVED_00618_mg_L` = as.numeric(NA), `NITROGEN_NITRATE_TOTAL_00620_mg_L` = as.numeric(NA), 
    `NITROGEN_KJELDAHL_TOTAL_00625_mg_L` = as.numeric(NA), `NITRITE+NITRATE_TOTAL_00630_mg_L` = as.numeric(NA), 
    `NITRITE+NITRATE_DISSOLVED_00631_mg_L` = as.numeric(NA), `PHOSPHORUS_TOTAL_00665_mg_L` = as.numeric(NA), 
    `PHOSPHORUS_DISSOLVED_00666_mg_L` = as.numeric(NA), `PHOSPHORUS_DISSOLVED_ORTHOPHOSPHATE_00671_mg_L` = as.numeric(NA), 
    `HARDNESS_TOTAL_00900_mg_L` = as.numeric(NA), `CHLORIDE_TOTAL_00940_mg_L` = as.numeric(NA), 
    `CHLORIDE_DISSOLVED_00941_mg_L` = as.numeric(NA), `SULFATE_TOTAL_00945_mg_L` = as.numeric(NA), 
    `SULFATE_DISSOLVED_00946_mg_L` = as.numeric(NA), `FECAL_COLIFORM_31616_NO_100mL` = as.numeric(NA), 
    `E._COLI_31648_NO_100mL` = as.numeric(NA), `ENTEROCOCCI_31649_NO_100mL` = as.numeric(NA), 
    `CHLOROPHYLL_32211_ug_L` = as.numeric(NA), `PHOSPHOROUS_PARTICULATE_49567_mg_L` = as.numeric(NA), 
    `NITROGEN_PARTICULATE_49570_mg_L` = as.numeric(NA), `NITROGEN_TOTAL_DISSOLVED_49571_mg_L` = as.numeric(NA), 
    `PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg_L` = as.numeric(NA), `PHOSPHORUS_TOTAL_ORTHOPHOSPHATE_70507_mg_L` = as.numeric(NA), 
    `E.COLI_ECOLI_CFU_100mL` = as.numeric(NA), `ORTHOPHOSPHATE_DISSOLVED_OPWLF_mg_L` = as.numeric(NA), 
    `PHOSPHORUS_SUSPENDED_INORGANIC_PIPLF_mg_L` = as.numeric(NA), `PHOSPHORUS_PARTICULATE_PPWLF_mg_L` = as.numeric(NA), 
    `SSC-TOTAL_00530_mg_L` = as.numeric(NA), `NITROGEN_TOTAL_DISSOLVED_TDNLF_mg_L` = as.numeric(NA), 
    `PHOSPHORUS_TOTAL_DISSOLVED_TDPLF_mg_L` = as.numeric(NA), `TOTAL_SUSPENDED_SOLIDS_TSS45_mg_L` = as.numeric(NA) )
  remarkTemplate <- tibble(#Ana_Id = as.character(NA), 
    Ana_Sam_Fdt_Id = as.character(NA), Ana_Sam_Mrs_Container_Id_Desc = as.character(NA), 
    `RMK_00530` = as.character(NA), `RMK_00600` = as.character(NA), `RMK_00608` = as.character(NA), `RMK_00610` = as.character(NA), 
    `RMK_00613` = as.character(NA), `RMK_00615` = as.character(NA), `RMK_00618` = as.character(NA), `RMK_00620` = as.character(NA), 
    `RMK_00625` = as.character(NA), `RMK_00630` = as.character(NA), `RMK_00631` = as.character(NA), `RMK_00665` = as.character(NA), 
    `RMK_00666` = as.character(NA), `RMK_00671` = as.character(NA), `RMK_00900` = as.character(NA), `RMK_00940` = as.character(NA), 
    `RMK_00941` = as.character(NA), `RMK_00945` = as.character(NA), `RMK_00946` = as.character(NA), `RMK_31616` = as.character(NA), 
    `RMK_31648` = as.character(NA), `RMK_31649` = as.character(NA), `RMK_32211` = as.character(NA), `RMK_49567` = as.character(NA), 
    `RMK_49570` = as.character(NA), `RMK_49571` = as.character(NA), `RMK_49572` = as.character(NA), `RMK_70507` = as.character(NA), 
    `RMK_ECOLI` = as.character(NA), `RMK_OPWLF` = as.character(NA), `RMK_PIPLF` = as.character(NA), `RMK_PPWLF` = as.character(NA), 
    `RMK_SSC_TOTAL_00530` = as.character(NA), `RMK_TDNLF` = as.character(NA), `RMK_TDPLF` = as.character(NA), `RMK_TSS45` = as.character(NA))
  
  # Step 1: Organize station information to match conventionals format
  stationData <- left_join(stationInfo, stationGIS_View, by = c('Sta_Id' = 'Station_Id')) %>%
    filter(! Sta_Lv1_Code %in% c('LND', 'PIPE', 'UNK', 'WELL')) %>% #drop unwanted Level 1 Codes
    filter(! Sta_Lv2_Code %in% c('INPLNT', 'NONAMB', 'TREATD', 'SEWER')) %>% #drop unwanted Level 2 Codes
    filter(! Sta_Lv3_Code %in% c('IR', 'IND', 'AGRI')) %>%  #drop unwanted Level 3 Codes
    dplyr::select(FDT_STA_ID = Sta_Id, 
                  STA_DESC = Sta_Desc,
                  Deq_Region = Admin_Region, # EVJ change bc Roger has opposite
                  STA_REC_CODE = Sta_Rec_Code, # EVJ change bc Roger has opposite
                  Latitude, Longitude, Huc6_Huc_8, Huc6_Huc_8_Name, Huc6_Name, Huc6_Vahu5, Huc6_Huc_12, Huc6_Huc_12_Name, Huc6_Vahu6, 
                  STA_LV1_CODE = Sta_Lv1_Code, 
                  LV1_DESCRIPTION = Lv1_Description,
                  STA_LV2_CODE = Sta_Lv2_Code,
                  LV2_DESCRIPTION = Lv2_Description, 
                  STA_LV3_CODE = Sta_Lv3_Code, 
                  LV3_DESCRIPTION = Lv3_Description, 
                  STA_CBP_NAME = Sta_Cbp_Name) %>% 
  mutate(GROUP_STA_ID = as.character(NA),
         Waterbody = as.character(NA),
         OTHER_CITMON_NONAGENCY_INFO = as.character(NA),
         Data_Source = as.character(NA))
  

  if(nrow(stationData) > 0){                             
    
    # Step 2: Make field data match conventionals format
    stationFieldDataUserFilter1 <- filter(stationFieldDataUserFilter, ! Fdt_Spg_Code %in% c('IR', 'PC', 'FI')) %>% # exclude targeted incident response and facility data by survey program code
      # drop undesired comment codes
      filter(! grepl('= S|=S|storm|target|duplicate|blank', Fdt_Comment, ignore.case = TRUE)) %>% 
      # Change field measurements to NA if Comment field in dropCodes
      mutate(
        Fdt_Air_Per_Sat = ifelse(coalesce(Fdt_Air_Per_Sat_Rmk, Air_Per_Sat_Remark) %in% dropCodes, NA, Fdt_Air_Per_Sat),
        Fdt_Air_Temp = ifelse(coalesce(Fdt_Air_Temp_Rmk, Air_Temp_Remark) %in% dropCodes, NA, Fdt_Air_Temp),
        Fdt_Baro_Pressure = ifelse(coalesce(Fdt_Baro_Pressure_Rmk, Baro_Pressure_Remark) %in% dropCodes, NA, Fdt_Baro_Pressure),
        Fdt_Chlorine_Residual = ifelse(coalesce(Fdt_Chlorine_Residual_Rmk, Chlorine_Residual_Remark) %in% dropCodes, NA, Fdt_Chlorine_Residual),
        Fdt_Conductivity = ifelse(coalesce(Fdt_Conductivity_Rmk, Conductivity_Remark) %in% dropCodes, NA, Fdt_Conductivity),
        Fdt_Do_Optical = ifelse(coalesce(Fdt_Do_Optical_Rmk, Do_Optical_Remark) %in% dropCodes, NA, Fdt_Do_Optical),
        Fdt_Do_Probe = ifelse(coalesce(Fdt_Do_Probe_Rmk, Do_Probe_Remark) %in% dropCodes, NA, Fdt_Do_Probe),
        Fdt_Do_Winkler = ifelse(coalesce(Fdt_Do_Winkler_Rmk, Do_Winkler_Remark) %in% dropCodes, NA, Fdt_Do_Winkler),
        Fdt_Field_Ph = ifelse(coalesce(Fdt_Field_Ph_Rmk, Field_Ph_Remark) %in% dropCodes, NA, Fdt_Field_Ph),
        Fdt_Flow_Cfs = ifelse(coalesce(Fdt_Flow_Cfs_Rmk, Flow_Cfs_Remark) %in% dropCodes, NA, Fdt_Flow_Cfs),
        Fdt_Gauge_Height = ifelse(coalesce(Fdt_Gauge_Height_Rmk, Gauge_Height_Remark) %in% dropCodes, NA, Fdt_Gauge_Height),
        Fdt_Salinity = ifelse(coalesce(Fdt_Salinity_Rmk, Salinity_Remark) %in% dropCodes, NA, Fdt_Salinity),
        Fdt_Secchi_Depth = ifelse(coalesce(Fdt_Secchi_Depth_Rmk, Secchi_Depth_Remark) %in% dropCodes, NA, Fdt_Secchi_Depth),
        Fdt_Specific_Conductance = ifelse(coalesce(Fdt_Specific_Conductance_Rmk, Specific_Conductance_Remark) %in% dropCodes, NA, Fdt_Specific_Conductance),
        Fdt_Temp_Celcius = ifelse(coalesce(Fdt_Temp_Celcius_Rmk, Temp_Celcius_Remark) %in% dropCodes, NA, Fdt_Temp_Celcius),
        Fdt_Turbidity = ifelse(coalesce(Fdt_Turbidity_Rmk, Turbidity_Remark) %in% dropCodes, NA, Fdt_Turbidity)) %>% 
      rename_with( toupper) %>% 
      mutate(RMK_FDT_FIELD_PH = FDT_FIELD_PH_RMK, LEVEL_FDT_FIELD_PH = as.character(NA),
             RMK_FDT_TEMP_CELCIUS = FDT_TEMP_CELCIUS_RMK, LEVEL_FDT_TEMP_CELCIUS = as.character(NA),
             DO_mg_L = coalesce( FDT_DO_PROBE, FDT_DO_OPTICAL, FDT_DO_WINKLER), # double check logic here
             RMK_DO = coalesce(FDT_DO_PROBE_RMK, FDT_DO_OPTICAL_RMK, FDT_DO_WINKLER_RMK), # double check logic here
             LEVEL_DO = as.character(NA),
             DISSOLVED_OXYGEN_00300_mg_L = FDT_DO_PROBE, RMK_FDT_DO_PROBE = FDT_DO_PROBE_RMK, LEVEL_FDT_DO_PROBE = as.character(NA),
             DISSOLVED_OXYGEN_DOOPT_mg_L = FDT_DO_OPTICAL, RMK_FDT_DO_OPTICAL = FDT_DO_OPTICAL_RMK, LEVEL_FDT_DO_OPTICAL = as.character(NA),
             DISSOLVED_OXYGEN_WINK_mg_L =  FDT_DO_WINKLER, RMK_FDT_DO_WINKLER = FDT_DO_WINKLER_RMK, LEVEL_FDT_DO_WINKLER = as.character(NA),
             RMK_FDT_SPECIFIC_CONDUCTANCE = FDT_SPECIFIC_CONDUCTANCE_RMK, LEVEL_FDT_SPECIFIC_CONDUCTANCE = as.character(NA),
             RMK_FDT_SALINITY = FDT_SALINITY_RMK, LEVEL_FDT_SALINITY = as.character(NA), 
             SECCHI_DEPTH_M = FDT_SECCHI_DEPTH, RMK_SECCHI_DEPTH = FDT_SECCHI_DEPTH_RMK, LEVEL_SECCHI_DEPTH = as.character(NA)) %>% 
      dplyr::select(
        FDT_ID, # necessary for joining field to analyte data
        FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, FDT_DEPTH_DESC, FDT_PERCENT_FRB, FDT_SSC_CODE, FDT_SPG_CODE, FDT_COMMENT,
        FDT_FIELD_PH, RMK_FDT_FIELD_PH, LEVEL_FDT_FIELD_PH, FDT_TEMP_CELCIUS, RMK_FDT_TEMP_CELCIUS, LEVEL_FDT_TEMP_CELCIUS, 
        DO_mg_L, RMK_DO, LEVEL_DO, DISSOLVED_OXYGEN_00300_mg_L, RMK_FDT_DO_PROBE, LEVEL_FDT_DO_PROBE, DISSOLVED_OXYGEN_DOOPT_mg_L,
        RMK_FDT_DO_OPTICAL, LEVEL_FDT_DO_OPTICAL, DISSOLVED_OXYGEN_WINK_mg_L, RMK_FDT_DO_WINKLER, LEVEL_FDT_DO_WINKLER, FDT_SPECIFIC_CONDUCTANCE,
        RMK_FDT_SPECIFIC_CONDUCTANCE, LEVEL_FDT_SPECIFIC_CONDUCTANCE, FDT_SALINITY, RMK_FDT_SALINITY, LEVEL_FDT_SALINITY, 
        SECCHI_DEPTH_M, RMK_SECCHI_DEPTH, LEVEL_SECCHI_DEPTH)
    
    
    # Step 3: Make Analytes match conventionals format, multistep process for remarks and data
    # Step 3.1: drop certain lab codes
    stationAnalyteDataUserFilter <- filter(stationAnalyteDataUserFilter, Ana_Lab_Seq_Num == 1) %>% # drop analyte lab sequence number != 1
      mutate(Ana_Uncensored_Value = case_when(Ana_Com_Code %in% c('IF', 'J', 'O', 'PE', 'Q1', 'QF', 'QFQ', 'V') ~ as.numeric(NA), 
                                              TRUE ~ Ana_Uncensored_Value)) %>% # drop results from invalid lab codes
      filter(Ana_Sam_Mrs_Container_Id_Desc %in% c('', 'C', 'H', 'HV', 'R', 'S1', 'V')) # only keep samples with select container descriptions
    

    # Step 3.2: Rename by storet codes to match conventionals format
    stationAnalyteDataUserFilter0 <- stationAnalyteDataUserFilter %>% 
      
      # only keep data we will do something with to avoid unnecessary data duplication
      filter(Pg_Storet_Code %in% c('00530', '00600', '00608', '00610', '00613' ,'00615', '00618', '00620',
                                   '00625', '00630', '00631', '00665', '00666', '00671', '00900', '00940',
                                   '00941', '00945', '00946', '31616', '31648', '31649', '32211', '49567',
                                   '49570', '49571', '49572', '70507', 'ECOLI', 'OPWLF', 'PIPLF', 'PPWLF',
                                   'SSC-TOTAL', 'TDNLF', 'TDPLF', 'TSS45')) %>% 
      
      mutate(ParameterName = case_when(Pg_Storet_Code == '00530' ~ 'TOTAL_SUSPENDED_SOLIDS_00530_mg_L',
                                       Pg_Storet_Code == '00600' ~ 'NITROGEN_TOTAL_00600_mg_L',
                                       Pg_Storet_Code == '00608' ~ 'NITROGEN_AMMONIA_DISSOLVED_00608_mg_L',
                                       Pg_Storet_Code == '00610' ~ 'NITROGEN_AMMONIA_TOTAL_00610_mg_L',
                                       Pg_Storet_Code == '00613' ~ 'NITROGEN_NITRITE_DISSOLVED_00613_mg_L',
                                       Pg_Storet_Code == '00615' ~ 'NITROGEN_NITRITE_TOTAL_00615_mg_L',
                                       Pg_Storet_Code == '00618' ~ 'NITROGEN_NITRATE_DISSOLVED_00618_mg_L',
                                       Pg_Storet_Code == '00620' ~ 'NITROGEN_NITRATE_TOTAL_00620_mg_L',
                                       Pg_Storet_Code == '00625' ~ 'NITROGEN_KJELDAHL_TOTAL_00625_mg_L',
                                       Pg_Storet_Code == '00630' ~ 'NITRITE+NITRATE_TOTAL_00630_mg_L',
                                       Pg_Storet_Code == '00631' ~ 'NITRITE+NITRATE_DISSOLVED_00631_mg_L',
                                       Pg_Storet_Code == '00665' ~ 'PHOSPHORUS_TOTAL_00665_mg_L',
                                       Pg_Storet_Code == '00666' ~ 'PHOSPHORUS_DISSOLVED_00666_mg_L',
                                       Pg_Storet_Code == '00671' ~ 'PHOSPHORUS_DISSOLVED_ORTHOPHOSPHATE_00671_mg_L',
                                       Pg_Storet_Code == '00900' ~ 'HARDNESS_TOTAL_00900_mg_L',
                                       Pg_Storet_Code == '00940' ~ 'CHLORIDE_TOTAL_00940_mg_L',
                                       Pg_Storet_Code == '00941' ~ 'CHLORIDE_DISSOLVED_00941_mg_L',
                                       Pg_Storet_Code == '00945' ~ 'SULFATE_TOTAL_00945_mg_L',
                                       Pg_Storet_Code == '00946' ~ 'SULFATE_DISSOLVED_00946_mg_L',
                                       Pg_Storet_Code == '31616' ~ 'FECAL_COLIFORM_31616_NO_100mL',
                                       Pg_Storet_Code == '31648' ~ 'E._COLI_31648_NO_100mL',
                                       Pg_Storet_Code == '31649' ~ 'ENTEROCOCCI_31649_NO_100mL',
                                       Pg_Storet_Code == '32211' ~ 'CHLOROPHYLL_32211_ug_L',
                                       Pg_Storet_Code == '49567' ~ 'PHOSPHOROUS_PARTICULATE_49567_mg_L',
                                       Pg_Storet_Code == '49570' ~ 'NITROGEN_PARTICULATE_49570_mg_L',
                                       Pg_Storet_Code == '49571' ~ 'NITROGEN_TOTAL_DISSOLVED_49571_mg_L',
                                       Pg_Storet_Code == '49572' ~ 'PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg_L',
                                       Pg_Storet_Code == '70507' ~ 'PHOSPHORUS_TOTAL_ORTHOPHOSPHATE_70507_mg_L',
                                       Pg_Storet_Code == 'ECOLI' ~ 'E.COLI_ECOLI_CFU_100mL',
                                       Pg_Storet_Code == 'OPWLF' ~ 'ORTHOPHOSPHATE_DISSOLVED_OPWLF_mg_L',
                                       Pg_Storet_Code == 'PIPLF' ~ 'PHOSPHORUS_SUSPENDED_INORGANIC_PIPLF_mg_L',
                                       Pg_Storet_Code == 'PPWLF' ~ 'PHOSPHORUS_PARTICULATE_PPWLF_mg_L',
                                       Pg_Storet_Code == 'SSC-TOTAL' ~ 'SSC-TOTAL_00530_mg_L',
                                       Pg_Storet_Code == 'TDNLF' ~ 'NITROGEN_TOTAL_DISSOLVED_TDNLF_mg_L',
                                       Pg_Storet_Code == 'TDPLF' ~ 'PHOSPHORUS_TOTAL_DISSOLVED_TDPLF_mg_L',
                                       Pg_Storet_Code == 'TSS45' ~ 'TOTAL_SUSPENDED_SOLIDS_TSS45_mg_L',
                                       TRUE ~ as.character(Pg_Storet_Code))) 
    
    
    # Step 3.2.1: Fix issues associated with multiple parameter values returned for a single sample for same parameter (when multiple lab group codes call for same analysis)
    # This step averages only the values that are not non detects (T, QQ) only when multiple parameters exist for the same storet code
    # All remark codes are concatenated regardless of non detect status
    stationAnalyteDataUserFilter0_0 <-  suppressMessages(suppressWarnings(
      stationAnalyteDataUserFilter0 %>% 
      group_by(Ana_Sam_Fdt_Id, Pg_Storet_Code) %>% 
      mutate(n = n()) %>% 
      arrange(desc(n), Ana_Sam_Fdt_Id, Pg_Storet_Code) %>% 
      fixMultipleParameterResultsFunction() ))
    
    # Step 3.3: Organize lab data
    stationAnalyteDataUserFilter1 <- bind_rows(parameterTemplate, 
                                               suppressWarnings(
                                                 stationAnalyteDataUserFilter0_0 %>% 
                                                 #stationAnalyteDataUserFilter0 %>% 
                                                   dplyr::select(Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc,  ParameterName, Ana_Uncensored_Value) %>% 
                                                   pivot_wider(id_cols = Ana_Sam_Fdt_Id, 
                                                               names_from = ParameterName, values_from = Ana_Uncensored_Value, values_fn = list()) %>% 
                                                   unnest(-Ana_Sam_Fdt_Id)) ) %>% 
      rowwise() %>% 
      # For sum in this step, use bazar::sumNA to return NA when none of the fields are populated (instead of 0 which is returned using sum(..., na.rm=T))
      mutate(ECOLI = coalesce(`E.COLI_ECOLI_CFU_100mL`, `E._COLI_31648_NO_100mL`),
             NITROGEN_mg_L = coalesce(NITROGEN_TOTAL_00600_mg_L,
                                      bazar::sumNA(NITROGEN_KJELDAHL_TOTAL_00625_mg_L, `NITRITE+NITRATE_TOTAL_00630_mg_L`, na.rm = F)),
             NITROGEN_mg_L = ifelse(is.na(NITROGEN_TOTAL_00600_mg_L) && !is.na(NITROGEN_NITRITE_TOTAL_00615_mg_L) && 
                                      !is.na(NITROGEN_NITRATE_TOTAL_00620_mg_L) && !is.na(NITROGEN_KJELDAHL_TOTAL_00625_mg_L), 
                                    bazar::sumNA(NITROGEN_NITRITE_TOTAL_00615_mg_L, NITROGEN_NITRATE_TOTAL_00620_mg_L, 
                                                 NITROGEN_KJELDAHL_TOTAL_00625_mg_L, na.rm = F), 
                                    NITROGEN_mg_L),  # Is this even right? Roger doesn't provide an alternate option
             NITROGEN_mg_L = ifelse(is.na(NITROGEN_TOTAL_00600_mg_L) && !is.na(NITROGEN_PARTICULATE_49570_mg_L) && 
                                      !is.na(NITROGEN_TOTAL_DISSOLVED_49571_mg_L), 
                                    bazar::sumNA(NITROGEN_PARTICULATE_49570_mg_L, NITROGEN_TOTAL_DISSOLVED_49571_mg_L, na.rm = F), 
                                    NITROGEN_mg_L),  # Is this even right? Roger doesn't provide an alternate option
             NOX_mg_L = ifelse(is.na(`NITRITE+NITRATE_TOTAL_00630_mg_L`), 
                               bazar::sumNA(NITROGEN_NITRITE_TOTAL_00615_mg_L, NITROGEN_NITRATE_TOTAL_00620_mg_L, na.rm = F),
                               `NITRITE+NITRATE_TOTAL_00630_mg_L`), # Is this even right? Roger doesn't provide an alternate option if NITRITE+NITRATE_TOTAL_00630_mg_L exists so just assuming things here
             AMMONIA_mg_L = coalesce(NITROGEN_AMMONIA_TOTAL_00610_mg_L, NITROGEN_AMMONIA_DISSOLVED_00608_mg_L),
             # secondary AMMONIA_mg_L adjustment based on remark codes when dataset later combined with remarks
             NITRATE_mg_L = coalesce(NITROGEN_NITRATE_TOTAL_00620_mg_L, NITROGEN_NITRATE_DISSOLVED_00618_mg_L),
             
             
             ## Phosphorus, how it is written in Conventionals with overwriting previous data
             PHOSPHORUS_mg_L = coalesce(PHOSPHORUS_TOTAL_00665_mg_L, PHOSPHORUS_TOTAL_ORTHOPHOSPHATE_70507_mg_L, 
                                        PHOSPHORUS_DISSOLVED_00666_mg_L, PHOSPHORUS_DISSOLVED_ORTHOPHOSPHATE_00671_mg_L),
             PHOSPHORUS_mg_L = ifelse(is.na(PHOSPHORUS_TOTAL_00665_mg_L) && !is.na(PHOSPHOROUS_PARTICULATE_49567_mg_L) &&
                                        !is.na(PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg_L),
                                      bazar::sumNA(PHOSPHOROUS_PARTICULATE_49567_mg_L, PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg_L, na.rm = F),
                                      PHOSPHORUS_mg_L), # should the order really be total, ortho, then dissolved???
             
             # ## Phosphorus, how it should be?
             # PHOSPHORUS_mg_L = coalesce(PHOSPHORUS_TOTAL_00665_mg_L, PHOSPHORUS_TOTAL_ORTHOPHOSPHATE_70507_mg_L, 
             #                            PHOSPHORUS_DISSOLVED_00666_mg_L, PHOSPHORUS_DISSOLVED_ORTHOPHOSPHATE_00671_mg_L),
             # PHOSPHORUS_mg_L = ifelse(is.na(PHOSPHORUS_mg_L) && !is.na(PHOSPHOROUS_PARTICULATE_49567_mg_L) &&
             #                            !is.na(PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg_L),
             #                          bazar::sumNA(PHOSPHOROUS_PARTICULATE_49567_mg_L, PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg_L, na.rm = F),
             #                          PHOSPHORUS_mg_L), # should the order really be total, ortho, then dissolved???
             # 
             
             TSS_mg_L = coalesce(TOTAL_SUSPENDED_SOLIDS_00530_mg_L, `SSC-TOTAL_00530_mg_L`, TOTAL_SUSPENDED_SOLIDS_TSS45_mg_L),
             # Chloride logic not spelled out in Roger's code but how his scripts work
             CHLORIDE_mg_L = coalesce(CHLORIDE_TOTAL_00940_mg_L, CHLORIDE_DISSOLVED_00941_mg_L), 
             CHLORIDE_mg_L = ifelse(!is.na(CHLORIDE_DISSOLVED_00941_mg_L) && !is.na(CHLORIDE_TOTAL_00940_mg_L) &&
                                      CHLORIDE_DISSOLVED_00941_mg_L > CHLORIDE_TOTAL_00940_mg_L, CHLORIDE_DISSOLVED_00941_mg_L, CHLORIDE_mg_L),
             # Sulfate logic not spelled out in Roger's code but how his scripts work
             SULFATE_mg_L = coalesce(SULFATE_TOTAL_00945_mg_L, SULFATE_DISSOLVED_00946_mg_L), 
             SULFATE_mg_L = ifelse(!is.na(SULFATE_DISSOLVED_00946_mg_L) && !is.na(SULFATE_TOTAL_00945_mg_L) &&
                                      SULFATE_DISSOLVED_00946_mg_L > SULFATE_TOTAL_00945_mg_L, SULFATE_DISSOLVED_00946_mg_L, SULFATE_mg_L),
             
             # EVJ added these even though not called for in this part of conventionals, no idea where comes from otherwise
             SULFATE_TOTAL_mg_L = SULFATE_TOTAL_00945_mg_L,
             SULFATE_DISS_mg_L = SULFATE_DISSOLVED_00946_mg_L,
             ECOLI_31648_NO_100mL = `E._COLI_31648_NO_100mL`,
             ENTEROCOCCI = ENTEROCOCCI_31649_NO_100mL,
             FECAL_COLI = FECAL_COLIFORM_31616_NO_100mL,
             CHLOROPHYLL_A_ug_L = CHLOROPHYLL_32211_ug_L,
             SSC_mg_L = `SSC-TOTAL_00530_mg_L`  ) %>% 
      # now limit to just necessary columns for Conventionals format
      dplyr::select(#Ana_Id, 
        Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc,
        NITROGEN_mg_L,  AMMONIA_mg_L, NITRATE_mg_L, NOX_mg_L, NITROGEN_TOTAL_00600_mg_L, NITROGEN_AMMONIA_DISSOLVED_00608_mg_L,
        NITROGEN_AMMONIA_TOTAL_00610_mg_L, NITROGEN_NITRITE_DISSOLVED_00613_mg_L, NITROGEN_NITRITE_TOTAL_00615_mg_L,
        NITROGEN_NITRATE_DISSOLVED_00618_mg_L, NITROGEN_NITRATE_TOTAL_00620_mg_L, NITROGEN_KJELDAHL_TOTAL_00625_mg_L,
        `NITRITE+NITRATE_TOTAL_00630_mg_L`, `NITRITE+NITRATE_DISSOLVED_00631_mg_L`, NITROGEN_PARTICULATE_49570_mg_L,
        NITROGEN_TOTAL_DISSOLVED_49571_mg_L, NITROGEN_TOTAL_DISSOLVED_TDNLF_mg_L, PHOSPHORUS_mg_L, PHOSPHORUS_TOTAL_00665_mg_L, 
        PHOSPHORUS_DISSOLVED_00666_mg_L, PHOSPHORUS_DISSOLVED_ORTHOPHOSPHATE_00671_mg_L, PHOSPHOROUS_PARTICULATE_49567_mg_L,
        PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg_L, PHOSPHORUS_TOTAL_ORTHOPHOSPHATE_70507_mg_L, ORTHOPHOSPHATE_DISSOLVED_OPWLF_mg_L, 
        PHOSPHORUS_SUSPENDED_INORGANIC_PIPLF_mg_L, PHOSPHORUS_PARTICULATE_PPWLF_mg_L, PHOSPHORUS_TOTAL_DISSOLVED_TDPLF_mg_L,
        HARDNESS_TOTAL_00900_mg_L, CHLORIDE_mg_L, CHLORIDE_TOTAL_00940_mg_L, CHLORIDE_DISSOLVED_00941_mg_L,
        SULFATE_mg_L, SULFATE_TOTAL_mg_L, SULFATE_DISS_mg_L, ECOLI, ECOLI_31648_NO_100mL, ENTEROCOCCI, FECAL_COLI, 
        CHLOROPHYLL_A_ug_L, TSS_mg_L, TOTAL_SUSPENDED_SOLIDS_00530_mg_L, SSC_mg_L, TOTAL_SUSPENDED_SOLIDS_TSS45_mg_L) %>% 
      filter(! is.na(Ana_Sam_Fdt_Id)) #%>%  # drop blank row from row_bind
      # double check things
      #left_join(dplyr::select(stationFieldDataUserFilter1, FDT_ID:FDT_DEPTH_DESC), by = c("Ana_Sam_Fdt_Id"= "FDT_ID")) %>% 
      #dplyr::select(FDT_STA_ID:FDT_DEPTH_DESC, everything())
    
    
    # Step 3.4: Organize lab remark fields
    # now fix remark fields
    stationAnalyteDataUserFilter2 <- bind_rows(remarkTemplate, 
                                               suppressWarnings(
                                                 stationAnalyteDataUserFilter0_0 %>% 
                                                   #stationAnalyteDataUserFilter %>% 
                                                   
                                                   # only keep codes we will do something with to avoid unnecessary data duplication
                                                   
                                                   filter(Pg_Storet_Code %in% c('00530', '00600', '00608', '00610', '00613' ,'00615', '00618', '00620',
                                                                                '00625', '00630', '00631', '00665', '00666', '00671', '00900', '00940',
                                                                                '00941', '00945', '00946', '31616', '31648', '31649', '32211', '49567',
                                                                                '49570', '49571', '49572', '70507', 'ECOLI', 'OPWLF', 'PIPLF', 'PPWLF',
                                                                                'SSC-TOTAL', 'TDNLF', 'TDPLF', 'TSS45')) %>% 
                                                   
                                                   dplyr::select(Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc, Pg_Storet_Code, Ana_Uncensored_Val_Comment) %>% 
                                                   pivot_wider(id_cols = Ana_Sam_Fdt_Id, 
                                                               names_from = 'Pg_Storet_Code', names_prefix = "RMK_", values_from = 'Ana_Uncensored_Val_Comment', values_fn = list()) %>% 
                                                   unnest(-Ana_Sam_Fdt_Id)) ) %>% 
      mutate(RMK_ECOLI = coalesce(`RMK_ECOLI`, `RMK_31648`),
             RMK_NITROGEN = coalesce(RMK_49570, RMK_49571), # what about NITROGEN_TOTAL_00600_mg_L remarks????
             RMK_NOX = coalesce(`RMK_00630`, RMK_00615, RMK_00620), # not in conventionals query, EVJ added logic
             RMK_AMMONIA = coalesce(RMK_00610, RMK_00608),
             RMK_NITRATE = coalesce(RMK_00620, RMK_00618),
             RMK_PHOSPHORUS = coalesce(RMK_00665, RMK_70507, RMK_00666, RMK_00671, 
                                       RMK_49567, RMK_49572), # don't love the logic on this one, double check me
             RMK_TSS = coalesce(RMK_00530, RMK_SSC_TOTAL_00530, RMK_TSS45),
             RMK_CHLORIDE = coalesce(RMK_00940, RMK_00941), # placeholder for now, fixed once combined with parameter data
             RMK_SULFATE = coalesce(RMK_00945, RMK_00946), # placeholder for now, fixed once combined with parameter data
             
             # EVJ added these even though not called for in this part of conventionals, no idea where comes from otherwise
             RMK_SULFATE_TOTAL = RMK_00945,
             RMK_SULFATE_DISS = RMK_00946,
             RMK_ENTEROCOCCI = RMK_31649,
             RMK_FECAL_COLI = RMK_31616,
             RMK_CHLOROPHYLL_A = RMK_32211,
             RMK_SSC = RMK_00530,
             LEVEL_NITROGEN = as.character(NA), LEVEL_AMMONIA = as.character(NA), LEVEL_NITRATE = as.character(NA),
             LEVEL_NOX = as.character(NA), LEVEL_00600 = as.character(NA), LEVEL_00608 = as.character(NA),
             LEVEL_00610 = as.character(NA), LEVEL_00613 = as.character(NA), LEVEL_00615 = as.character(NA),
             LEVEL_00618 = as.character(NA), LEVEL_00620 = as.character(NA), LEVEL_00625 = as.character(NA),
             LEVEL_00630 = as.character(NA), LEVEL_00631 = as.character(NA), LEVEL_49570 = as.character(NA), 
             LEVEL_49571 = as.character(NA), LEVEL_TDNLF = as.character(NA), LEVEL_PHOSPHORUS = as.character(NA), 
             LEVEL_00665 = as.character(NA), LEVEL_00666 = as.character(NA), LEVEL_00671 = as.character(NA),
             LEVEL_49567 = as.character(NA), LEVEL_49572 = as.character(NA), LEVEL_70507 = as.character(NA),
             LEVEL_OPWLF = as.character(NA), LEVEL_PIPLF = as.character(NA), LEVEL_PPWLF = as.character(NA),
             LEVEL_TDPLF = as.character(NA), LEVEL_00900 = as.character(NA), LEVEL_CHLORIDE = as.character(NA),
             LEVEL_00940 = as.character(NA), LEVEL_00941 = as.character(NA), LEVEL_SULFATE = as.character(NA),
             LEVEL_SULFATE_TOTAL = as.character(NA), LEVEL_SULFATE_DISS = as.character(NA), LEVEL_ECOLI = as.character(NA),
             LEVEL_31648 = as.character(NA), LEVEL_ENTEROCOCCI = as.character(NA), LEVEL_FECAL_COLI = as.character(NA),
             LEVEL_CHLOROPHYLL_A = as.character(NA), LEVEL_TSS = as.character(NA), LEVEL_00530 = as.character(NA),
             LEVEL_SSC = as.character(NA), LEVEL_TSS45 = as.character(NA)) %>% 
      dplyr::select(#Ana_Id, 
        Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc,
        RMK_NITROGEN, LEVEL_NITROGEN, RMK_AMMONIA, LEVEL_AMMONIA, RMK_NITRATE, LEVEL_NITRATE,
        RMK_NOX, LEVEL_NOX, RMK_00600, LEVEL_00600, RMK_00608, LEVEL_00608,
        RMK_00610, LEVEL_00610, RMK_00613, LEVEL_00613, RMK_00615, LEVEL_00615, RMK_00618, LEVEL_00618,
        RMK_00620, LEVEL_00620, RMK_00625, LEVEL_00625, RMK_00630, LEVEL_00630, RMK_00631, LEVEL_00631, RMK_49570, LEVEL_49570,
        RMK_49571, LEVEL_49571, RMK_TDNLF, LEVEL_TDNLF, RMK_PHOSPHORUS, LEVEL_PHOSPHORUS, RMK_00665, LEVEL_00665,
        RMK_00666, LEVEL_00666, RMK_00671, LEVEL_00671, RMK_49567, LEVEL_49567, RMK_49572, LEVEL_49572, RMK_70507, 
        LEVEL_70507, RMK_OPWLF, LEVEL_OPWLF, RMK_PIPLF, LEVEL_PIPLF, RMK_PPWLF, LEVEL_PPWLF, RMK_TDPLF, LEVEL_TDPLF,
        RMK_00900, LEVEL_00900, RMK_CHLORIDE, LEVEL_CHLORIDE, RMK_00940, LEVEL_00940, RMK_00941, LEVEL_00941,
        RMK_SULFATE, LEVEL_SULFATE, RMK_SULFATE_TOTAL, LEVEL_SULFATE_TOTAL, RMK_SULFATE_DISS, LEVEL_SULFATE_DISS,
        RMK_ECOLI, LEVEL_ECOLI, RMK_31648, LEVEL_31648, RMK_ENTEROCOCCI, LEVEL_ENTEROCOCCI, RMK_FECAL_COLI, LEVEL_FECAL_COLI,
        RMK_CHLOROPHYLL_A, LEVEL_CHLOROPHYLL_A, RMK_TSS, LEVEL_TSS, RMK_00530, LEVEL_00530, RMK_SSC, LEVEL_SSC,
        RMK_TSS45, LEVEL_TSS45 ) %>% 
      filter(! is.na(Ana_Sam_Fdt_Id)) # drop blank row from row_bind
      # double check things
      #left_join(dplyr::select(stationFieldDataUserFilter1, FDT_ID:FDT_DEPTH_DESC), by = c("Ana_Sam_Fdt_Id"= "FDT_ID")) %>% 
      #dplyr::select(FDT_STA_ID:FDT_DEPTH_DESC, everything())
    
    
      
    # Step 3.5: Combine analyte data and remarks (and empty Level fields)
    stationAnalyteDataUserFilter3 <- left_join(stationAnalyteDataUserFilter1, stationAnalyteDataUserFilter2,
                                               by = c(#"Ana_Id", 
                                                 "Ana_Sam_Fdt_Id", "Ana_Sam_Mrs_Container_Id_Desc")) %>% 
      # secondary adjustment based on remark codes when combined with remarks
      mutate(
        AMMONIA_mg_L = ifelse(!is.na(NITROGEN_AMMONIA_DISSOLVED_00608_mg_L) &&
                                NITROGEN_AMMONIA_DISSOLVED_00608_mg_L > NITROGEN_AMMONIA_TOTAL_00610_mg_L && 
                                RMK_00610 %in% c('U','QQ') && is.na(RMK_00608), NITROGEN_AMMONIA_DISSOLVED_00608_mg_L, AMMONIA_mg_L),
        RMK_AMMONIA = ifelse(!is.na(NITROGEN_AMMONIA_DISSOLVED_00608_mg_L) &&
                               NITROGEN_AMMONIA_DISSOLVED_00608_mg_L > NITROGEN_AMMONIA_TOTAL_00610_mg_L && 
                               RMK_00610 %in% c('U','QQ'),
                             RMK_00608, RMK_AMMONIA),
        RMK_CHLORIDE = ifelse(!is.na(CHLORIDE_DISSOLVED_00941_mg_L) && !is.na(CHLORIDE_TOTAL_00940_mg_L) &&
                                CHLORIDE_DISSOLVED_00941_mg_L > CHLORIDE_TOTAL_00940_mg_L, RMK_00941, RMK_00940), # fix from above
        RMK_SULFATE =  ifelse(!is.na(SULFATE_DISS_mg_L) && !is.na(SULFATE_TOTAL_mg_L) &&  # conventionals changes these names for some reason: SULFATE_TOTAL_mg_L = SULFATE_TOTAL_00945_mg_L;  SULFATE_DISS_mg_L = SULFATE_DISSOLVED_00946_mg_L,
                                SULFATE_DISS_mg_L > SULFATE_TOTAL_mg_L, RMK_SULFATE_DISS, RMK_SULFATE_TOTAL)) %>%  # fix from above; # conventionals changes these names for some reason: RMK_SULFATE_DISS = RMK_00946; RMK_SULFATE_TOTAL = RMK_00945
      dplyr::select(#Ana_Id, # maybe don't need?
        Ana_Sam_Fdt_Id, Ana_Sam_Mrs_Container_Id_Desc,
        NITROGEN_mg_L, RMK_NITROGEN, LEVEL_NITROGEN, AMMONIA_mg_L, 
        RMK_AMMONIA, LEVEL_AMMONIA, NITRATE_mg_L, RMK_NITRATE, LEVEL_NITRATE, NOX_mg_L, RMK_NOX, LEVEL_NOX, NITROGEN_TOTAL_00600_mg_L, 
        RMK_00600, LEVEL_00600, NITROGEN_AMMONIA_DISSOLVED_00608_mg_L, RMK_00608, LEVEL_00608, NITROGEN_AMMONIA_TOTAL_00610_mg_L, 
        RMK_00610, LEVEL_00610, NITROGEN_NITRITE_DISSOLVED_00613_mg_L, RMK_00613, LEVEL_00613, NITROGEN_NITRITE_TOTAL_00615_mg_L, 
        RMK_00615, LEVEL_00615, NITROGEN_NITRATE_DISSOLVED_00618_mg_L, RMK_00618, LEVEL_00618, NITROGEN_NITRATE_TOTAL_00620_mg_L, 
        RMK_00620, LEVEL_00620, NITROGEN_KJELDAHL_TOTAL_00625_mg_L, RMK_00625, LEVEL_00625, `NITRITE+NITRATE_TOTAL_00630_mg_L`, RMK_00630, 
        LEVEL_00630, `NITRITE+NITRATE_DISSOLVED_00631_mg_L`, RMK_00631, LEVEL_00631, NITROGEN_PARTICULATE_49570_mg_L, RMK_49570, LEVEL_49570, 
        NITROGEN_TOTAL_DISSOLVED_49571_mg_L, RMK_49571, LEVEL_49571, NITROGEN_TOTAL_DISSOLVED_TDNLF_mg_L, RMK_TDNLF, LEVEL_TDNLF, 
        PHOSPHORUS_mg_L, RMK_PHOSPHORUS, LEVEL_PHOSPHORUS, PHOSPHORUS_TOTAL_00665_mg_L, RMK_00665, LEVEL_00665, 
        PHOSPHORUS_DISSOLVED_00666_mg_L, RMK_00666, LEVEL_00666, PHOSPHORUS_DISSOLVED_ORTHOPHOSPHATE_00671_mg_L, RMK_00671, LEVEL_00671, 
        PHOSPHOROUS_PARTICULATE_49567_mg_L, RMK_49567, LEVEL_49567, PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg_L, RMK_49572, LEVEL_49572, 
        PHOSPHORUS_TOTAL_ORTHOPHOSPHATE_70507_mg_L, RMK_70507, LEVEL_70507, ORTHOPHOSPHATE_DISSOLVED_OPWLF_mg_L, RMK_OPWLF, LEVEL_OPWLF, 
        PHOSPHORUS_SUSPENDED_INORGANIC_PIPLF_mg_L, RMK_PIPLF, LEVEL_PIPLF, PHOSPHORUS_PARTICULATE_PPWLF_mg_L, RMK_PPWLF, LEVEL_PPWLF, 
        PHOSPHORUS_TOTAL_DISSOLVED_TDPLF_mg_L, RMK_TDPLF, LEVEL_TDPLF, HARDNESS_TOTAL_00900_mg_L, RMK_00900, LEVEL_00900, CHLORIDE_mg_L, 
        RMK_CHLORIDE, LEVEL_CHLORIDE, CHLORIDE_TOTAL_00940_mg_L, RMK_00940, LEVEL_00940, CHLORIDE_DISSOLVED_00941_mg_L, RMK_00941, 
        LEVEL_00941, SULFATE_mg_L, RMK_SULFATE, LEVEL_SULFATE, SULFATE_TOTAL_mg_L, RMK_SULFATE_TOTAL, LEVEL_SULFATE_TOTAL, 
        SULFATE_DISS_mg_L, RMK_SULFATE_DISS, LEVEL_SULFATE_DISS, ECOLI, RMK_ECOLI, LEVEL_ECOLI, ECOLI_31648_NO_100mL, RMK_31648, 
        LEVEL_31648, ENTEROCOCCI, RMK_ENTEROCOCCI, LEVEL_ENTEROCOCCI, FECAL_COLI, RMK_FECAL_COLI, LEVEL_FECAL_COLI, CHLOROPHYLL_A_ug_L, 
        RMK_CHLOROPHYLL_A, LEVEL_CHLOROPHYLL_A, TSS_mg_L, RMK_TSS, LEVEL_TSS, TOTAL_SUSPENDED_SOLIDS_00530_mg_L, RMK_00530, LEVEL_00530, 
        SSC_mg_L, RMK_SSC, LEVEL_SSC, TOTAL_SUSPENDED_SOLIDS_TSS45_mg_L, RMK_TSS45, LEVEL_TSS45) #%>% 
      # QUick double check
      # left_join(dplyr::select(stationFieldDataUserFilter1, FDT_ID:FDT_DEPTH_DESC), by = c("Ana_Sam_Fdt_Id"= "FDT_ID")) %>% 
      # dplyr::select(FDT_STA_ID:FDT_DEPTH_DESC, everything())
      # 
    
    
    # Step 4: Combine field and analyte data
    # use left join here bc stationFieldDataUserFilter1 removes any unwanted sample types (incident response, facility data)
    combo <- left_join(stationFieldDataUserFilter1, stationAnalyteDataUserFilter3, by = c("FDT_ID" = "Ana_Sam_Fdt_Id")) %>% 
      dplyr::select(-FDT_ID) # no longer needed
    
    # Step 5: Combine station and combo data
    combo2 <- full_join(stationData, combo, by = 'FDT_STA_ID') %>% 
      dplyr::select(names(conventionals))
        
    return(combo2)
    
  } else {
    return(conventionals)
  }
}
    
# 
# ## Function Demo
# library(tidyverse)
# library(config)
# library(sf)
# library(lubridate)
# library(pool)
# library(pins)
# library(dbplyr)
# 
# 
# # Server connection things
# conn <- config::get("connectionSettings") # get configuration settings
# 
# # set up pool
# pool <- dbPool(
#   drv = odbc::odbc(),
#   Driver = "ODBC Driver 11 for SQL Server",#"SQL Server Native Client 11.0",
#   Server= "DEQ-SQLODS-PROD,50000",
#   dbname = "ODS",
#   trusted_connection = "yes"
# )
# 

# single station
# station <- '2-JKS023.61'
# dateRange <- c(as.Date('2015-01-01'), as.Date('2021-01-01'))
# stationFieldData <- pool %>% tbl(in_schema("wqm", "Wqm_Field_Data_View")) %>%
#   filter(Fdt_Sta_Id %in% !! station &
#            between(as.Date(Fdt_Date_Time), !! dateRange[1], !! dateRange[2]) ) %>% 
#   as_tibble() %>% 
#   filter(! Ssc_Description %in% "INVALID DATA SET QUALITY ASSURANCE FAILURE")
# stationAnalyteData <- pool %>% tbl(in_schema("wqm", "Wqm_Analytes_View")) %>%
#   filter(Ana_Sam_Fdt_Id %in% !! stationFieldData$Fdt_Id  &
#            between(as.Date(Ana_Received_Date), !! dateRange[1], !! dateRange[2]) & # x >= left & x <= right
#            Pg_Parm_Name != "STORET STORAGE TRANSACTION DATE YR/MO/DAY") %>% 
#   as_tibble() %>%
#   left_join(dplyr::select(stationFieldData, Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time), by = c("Ana_Sam_Fdt_Id" = "Fdt_Id"))
# stationInfo <- pool %>% tbl(in_schema("wqm",  "Wqm_Stations_View")) %>%
#   filter(Sta_Id %in% !! toupper(station)) %>%
#   as_tibble()
# stationGIS_View <-  pool %>% tbl(in_schema("wqm",  "Wqm_Sta_GIS_View")) %>%
#   filter(Station_Id %in% !! toupper(station)) %>%
#   as_tibble()
# 
# zz <- conventionalsSummary(conventionals = pin_get("conventionals2022IRfinalWithSecchi", board = "rsconnect")[0,],
#                            stationFieldData,
#                            stationAnalyteData,
#                            stationInfo, 
#                            stationGIS_View, 
#                            dropCodes = c('QF'))


# multistation
# station <- pin_get("conventionals2022IRfinalWithSecchi", board = "rsconnect") %>% 
#   filter(STA_REC_CODE == 'BRRO') %>% 
#   distinct(FDT_STA_ID)
# station <- station$FDT_STA_ID[1:100]
# 
# 
# dateRange <- c(as.Date('2015-01-01'), as.Date('2020-12-31'))
# stationFieldData <- pool %>% tbl(in_schema("wqm", "Wqm_Field_Data_View")) %>%
#   filter(Fdt_Sta_Id %in% !! station &
#            between(as.Date(Fdt_Date_Time), !! dateRange[1], !! dateRange[2]) ) %>%
#   as_tibble() %>%
#   filter(! Ssc_Description %in% "INVALID DATA SET QUALITY ASSURANCE FAILURE")
# stationAnalyteData <- pool %>% tbl(in_schema("wqm", "Wqm_Analytes_View")) %>%
#   filter(Ana_Sam_Fdt_Id %in% !! stationFieldData$Fdt_Id &
#            #between(as.Date(Ana_Received_Date), !! dateRange[1], !! dateRange[2]) & # x >= left & x <= right
#            Pg_Parm_Name != "STORET STORAGE TRANSACTION DATE YR/MO/DAY") %>%
#   as_tibble() %>%
#   left_join(dplyr::select(stationFieldData, Fdt_Id, Fdt_Sta_Id, Fdt_Date_Time), by = c("Ana_Sam_Fdt_Id" = "Fdt_Id"))
# stationInfo <- pool %>% tbl(in_schema("wqm",  "Wqm_Stations_View")) %>%
#   filter(Sta_Id %in% !! toupper(station)) %>%
#   as_tibble()
# stationGIS_View <-  pool %>% tbl(in_schema("wqm",  "Wqm_Sta_GIS_View")) %>%
#   filter(Station_Id %in% !! toupper(station)) %>%
#   as_tibble()
# zz <- conventionalsSummary(conventionals= pin_get("conventionals2022IRfinalWithSecchi", board = "rsconnect")[0,],
#                            stationFieldDataUserFilter= stationFieldData, stationAnalyteDataUserFilter = stationAnalyteData,
#                            stationInfo,
#                            stationGIS_View,
#                            dropCodes = c('QF'))