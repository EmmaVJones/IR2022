# Bring in final 2020IR stations table and limit to just citmon and non agency sites
BRRO <- read_csv('data/final2020data/RegionalResultsRiverine_BRROCitMonNonAgencyFINAL.csv') %>%
  bind_rows(read_csv('data/final2020data/lakeStations2020_BRRO_citmonNonAgency.csv')) %>% 
  filter(STATION_TYPE_1 %in% c( "CMON", "NONA", "USPS") |
           STATION_TYPE_2 %in% c( "CMON", "NONA", "USPS") |
           STATION_TYPE_3 %in% c( "CMON", "NONA", "USPS"))

# Bring in final 2020 IR conventionals dataset that includes BRRO citmon/non agency data organized

ir2020 <- read_csv('data/final2020data/conventionals_final2020_citmonNonAgency.csv') %>% 
  # just get citmon and non agency data
  filter(FDT_STA_ID %in% BRRO$FDT_STA_ID) %>% 
  # and only in current IR window
  filter(as.Date(FDT_DATE_TIME) >= as.Date('2015-01-01') &
           as.Date(FDT_DATE_TIME) <= as.Date('2020-12-31')) %>%
  # make old format match new
  mutate(GROUP_STA_ID = as.character(NA)) %>% 
  dplyr::select(FDT_STA_ID, GROUP_STA_ID, everything()) %>% 
  # remove duplicate ecoli
  dplyr::select(-c(`E.COLI_ECOLI_CFU/100mL`,	RMK_ECOLI)) %>%
  # change to naming system 
  mutate(FDT_TEMP_CELCIUS  = `TEMPERATURE_00010_DEGREES CENTIGRADE`,
         RMK_FDT_TEMP_CELCIUS = FDT_TEMP_CELCIUS_RMK,  
         LEVEL_FDT_TEMP_CELCIUS  = as.factor(NA),
         FDT_FIELD_PH = pH_00400_STD_UNITS ,          
         RMK_FDT_FIELD_PH  =FDT_FIELD_PH_RMK, 
         LEVEL_FDT_FIELD_PH = as.factor(NA),
         DO_mg_L =  `DO_mg/L`,       
         RMK_DO  =DO_RMK,    
         LEVEL_DO = as.factor(NA),
         DISSOLVED_OXYGEN_00300_mg_L = `DISSOLVED_OXYGEN_00300_mg/L`,
         RMK_FDT_DO_PROBE = FDT_DO_PROBE_RMK,
         LEVEL_FDT_DO_PROBE = as.factor(NA),
         DISSOLVED_OXYGEN_DOOPT_mg_L = `DISSOLVED_OXYGEN_DOOPT_mg/L`,
         RMK_FDT_DO_OPTICAL = FDT_DO_OPTICAL_RMK,
         LEVEL_FDT_DO_OPTICAL = as.factor(NA),
         DISSOLVED_OXYGEN_WINK_mg_L = `DISSOLVED_OXYGEN_WINK_mg/L`,
         RMK_FDT_DO_WINKLER = FDT_DO_WINKLER_RMK,
         LEVEL_FDT_DO_WINKLER = as.factor(NA), 
         FDT_SPECIFIC_CONDUCTANCE=`SPECIFIC_CONDUCTANCE_00095_UMHOS/CM @ 25C`,    
         RMK_FDT_SPECIFIC_CONDUCTANCE =FDT_SPECIFIC_CONDUCTANCE_RMK ,
         LEVEL_FDT_SPECIFIC_CONDUCTANCE = as.factor(NA),
         FDT_SALINITY = SALINITY_00480_PPT ,            
         RMK_FDT_SALINITY  =FDT_SALINITY_RMK, 
         LEVEL_FDT_SALINITY = as.factor(NA), 
         NITROGEN_mg_L = `NITROGEN_mg/L` ,  
         LEVEL_NITROGEN = as.factor(NA),
         AMMONIA_mg_L =`AMMONIA_mg/L`,
         LEVEL_AMMONIA = as.factor(NA),
         NITRATE_mg_L = `NITRATE_mg/L`,
         LEVEL_NITRATE = as.factor(NA),
         NOX_mg_L = `NOX_mg/L`,
         LEVEL_NOX = as.factor(NA), 
         NITROGEN_TOTAL_00600_mg_L = `NITROGEN_TOTAL_00600_mg/L`,
         LEVEL_00600 = as.factor(NA),
         NITROGEN_AMMONIA_DISSOLVED_00608_mg_L = `NITROGEN_AMMONIA_DISSOLVED_00608_mg/L`,
         LEVEL_00608 = as.factor(NA),
         NITROGEN_AMMONIA_TOTAL_00610_mg_L = `NITROGEN_AMMONIA_TOTAL_00610_mg/L`,
         LEVEL_00610 = as.factor(NA), 
         NITROGEN_NITRITE_DISSOLVED_00613_mg_L = `NITROGEN_NITRITE_DISSOLVED_00613_mg/L`,
         LEVEL_00613 = as.factor(NA), 
         NITROGEN_NITRITE_TOTAL_00615_mg_L = `NITROGEN_NITRITE_TOTAL_00615_mg/L`,
         LEVEL_00615 = as.factor(NA), 
         NITROGEN_NITRATE_DISSOLVED_00618_mg_L = `NITROGEN_NITRATE_DISSOLVED_00618_mg/L`,
         LEVEL_00618 = as.factor(NA), 
         NITROGEN_NITRATE_TOTAL_00620_mg_L = `NITROGEN_NITRATE_TOTAL_00620_mg/L`,
         LEVEL_00620 = as.factor(NA),
         NITROGEN_KJELDAHL_TOTAL_00625_mg_L = `NITROGEN_KJELDAHL_TOTAL_00625_mg/L`,
         LEVEL_00625 = as.factor(NA),
         `NITRITE+NITRATE_TOTAL_00630_mg_L` = `NITRITE+NITRATE_TOTAL_00630_mg/L`,
         LEVEL_00630 = as.factor(NA),
         `NITRITE+NITRATE_DISSOLVED_00631_mg_L` = `NITRITE+NITRATE_DISSOLVED_00631_mg/L`,
         LEVEL_00631 = as.factor(NA), 
         NITROGEN_PARTICULATE_49570_mg_L = `NITROGEN_PARTICULATE_49570_mg/L`,
         LEVEL_49570 = as.factor(NA),
         NITROGEN_TOTAL_DISSOLVED_49571_mg_L = `NITROGEN_TOTAL_DISSOLVED_49571_mg/L`,
         LEVEL_49571 = as.factor(NA),
         NITROGEN_TOTAL_DISSOLVED_TDNLF_mg_L = `NITROGEN_TOTAL_DISSOLVED_TDNLF_mg/L`,
         LEVEL_TDNLF = as.factor(NA),
         PHOSPHORUS_mg_L =  `PHOSPHORUS_mg/L`,
         LEVEL_PHOSPHORUS = as.factor(NA),
         PHOSPHORUS_TOTAL_00665_mg_L = `PHOSPHORUS_TOTAL_00665_mg/L`,
         LEVEL_00665 = as.factor(NA),
         PHOSPHORUS_DISSOLVED_00666_mg_L = `PHOSPHORUS_DISSOLVED_00666_mg/L`,
         LEVEL_00666 = as.factor(NA), 
         PHOSPHORUS_DISSOLVED_ORTHOPHOSPHATE_00671_mg_L = `PHOSPHORUS_DISSOLVED_ORTHOPHOSPHATE_00671_mg/L`,
         LEVEL_00671 = as.factor(NA),
         PHOSPHOROUS_PARTICULATE_49567_mg_L = `PHOSPHOROUS_PARTICULATE_49567_mg/L`,
         LEVEL_49567 = as.factor(NA), 
         PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg_L = `PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg/L`,
         LEVEL_49572 = as.factor(NA),
         PHOSPHORUS_TOTAL_ORTHOPHOSPHATE_70507_mg_L = `PHOSPHORUS_TOTAL_ORTHOPHOSPHATE_70507_mg/L`,
         LEVEL_70507 = as.factor(NA),
         ORTHOPHOSPHATE_DISSOLVED_OPWLF_mg_L = `ORTHOPHOSPHATE_DISSOLVED_OPWLF_mg/L`,
         LEVEL_OPWLF = as.factor(NA),
         PHOSPHORUS_SUSPENDED_INORGANIC_PIPLF_mg_L = `PHOSPHORUS_SUSPENDED_INORGANIC_PIPLF_mg/L`,
         LEVEL_PIPLF = as.factor(NA),
         PHOSPHORUS_PARTICULATE_PPWLF_mg_L = `PHOSPHORUS_PARTICULATE_PPWLF_mg/L`,
         LEVEL_PPWLF = as.factor(NA),
         PHOSPHORUS_TOTAL_DISSOLVED_TDPLF_mg_L = `PHOSPHORUS_TOTAL_DISSOLVED_TDPLF_mg/L`,
         LEVEL_TDPLF = as.factor(NA), 
         HARDNESS_TOTAL_00900_mg_L = `HARDNESS_TOTAL_00900_mg/L`,
         LEVEL_00900 = as.factor(NA), 
         CHLORIDE_mg_L = `CHLORIDE_mg/L`,
         LEVEL_CHLORIDE = as.factor(NA), 
         CHLORIDE_TOTAL_00940_mg_L = `CHLORIDE_TOTAL_00940_mg/L`,
         LEVEL_00940 = as.factor(NA), 
         CHLORIDE_DISSOLVED_00941_mg_L = `CHLORIDE_DISSOLVED_00941_mg/L`,
         LEVEL_00941 = as.factor(NA),
         SULFATE_mg_L = `SULFATE_mg/L`,
         LEVEL_SULFATE = as.factor(NA),
         SULFATE_TOTAL_mg_L = `SULFATE_TOTAL_00945_mg/L`,
         RMK_SULFATE_TOTAL = RMK_00945,
         LEVEL_SULFATE_TOTAL = as.factor(NA),
         SULFATE_DISS_mg_L = `SULFATE_DISSOLVED_00946_mg/L`,
         RMK_SULFATE_DISS = RMK_00946,
         LEVEL_SULFATE_DISS = as.factor(NA),
         ECOLI = `ECOLI_CFU/100mL`,  
         RMK_ECOLI = ECOLI_RMK,
         LEVEL_ECOLI = as.factor(NA),
         ECOLI_31648_NO_100mL = as.numeric(NA), #`E._COLI_31648_NO/100mL`,
         LEVEL_31648 = as.factor(NA),
         RMK_31648 = as.character(NA),
         ENTEROCOCCI = `ENTEROCOCCI_31649_NO/100mL`,
         RMK_ENTEROCOCCI =  RMK_31649,
         LEVEL_ENTEROCOCCI = as.factor(NA), 
         FECAL_COLI = `FECAL_COLIFORM_31616_NO/100mL` ,
         RMK_FECAL_COLI = RMK_31616,
         LEVEL_FECAL_COLI = as.factor(NA),
         CHLOROPHYLL_A_ug_L =`CHLOROPHYLL_32211_ug/L`, 
         RMK_CHLOROPHYLL_A = RMK_32211,
         LEVEL_CHLOROPHYLL_A = as.factor(NA), 
         TSS_mg_L = `TSS_mg/L`,
         LEVEL_TSS = as.factor(NA),
         TOTAL_SUSPENDED_SOLIDS_00530_mg_L = `TOTAL_SUSPENDED_SOLIDS_00530_mg/L`,
         LEVEL_00530 = as.factor(NA),
         SSC_mg_L =`SSC-TOTAL_00530_mg/L`,
         RMK_SSC = `RMK_SSC-TOTAL`,
         LEVEL_SSC = as.factor(NA), 
         TOTAL_SUSPENDED_SOLIDS_TSS45_mg_L = `TOTAL_SUSPENDED_SOLIDS_TSS45_mg/L`,
         LEVEL_TSS45 = as.factor(NA)) %>%
  mutate(FDT_DATE_TIME = as.POSIXct(FDT_DATE_TIME, format="%m/%d/%Y %H:%M"), # fix date time
         GROUP_STA_ID = as.character(NA),  #Source_Sta_Id = NA,
         OTHER_CITMON_NONAGENCY_INFO = as.character(NA), 
         Data_Source = "DEQ",
         Waterbody = as.character(NA), 
         SECCHI_DEPTH_M = as.numeric(NA),
         RMK_SECCHI_DEPTH = as.character(NA), 
         LEVEL_SECCHI_DEPTH = as.factor(NA)) %>%
  dplyr::select(names(conventionals))

# double check dates are in range
summary(ir2020$FDT_DATE_TIME)
 
conventionals <- bind_rows( conventionals, ir2020)


rm(ir2020)
