# Update conventionals schema to match new citmon/non agency data schema and 
#  improve data quality. This will be the format the IR2022 (and beyond) scripts
#  use and the format the R generated 'conventionals' data pull produces.

#library(tidyverse)
#library(readxl)

# Read in new schema from Roland
schema <- read_excel('data/citMonConventionalsSchema.xlsx') %>% # email 12/2/20
  drop_na() %>% 
  separate(`Column Name`, c('Name','Type','Value'), sep =" ", extra = "merge") %>%
  mutate(Value = case_when(str_detect(Type, "nvarchar") ~ 'character',
                           str_detect(Type, 'datetime') ~ "02/06/2015 09:18",
                           str_detect(Type, 'decimal') ~ "5.5",
                           str_detect(Type, 'int') ~ "5.5",
                           TRUE ~ Type),
         Name = case_when(str_detect(Type, "nvarchar") ~ paste0(Name, '.char'),
                          str_detect(Type, 'datetime') ~ paste0(Name, ".date"),
                          str_detect(Type, 'decimal')  ~ paste0(Name, ".num"),
                          str_detect(Type, 'int')  ~ paste0(Name, ".num"),  
                          TRUE ~ Type)) %>%
  dplyr::select(-Type) %>%
  pivot_wider(names_from = Name, values_from = Value) %>%
  mutate_at(vars(contains('.num')), as.numeric) %>% # change data format
  mutate_at(vars(contains('.char')), as.character) %>%# change data format
  rename_at(.vars = vars(ends_with(".num")),
            .funs = funs(sub("[.]num$", "", .))) %>%
  rename_at(.vars = vars(ends_with(".char")),
            .funs = funs(sub("[.]char$", "", .))) %>%
  rename_at(.vars = vars(ends_with(".date")),
            .funs = funs(sub("[.]date$", "", .))) %>%
  mutate(FDT_DATE_TIME = as.POSIXct(FDT_DATE_TIME, format="%m/%d/%Y %H:%M")) # change data format
#glimpse(schema)

schemaFin <- read_csv('data/citmonnonagencydummydata.csv')# email 1/8/21


# map new names
conventionals <- read_csv('data/final2020data/CEDSWQM_2020_IR_DATA-CONVENTIONALS_20190305.csv') %>%
  # remove duplicate ecoli
  dplyr::select(-c(`E.COLI_ECOLI_CFU/100mL`,	RMK_ECOLI)) %>%
  # change to naming system 
  rename("FDT_TEMP_CELCIUS"  ="TEMPERATURE_00010_DEGREES CENTIGRADE",
         "RMK_FDT_TEMP_CELCIUS" = "FDT_TEMP_CELCIUS_RMK",  
         "FDT_FIELD_PH" = "pH_00400_STD_UNITS" ,          
         "RMK_FDT_FIELD_PH"  ="FDT_FIELD_PH_RMK", 
         "DO_mg_L" =  "DO_mg/L",       
         "RMK_DO"  ="DO_RMK",    
         "DISSOLVED_OXYGEN_00300_mg_L" = "DISSOLVED_OXYGEN_00300_mg/L",
         "RMK_FDT_DO_PROBE" = "FDT_DO_PROBE_RMK",
         "DISSOLVED_OXYGEN_DOOPT_mg_L" = "DISSOLVED_OXYGEN_DOOPT_mg/L",
         "RMK_FDT_DO_OPTICAL" = "FDT_DO_OPTICAL_RMK",
         "DISSOLVED_OXYGEN_WINK_mg_L" = "DISSOLVED_OXYGEN_WINK_mg/L",
         "RMK_FDT_DO_WINKLER" = "FDT_DO_WINKLER_RMK",
         "FDT_SPECIFIC_CONDUCTANCE"="SPECIFIC_CONDUCTANCE_00095_UMHOS/CM @ 25C",    
         "RMK_FDT_SPECIFIC_CONDUCTANCE" ="FDT_SPECIFIC_CONDUCTANCE_RMK" ,
         "FDT_SALINITY" = "SALINITY_00480_PPT" ,            
         "RMK_FDT_SALINITY"  ="FDT_SALINITY_RMK",  
         "NITROGEN_mg_L" = "NITROGEN_mg/L" ,          
         "AMMONIA_mg_L" ="AMMONIA_mg/L",
         "NITRATE_mg_L" = "NITRATE_mg/L",
         "NOX_mg_L" = "NOX_mg/L",
         "NITROGEN_TOTAL_00600_mg_L" = "NITROGEN_TOTAL_00600_mg/L",
         "NITROGEN_AMMONIA_DISSOLVED_00608_mg_L" = "NITROGEN_AMMONIA_DISSOLVED_00608_mg/L",
         "NITROGEN_AMMONIA_TOTAL_00610_mg_L" = "NITROGEN_AMMONIA_TOTAL_00610_mg/L",
         "NITROGEN_NITRITE_DISSOLVED_00613_mg_L" = "NITROGEN_NITRITE_DISSOLVED_00613_mg/L",
         "NITROGEN_NITRITE_TOTAL_00615_mg_L" = "NITROGEN_NITRITE_TOTAL_00615_mg/L",
         "NITROGEN_KJELDAHL_TOTAL_00625_mg_L" = "NITROGEN_KJELDAHL_TOTAL_00625_mg/L",
         "NITRITE+NITRATE_TOTAL_00630_mg_L" = "NITRITE+NITRATE_TOTAL_00630_mg/L",
         "NITRITE+NITRATE_DISSOLVED_00631_mg_L" = "NITRITE+NITRATE_DISSOLVED_00631_mg/L",
         "NITROGEN_PARTICULATE_49570_mg_L" = "NITROGEN_PARTICULATE_49570_mg/L",
         "NITROGEN_TOTAL_DISSOLVED_49571_mg_L" = "NITROGEN_TOTAL_DISSOLVED_49571_mg/L",
         "NITROGEN_TOTAL_DISSOLVED_TDNLF_mg_L" = "NITROGEN_TOTAL_DISSOLVED_TDNLF_mg/L",
         "PHOSPHORUS_mg_L" =  "PHOSPHORUS_mg/L",
         "PHOSPHORUS_TOTAL_00665_mg_L" = "PHOSPHORUS_TOTAL_00665_mg/L",
         "PHOSPHORUS_DISSOLVED_00666_mg_L" = "PHOSPHORUS_DISSOLVED_00666_mg/L",
         "PHOSPHORUS_DISSOLVED_ORTHOPHOSPHATE_00671_mg_L" = "PHOSPHORUS_DISSOLVED_ORTHOPHOSPHATE_00671_mg/L",
         "PHOSPHOROUS_PARTICULATE_49567_mg_L" = "PHOSPHOROUS_PARTICULATE_49567_mg/L",
         "PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg_L" = "PHOSPHOROUS_TOTAL_DISSOLVED_49572_mg/L",
         "PHOSPHORUS_TOTAL_ORTHOPHOSPHATE_70507_mg_L" = "PHOSPHORUS_TOTAL_ORTHOPHOSPHATE_70507_mg/L",
         "ORTHOPHOSPHATE_DISSOLVED_OPWLF_mg_L" = "ORTHOPHOSPHATE_DISSOLVED_OPWLF_mg/L",
         "PHOSPHORUS_SUSPENDED_INORGANIC_PIPLF_mg_L" = "PHOSPHORUS_SUSPENDED_INORGANIC_PIPLF_mg/L",
         "PHOSPHORUS_PARTICULATE_PPWLF_mg_L" = "PHOSPHORUS_PARTICULATE_PPWLF_mg/L",
         "PHOSPHORUS_TOTAL_DISSOLVED_TDPLF_mg_L" = "PHOSPHORUS_TOTAL_DISSOLVED_TDPLF_mg/L",
         "HARDNESS_TOTAL_00900_mg_L" = "HARDNESS_TOTAL_00900_mg/L",
         "CHLORIDE_mg_L" = "CHLORIDE_mg/L",
         "CHLORIDE_TOTAL_00940_mg_L" = "CHLORIDE_TOTAL_00940_mg/L",
         "SULFATE_mg_L" = "SULFATE_mg/L",
         "SULFATE_TOTAL_00945_mg_L" = "SULFATE_TOTAL_00945_mg/L",
         "RMK_SULFATE_TOTAL" = "RMK_00945",
         "SULFATE_DISS_mg_L" = "SULFATE_DISSOLVED_00946_mg/L",
         "RMK_SULFATE_DISS" = "RMK_00946",
         "ECOLI" = "ECOLI_CFU/100mL",  
         "RMK_ECOLI" = "ECOLI_RMK",
         "ECOLI_31648_NO_100mL" = "E._COLI_31648_NO/100mL",
         "ENTEROCOCCI" = "ENTEROCOCCI_31649_NO/100mL",
         "RMK_ENTEROCOCCI" =  "RMK_31649",
         "FECAL_COLI" = "FECAL_COLIFORM_31616_NO/100mL" ,
         "RMK_FECAL_COLI" = "RMK_31616",
         "CHLOROPHYLL_A_ug_L" ="CHLOROPHYLL_32211_ug/L", 
         "RMK_CHLOROPHYLL_A" = "RMK_32211",
         "TSS_mg_L" = "TSS_mg/L",
         "TOTAL_SUSPENDED_SOLIDS_00530_mg_L" = "TOTAL_SUSPENDED_SOLIDS_00530_mg/L",
         "SSC_mg_L" ="SSC-TOTAL_00530_mg/L",
         "RMK_SSC" = "RMK_SSC-TOTAL",
         "TOTAL_SUSPENDED_SOLIDS_TSS45_mg_L" = "TOTAL_SUSPENDED_SOLIDS_TSS45_mg/L") %>%
  mutate(FDT_DATE_TIME = as.POSIXct(FDT_DATE_TIME, format="%m/%d/%Y %H:%M"), # fix date time
         GROUP_STA_ID = NA,  #Source_Sta_Id = NA,
         OTHER_CITMON_NONAGENCY_INFO = NA, 
         Data_Source = "DEQ") %>%
  dplyr::select(FDT_STA_ID, GROUP_STA_ID, everything())

z <- bind_rows(schemaFin, conventionals)

names(conventionals) == names(schemaFin)


names(conventionals)[14:20]
names(schemaFin)[14:20]

rm(schema)
