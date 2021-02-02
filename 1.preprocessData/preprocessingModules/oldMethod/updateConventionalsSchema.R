# Update conventionals schema to match new citmon/non agency data schema and 
#  improve data quality. This will be the format the IR2022 (and beyond) scripts
#  use and the format the R generated 'conventionals' data pull produces.

#library(tidyverse)
#library(readxl)

# Read in new schema from Roland (email 12/2/2020)
schema <- read_excel('data/citMonConventionalsSchema.xlsx') %>%
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


# map new names
conventionals <- conventionalsRaw %>%
  #read_csv('./data/final2020data/CEDSWQM_2020_IR_DATA-CONVENTIONALS_20190305.csv') %>%
  # remove duplicate ecoli
  dplyr::select(-c(`E.COLI_ECOLI_CFU/100mL`,	RMK_ECOLI)) %>%
  # change to naming system 
#  rename("FDT_TEMP_CELCIUS"  ="TEMPERATURE_00010_DEGREES CENTIGRADE",
#         #"FDT_TEMP_CELCIUS_RMK" = "FDT_TEMP_CELCIUS_RMK",  
#         "FDT_FIELD_PH" = "pH_00400_STD_UNITS" ,          
#         #"FDT_FIELD_PH_RMK"  ="FDT_FIELD_PH_RMK", 
#         "DO" =  "DO_mg/L",       
#         #"DO_RMK"  ="DO_RMK",    
#         "FDT_SPECIFIC_CONDUCTANCE"="SPECIFIC_CONDUCTANCE_00095_UMHOS/CM @ 25C",    
#         #"FDT_SPECIFIC_CONDUCTANCE_RMK" ="FDT_SPECIFIC_CONDUCTANCE_RMK" ,
#         "FDT_SALINITY" = "SALINITY_00480_PPT" ,            
#         #"FDT_SALINITY_RMK"  ="FDT_SALINITY_RMK",  
#         "NITROGEN" = "NITROGEN_mg/L" ,                    
#         "AMMONIA" ="AMMONIA_mg/L",
#         "PHOSPHORUS" =  "PHOSPHORUS_mg/L",
#         "HARDNESS" = "HARDNESS_TOTAL_00900_mg/L",
#         "RMK_HARDNESS" = "RMK_00900",
#         "FECAL_COLI" = "FECAL_COLIFORM_31616_NO/100mL" ,
#         "RMK_FECAL_COLI" = "RMK_31616",
#         "E.COLI" = "ECOLI_CFU/100mL",                       
#         "ENTEROCOCCI" =  "ENTEROCOCCI_31649_NO/100mL",
#         "RMK_ENTEROCOCCI" =  "RMK_31649",
#         "CHLOROPHYLL" ="CHLOROPHYLL_32211_ug/L", 
#         "RMK_CHLOROPHYLL" = "RMK_32211",
#         "SSC" ="SSC-TOTAL_00530_mg/L" , 
#         "NITRATE" ="NITRATE_mg/L", 
#         "CHLORIDE" ="CHLORIDE_mg/L",
#         "SULFATE_TOTAL" = "SULFATE_TOTAL_00945_mg/L",  
#         "RMK_SULFATE_TOTAL" = "RMK_00945",
#         "SULFATE_DISS" ="SULFATE_DISSOLVED_00946_mg/L",
#         "RMK_SULFATE_DISS" = "RMK_00946") %>%
  mutate(FDT_DATE_TIME = as.POSIXct(FDT_DATE_TIME, format="%m/%d/%Y %H:%M"), # fix date time
         Source_Sta_Id = NA,
         OTHER_CITMON_NONAGENCY_INFO = NA, 
         Data_Source = "DEQ") %>%
  dplyr::select(FDT_STA_ID, Source_Sta_Id, everything())

names(conventionals) <- names(schema)

names(conventionals) == names(schema)

rm(schema)
