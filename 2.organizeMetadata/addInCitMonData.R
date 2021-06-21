citmon <- read_excel('data/final2022data/Citizen Monitoring Data/20192020CitMonData_20210611.xlsx') %>% 
  rename('DO_mg_L' = 'DO', #forgot to change that in 2022 IR scripts, need to use DO not DO_mg_L in 2024
         'CHLOROPHYLL_A_ug_L' = 'CHLOROPHYLL_ug_L') %>% # this needs to be fixed on Roland's side
  dplyr::select(FDT_STA_ID:LEVEL_FDT_TEMP_CELCIUS, DO_mg_L:LEVEL_TSS45, SECCHI_DEPTH_M, RMK_SECCHI_DEPTH, LEVEL_SECCHI_DEPTH,
                Latitude:Data_Source) %>% 
  naniar::replace_with_na_if(.predicate = is.character,
                             condition = ~.x %in% ("NULL")) %>% 
  # recode Level fields to match automated assessment functions
  mutate(across(starts_with("LEVEL_"), ~recode(., '3' = "Level III") )) %>% 
  mutate(across(starts_with("LEVEL_"), ~recode(., '2' = "Level II") )) %>% 
  mutate(across(starts_with("LEVEL_"), ~recode(., '1' = "Level I") )) %>% 
  
  # fix station names if none exist in template
  mutate(FDT_STA_ID = ifelse(is.na(FDT_STA_ID), GROUP_STA_ID, FDT_STA_ID)) %>% 
  
  # fix STA_REC_CODE
  mutate(STA_REC_CODE = case_when(Deq_Region == 'Piedmont' ~ 'PRO',
                                  Deq_Region == 'Northern' ~ 'NRO',
                                  Deq_Region == 'Valley' ~ 'VRO',
                                  Deq_Region == 'Tidewater' ~ 'TRO',
                                  Deq_Region == 'Blue Ridge' ~ 'BRRO', 
                                  TRUE ~ as.character(Deq_Region)))


# make sure data formats match for combining
citmon1 <- citmon %>% 
  mutate_at(conventionals  %>% dplyr::select(where(is.numeric)) %>% names(), as.numeric) %>% 
  # a few special cases
  mutate(Huc6_Huc_8 = as.character(Huc6_Huc_8),
         Huc6_Huc_12 = as.character(Huc6_Huc_12))

conventionals1 <- conventionals %>% 
  mutate_at(conventionals  %>% dplyr::select(where(is.logical)) %>% names(), as.character)


names(conventionals1) == names(citmon1)
  

conventionals2 <- bind_rows(conventionals1, citmon1)

conventionals <- conventionals2

rm(conventionals1);rm(conventionals2); rm(citmon1)#; rm(citmon)

