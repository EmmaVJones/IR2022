
regionalAUs <- st_zm(st_as_sf(pin_get('AUreservoir_EVJ', board = 'rsconnect')))


lakeNutStandards <- read_csv('data/9VAC25-260-187lakeNutrientStandards.csv')


# summarize AUs into entire lakes

lakes <- regionalAUs %>%
  group_by(WATER_NAME) %>%
  summarise()

# for lakes with multiple lake names, create one name
lakes2 <- lakes %>%
  mutate(Lake_Name = case_when(WATER_NAME %in% c('Smith Lake (Aquia Reservoir)') ~ 'Aquia Reservoir (Smith Lake)',
                               WATER_NAME %in% c('Claytor Lake (New River)', 'Claytor Lake (Peak Creek)') ~ 'Claytor Lake',
                               WATER_NAME %in% c('Fairystone Lake (Goblin Town Creek)') ~ 'Fairystone Lake', 
                               WATER_NAME %in% c('Harwoods Mill Reservoir (PWS)') ~ 'Harwoods Mill Reservoir',
                               WATER_NAME %in% c('Lake Anna', 'Lake Anna/Contrary Creek', 'Lake Anna/Freshwater Creek', 
                                                 'Lake Anna/Gold Mine Creek', 'Lake Anna/Pamunkey Creek', 
                                                 'Lake Anna/Plentiful Creek', 'Terrys Run/Lake Anna') ~ 'Lake Anna',
                               WATER_NAME %in% c('Lake Cohoon (PWS)') ~ 'Lake Cohoon',          
                               WATER_NAME %in% c('Lake Kilby (PWS)') ~ 'Lake Kilby',
                               WATER_NAME %in% c('Lake Meade (PWS)') ~ 'Lake Meade',
                               WATER_NAME %in% c('Lake Moomaw (Jackson River)') ~ 'Lake Moomaw',
                               WATER_NAME %in% c('Lake Prince - Reservoir (PWS)') ~ 'Lake Prince - Reservoir',
                               WATER_NAME %in% c('Lake Smith (PWS)') ~ 'Lake Smith',
                               WATER_NAME %in% c('Lake Whitehurst (PWS)') ~ 'Lake Whitehurst',
                               WATER_NAME %in% c('Leesville Lake', 'Leesville Lake (Pigg R.)', 
                                                 'Leesville Lake Middle (Roanoke R.)') ~ 'Leesville Lake',
                               WATER_NAME %in% c('Lee Hall Reservoir- Upper, Middle','Lee Hall Reservoir-Lower') ~ 'Lee Hall Reservoir',
                               WATER_NAME %in% c('Little Creek Reservoir - (PWS)') ~ 'Little Creek Reservoir (VBC)',
                               WATER_NAME %in% c('Little Creek Reservoir (PWS)') ~ 'Little Creek Reservoir (JCC)',
                               WATER_NAME %in% c('Lone Star Lake F (PWS)') ~ 'Lone Star Lake F (Crystal Lake)', 
                               WATER_NAME %in% c('Lone Star Lake G (PWS)') ~ 'Lone Star Lake G (Crane Lake)', 
                               WATER_NAME %in% c('Lone Star Lake I (PWS)') ~ 'Lone Star Lake I (Butler Lake)', 
                               WATER_NAME %in% c('Martinsville (Beaver Creek) Reservoir') ~ 'Martinsville Reservoir (Beaver Creek Reservoir)',
                               WATER_NAME %in% c('Philpott Reservoir (Goblin Town Creek)', 
                                                 'Philpott Reservoir (Smith River)') ~ "Philpott Reservoir",
                               WATER_NAME %in% c('Roanoke River') ~ 'Lake Gaston',                         
                               WATER_NAME %in% c('S F Rivanna River Reservoir') ~ 'Rivanna Reservoir (South Fork Rivanna Reservoir)',
                               str_detect(WATER_NAME, 'Smith Mtn. Lake') ~ 'Smith Mountain Lake', 
                               WATER_NAME %in% c('Speights Run - Lake (PWS)') ~ 'Speights Run Lake',
                               WATER_NAME %in% c('Waller Mill Reservoir (PWS)') ~ 'Waller Mill Reservoir',
                               WATER_NAME %in% c('Unnamed pond near Tanyard Swamp') ~ 'Tanyard Swamp',
                               WATER_NAME %in% c('Unsegmented lakes in G03') ~ 'West Run',
                               TRUE ~ as.character(WATER_NAME))) 

# Try to join to lake nutrient standards
lakes3 <- left_join(lakes2, lakeNutStandards, by = c('Lake_Name')) %>%
  arrange(Lake_Name)
View(lakes3 %>% st_drop_geometry())         
         
         
# Now try to join to stations by ID305B instead of WQS names to avoid another name mapping nightmare
lakes4 <- regionalAUs %>%
  mutate(Lake_Name = case_when(WATER_NAME %in% c('Smith Lake (Aquia Reservoir)') ~ 'Aquia Reservoir (Smith Lake)',
                               WATER_NAME %in% c('Claytor Lake (New River)', 'Claytor Lake (Peak Creek)') ~ 'Claytor Lake',
                               WATER_NAME %in% c('Fairystone Lake (Goblin Town Creek)') ~ 'Fairystone Lake', 
                               WATER_NAME %in% c('Harwoods Mill Reservoir (PWS)') ~ 'Harwoods Mill Reservoir',
                               WATER_NAME %in% c('Lake Anna', 'Lake Anna/Contrary Creek', 'Lake Anna/Freshwater Creek', 
                                                 'Lake Anna/Gold Mine Creek', 'Lake Anna/Pamunkey Creek', 
                                                 'Lake Anna/Plentiful Creek', 'Terrys Run/Lake Anna') ~ 'Lake Anna',
                               WATER_NAME %in% c('Lake Cohoon (PWS)') ~ 'Lake Cohoon',          
                               WATER_NAME %in% c('Lake Kilby (PWS)') ~ 'Lake Kilby',
                               WATER_NAME %in% c('Lake Meade (PWS)') ~ 'Lake Meade',
                               WATER_NAME %in% c('Lake Moomaw (Jackson River)') ~ 'Lake Moomaw',
                               WATER_NAME %in% c('Lake Prince - Reservoir (PWS)') ~ 'Lake Prince - Reservoir',
                               WATER_NAME %in% c('Lake Smith (PWS)') ~ 'Lake Smith',
                               WATER_NAME %in% c('Lake Whitehurst (PWS)') ~ 'Lake Whitehurst',
                               WATER_NAME %in% c('Leesville Lake', 'Leesville Lake (Pigg R.)', 
                                                 'Leesville Lake Middle (Roanoke R.)') ~ 'Leesville Lake',
                               WATER_NAME %in% c('Lee Hall Reservoir- Upper, Middle','Lee Hall Reservoir-Lower') ~ 'Lee Hall Reservoir',
                               WATER_NAME %in% c('Little Creek Reservoir - (PWS)') ~ 'Little Creek Reservoir (VBC)',
                               WATER_NAME %in% c('Little Creek Reservoir (PWS)') ~ 'Little Creek Reservoir (JCC)',
                               WATER_NAME %in% c('Lone Star Lake F (PWS)') ~ 'Lone Star Lake F (Crystal Lake)', 
                               WATER_NAME %in% c('Lone Star Lake G (PWS)') ~ 'Lone Star Lake G (Crane Lake)', 
                               WATER_NAME %in% c('Lone Star Lake I (PWS)') ~ 'Lone Star Lake I (Butler Lake)', 
                               WATER_NAME %in% c('Martinsville (Beaver Creek) Reservoir') ~ 'Martinsville Reservoir (Beaver Creek Reservoir)',
                               WATER_NAME %in% c('Philpott Reservoir (Goblin Town Creek)', 
                                                 'Philpott Reservoir (Smith River)') ~ "Philpott Reservoir",
                               WATER_NAME %in% c('Roanoke River') ~ 'Lake Gaston',                         
                               WATER_NAME %in% c('S F Rivanna River Reservoir') ~ 'Rivanna Reservoir (South Fork Rivanna Reservoir)',
                               str_detect(WATER_NAME, 'Smith Mtn. Lake') ~ 'Smith Mountain Lake', 
                               WATER_NAME %in% c('Speights Run - Lake (PWS)') ~ 'Speights Run Lake',
                               WATER_NAME %in% c('Waller Mill Reservoir (PWS)') ~ 'Waller Mill Reservoir',
                               WATER_NAME %in% c('Unnamed pond near Tanyard Swamp') ~ 'Tanyard Swamp',
                               WATER_NAME %in% c('Unsegmented lakes in G03') ~ 'West Run',
                               TRUE ~ as.character(WATER_NAME))) %>%
  left_join(lakeNutStandards, by = c('Lake_Name'))

stationTable1 <- left_join(stationTable, 
                           dplyr::select(lakes4, ID305B, Lake_Name:`Total Phosphorus (ug/L)`) %>% st_drop_geometry(), 
                           by = c('ID305B_1' = 'ID305B')) %>%
  group_by(STATION_ID) 

View(filter(regionalAUs, ID305B %in% c('VAW-I03L_JKS03A02', 'VAN-F07L_NAR01A02')))



# make it into a function for easier application

lakeNameStandardization <- function(x){
  x %>%
    mutate(Lake_Name = case_when(WATER_NAME %in% c('Smith Lake (Aquia Reservoir)') ~ 'Aquia Reservoir (Smith Lake)',
                                 WATER_NAME %in% c('Claytor Lake (New River)', 'Claytor Lake (Peak Creek)') ~ 'Claytor Lake',
                                 WATER_NAME %in% c('Fairystone Lake (Goblin Town Creek)') ~ 'Fairystone Lake', 
                                 WATER_NAME %in% c('Harwoods Mill Reservoir (PWS)') ~ 'Harwoods Mill Reservoir',
                                 WATER_NAME %in% c('Lake Anna', 'Lake Anna/Contrary Creek', 'Lake Anna/Freshwater Creek', 
                                                   'Lake Anna/Gold Mine Creek', 'Lake Anna/Pamunkey Creek', 
                                                   'Lake Anna/Plentiful Creek', 'Terrys Run/Lake Anna') ~ 'Lake Anna',
                                 WATER_NAME %in% c('Lake Cohoon (PWS)') ~ 'Lake Cohoon',          
                                 WATER_NAME %in% c('Lake Kilby (PWS)') ~ 'Lake Kilby',
                                 WATER_NAME %in% c('Lake Meade (PWS)') ~ 'Lake Meade',
                                 WATER_NAME %in% c('Lake Moomaw (Jackson River)') ~ 'Lake Moomaw',
                                 WATER_NAME %in% c('Lake Prince - Reservoir (PWS)') ~ 'Lake Prince - Reservoir',
                                 WATER_NAME %in% c('Lake Smith (PWS)') ~ 'Lake Smith',
                                 WATER_NAME %in% c('Lake Whitehurst (PWS)') ~ 'Lake Whitehurst',
                                 WATER_NAME %in% c('Leesville Lake', 'Leesville Lake (Pigg R.)', 
                                                   'Leesville Lake Middle (Roanoke R.)') ~ 'Leesville Lake',
                                 WATER_NAME %in% c('Lee Hall Reservoir- Upper, Middle','Lee Hall Reservoir-Lower') ~ 'Lee Hall Reservoir',
                                 WATER_NAME %in% c('Little Creek Reservoir - (PWS)') ~ 'Little Creek Reservoir (VBC)',
                                 WATER_NAME %in% c('Little Creek Reservoir (PWS)') ~ 'Little Creek Reservoir (JCC)',
                                 WATER_NAME %in% c('Lone Star Lake F (PWS)') ~ 'Lone Star Lake F (Crystal Lake)', 
                                 WATER_NAME %in% c('Lone Star Lake G (PWS)') ~ 'Lone Star Lake G (Crane Lake)', 
                                 WATER_NAME %in% c('Lone Star Lake I (PWS)') ~ 'Lone Star Lake I (Butler Lake)', 
                                 WATER_NAME %in% c('Martinsville (Beaver Creek) Reservoir') ~ 'Martinsville Reservoir (Beaver Creek Reservoir)',
                                 WATER_NAME %in% c('Philpott Reservoir (Goblin Town Creek)', 
                                                   'Philpott Reservoir (Smith River)') ~ "Philpott Reservoir",
                                 WATER_NAME %in% c('Roanoke River') ~ 'Lake Gaston',                         
                                 WATER_NAME %in% c('S F Rivanna River Reservoir') ~ 'Rivanna Reservoir (South Fork Rivanna Reservoir)',
                                 str_detect(WATER_NAME, 'Smith Mtn. Lake') ~ 'Smith Mountain Lake', 
                                 WATER_NAME %in% c('Speights Run - Lake (PWS)') ~ 'Speights Run Lake',
                                 WATER_NAME %in% c('Waller Mill Reservoir (PWS)') ~ 'Waller Mill Reservoir',
                                 WATER_NAME %in% c('Unnamed pond near Tanyard Swamp') ~ 'Tanyard Swamp',
                                 WATER_NAME %in% c('Unsegmented lakes in G03') ~ 'West Run',
                                 TRUE ~ as.character(WATER_NAME)))
}



test <- regionalAUs %>%
  lakeNameStandardization()





# view individual lakes
x <- filter(lakes, WATER_NAME %in% c('Unsegmented lakes in G03'))

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = x)

              