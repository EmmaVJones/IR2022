
st_layers('GIS/WQS_layers_05082020.gdb')


test <- st_read('GIS/WQS_layers_05082020.gdb', layer = 'riverine_05082020' , fid_column_name = "OBJECTID") %>%
  st_transform(4326) 

test2 <- test %>%
  filter(BASIN %in% filter(basinCodesConversion, Basin_Code %in% c('New'))$BASIN)

basinCodes <- filter(basinAssessmentRegion, BASIN %in% c('9', '2A', '2B')) %>% #unique(WQSstatewide()$BASIN)) %>%
    filter(ASSESS_REG %in% 'BRRO') %>% #input$WQSDEQregionSelection) %>%
    filter(Basin_Code %in% 'New') %>% #input$WQSsubbasinSelection) %>%
    distinct(BASIN_CODE) %>% 
    pull() 

# this is all sites limited to waterbody type
snap_input <- readRDS('data/processedWQS/WQStable.RDS') %>%
  filter(str_extract(WQS_ID, "^.{2}") %in% filter(WQSlayerConversion, waterbodyType %in% 'Riverine')$WQS_ID) %>% 
  #filter(gsub("_","",str_extract(WQS_ID, ".{3}_")) %in% '09')#unique(filter(basinAssessmentRegion, BASIN_CODE %in% basinCodes)$BASIN_CODE)) %>%
  group_by(StationID) %>%
  mutate(n = n()) %>% ungroup()


# really need this dataset to get down to subbasin inside assessment region only

# Make dataset of all sites for highlighting purposes, preliminary list
sitesUnique <- snap_input %>%
  left_join(conventionals_D, by = 'StationID') %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = T, # don't remove these lat/lon cols from df
           crs = 4326) %>%
  st_intersection(filter(assessmentLayer, )
# Make dataset of all WQS_IDs available for table purposes, this will hold corrected WQS_ID information after user review
WQSreactive_objects$WQS_IDs <- WQSreactive_objects$snap_input 
# Make dataset of multiple segments snapped to single site
WQSreactive_objects$tooMany <- filter(WQSreactive_objects$snap_input, n > 1) %>%
  group_by(StationID) %>% mutate(colorFac = row_number()) %>% ungroup()
# Make dataset of sites associated with too many segments
WQSreactive_objects$tooMany_sites <- filter(WQSreactive_objects$sitesUnique, StationID %in% WQSreactive_objects$tooMany$StationID) %>%
  left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID')
# Make dataset of sites that snapped to a single WQS and join WQS info  
WQSreactive_objects$snapSingle <- filter(WQSreactive_objects$sitesUnique, n == 1) %>%
  left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID')
# Make dataset of sites associated with no segments
WQSreactive_objects$snapNone <- filter(WQSreactive_objects$sitesUnique, is.na(WQS_ID)) %>%
  left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID')