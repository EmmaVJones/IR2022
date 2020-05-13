
st_layers('GIS/WQS_layers_05082020.gdb')


test <- st_read('GIS/WQS_layers_05082020.gdb', layer = 'riverine_05082020' , fid_column_name = "OBJECTID") %>%
  st_transform(4326) 

test2 <- test %>%
  filter(BASIN %in% filter(basinCodesConversion, Basin_Code %in% c('New'))$BASIN)

basinCodes <- filter(basinAssessmentRegion, BASIN %in% c('9', '2A', '2B')) %>% #unique(WQSstatewide()$BASIN)) %>%
    filter(ASSESS_REG %in% 'BRRO') %>% #input$WQSDEQregionSelection) %>%
    filter(Basin_Code %in% 'James-Middle') %>% #input$WQSsubbasinSelection) %>%
    distinct(BASIN_CODE) %>% 
    pull() 

# this is all sites limited to waterbody type and subbasin
snap_input <- readRDS('data/processedWQS/WQStable.RDS') %>%
  filter(str_extract(WQS_ID, "^.{2}") %in% filter(WQSlayerConversion, waterbodyType %in% 'Riverine')$WQS_ID) %>% 
  filter(gsub("_","",str_extract(WQS_ID, ".{3}_")) %in% 
           str_pad(unique(filter(basinAssessmentRegion, BASIN_CODE %in% basinCodes)$BASIN_CODE), 
                   width = 2, side = 'left', pad = '0')) %>%
  group_by(StationID) %>%
  mutate(n = n()) %>% ungroup() 
snap_input_Region <- snap_input %>%
  # limit to just region of interest
  left_join(conventionals_D, by = 'StationID') %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = T, # don't remove these lat/lon cols from df
           crs = 4326) %>%
  st_intersection(filter(assessmentRegions, ASSESS_REG %in% 'BRRO')) %>%#input$WQSDEQregionSelection) %>%
  st_drop_geometry() %>% # back to tibble
  rename('Buffer Distance' = 'Buffer.Distance') %>%
  dplyr::select(StationID, WQS_ID, `Buffer Distance`, n)


# Make dataset of all sites in subbasin for highlighting purposes, preliminary list, left at subbasin in case regional swapping occurs or outside state boundary
sitesUnique <- snap_input %>%
  left_join(conventionals_D, by = 'StationID') %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = T, # don't remove these lat/lon cols from df
           crs = 4326) 
# Make dataset of all WQS_IDs available for table purposes, this will hold corrected WQS_ID information after user review
WQS_IDs <- snap_input
# Make dataset of multiple segments snapped to single site
tooMany <- filter(snap_input_Region, n > 1) %>%
  group_by(StationID) %>% mutate(colorFac = row_number()) %>% ungroup()
# Make dataset of sites associated with too many segments
tooMany_sites <- filter(sitesUnique, StationID %in% tooMany$StationID) %>%
  left_join(test2 %>% st_drop_geometry(), by = 'WQS_ID') %>%#left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID')
  distinct(StationID, .keep_all = T) %>%
  dplyr::select(-c(WQS_ID, `Buffer Distance`, n))
# Make dataset of sites that snapped to a single WQS and join WQS info  
snapSingle <- filter(sitesUnique, n == 1) %>%
  filter(StationID %in% snap_input_Region$StationID) %>% # limit assignment to just what falls in a region
  left_join(test2 %>% st_drop_geometry(), by = 'WQS_ID')#left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID')
# Make dataset of sites associated with no segments
snapNone <- filter(sitesUnique, is.na(WQS_ID)) %>%
  filter(StationID %in% snap_input_Region$StationID) %>% # limit assignment to just what falls in a region
  left_join(test2 %>% st_drop_geometry(), by = 'WQS_ID')#left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID')
