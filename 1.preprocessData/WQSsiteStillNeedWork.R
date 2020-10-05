# figure out stations that still need work WQS
assessmentLayer <- st_read('data/GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) 

WQSlookup <- loadData("WQSlookupTable")

snap_input <- readRDS('data/WQStable.RDS') 

needWQS <- filter(snap_input, !(StationID %in% WQSlookup$StationID))

# number of stations that need work
nrow(needWQS)

# breakdown of which regions still need work
needWQSregions <- filter(conventionals_DWQS, StationID %in% needWQS$StationID) %>%
  st_intersection(assessmentLayer) %>%
  left_join(needWQS) %>%
  group_by(ASSESS_REG) %>%
  mutate(RegionLeftToDo = n(),
         Type = str_extract(WQS_ID, ".{2}")) %>%
  left_join(WQSlayerConversion, by = c('Type' = 'WQS_ID')) %>%
  ungroup() %>% st_drop_geometry()
  

dataBreakdown <- needWQSregions %>%
  group_by(ASSESS_REG, waterbodyType) %>%
  summarise(`Regional Total That Needs WQS` = n())
