# figure out stations that still need work WQS
assessmentLayer <- st_read('data/GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) 

WQSlookup <- loadData("WQSlookupTable")

snap_input <- readRDS('data/WQStable02032021.RDS')# readRDS('data/WQStable.RDS')  # original

# bring in stuff already on server
library(pins)
WQSlookup_server <- pin_get("WQSlookup-withStandards",  board = "rsconnect")%>%
  dplyr::select(StationID:Comments)

WQSlookup <- bind_rows(WQSlookup, WQSlookup_server)


needWQS <- filter(snap_input, !(StationID %in% WQSlookup$StationID)) 

# number of stations that need work
nrow(distinct(needWQS, StationID))

# breakdown of which regions still need work
needWQSregions <- filter(conventionals_DWQS, StationID %in% needWQS$StationID) %>%
  st_intersection(assessmentLayer) %>%
  left_join(needWQS) %>%
  distinct(StationID, .keep_all = T) %>% 
  group_by(ASSESS_REG) %>%
  mutate(RegionLeftToDo = n(),
         Type = str_extract(WQS_ID, ".{2}")) %>%
  left_join(WQSlayerConversion, by = c('Type' = 'WQS_ID')) %>%
  ungroup() %>% st_drop_geometry()
  
View(filter(needWQSregions, ASSESS_REG == 'CO'))

dataBreakdown <- needWQSregions %>%
  group_by(ASSESS_REG, waterbodyType) %>%
  summarise(`Regional Total That Needs WQS` = n())
# this is a loose breakdown. Examples of stations being assigned to different regions in this report are
#  prevalent bc this quick analysis breaks things down by hard AU borders. The application
#  assigns stations to regions based on subbasin codes in the WQS_ID to be more inclusive of wonky stationID's
#  (e.g. 7B and 7- are CO)



COmissing <- filter(needWQSregions, ASSESS_REG == 'CO') %>% 
  dplyr::select(WQS_ID:waterbodyType, everything())

COmissing$FDT_STA_ID %in% conventionalsRaw$FDT_STA_ID
COmissing$FDT_STA_ID %in% WQSlookup_server$StationID
COmissing$FDT_STA_ID %in% conventionals_DWQS_Region$FDT_STA_ID
# moral of the story, 7- is not included in 