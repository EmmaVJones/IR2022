
library(pins)
library(config)

# get configuration settings
conn <- config::get("connectionSettings")

# use API key to register board
board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))


assessmentLayer <- st_read('data/GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) %>%
  group_by(ASSESS_REG) %>%
  summarise() %>% ungroup()

# Riverine split up
riverineL <- st_read('C:/HardDriveBackup/GIS/Assessment/2020IR_final/2020IR_GISData/va_aus_riverine.shp') %>%
  st_transform(4326)   # transform to WQS84 for spatial intersection 

# intersect segments with Assessment regions to get appropriate ASSESS_REG argument for data organization
riverineLB <- st_join(st_zm(riverineL), assessmentLayer, join = st_intersects) 

for(i in as.character(unique(assessmentLayer$ASSESS_REG))){
  print(paste('pinning:', i))
  regionalAUs <- filter(riverineLB, ASSESS_REG %in% i) 
  pin(regionalAUs, name = paste0(i, '_AUriverine'), description = paste0("2020IR final", i, " AU riverine"), board = "rsconnect")
}

