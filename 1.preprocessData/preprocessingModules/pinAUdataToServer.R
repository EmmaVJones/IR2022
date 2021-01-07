
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

rm(riverineL); rm(riverineLB)



# Lakes
# we are going to keep lakes all together bc data sent across server is much less than riverine segments

# Using the final 2020IR lake AUs that have been edited by Emma to include Paula's intended AU splits (from the start)
lakesP <- st_read('C:/HardDriveBackup/GIS/Assessment/2020IR_final/2020IR_GISData/va_aus_reservoir_EVJ.shp') %>%
  st_transform(4326)   # transform to WQS84 for spatial intersection 
# intersect segments with Assessment regions to get appropriate ASSESS_REG argument for data organization
lakesPB <- st_join(st_zm(lakesP), assessmentLayer, join = st_intersects) 

# lake Anna and Lake Moomaw fall into two regions, keep only real regions to avoid duplication
lakesPBfin <- filter(lakesPB, !(ID305B == 'VAN-F07L_NAR01A02' & ASSESS_REG == 'PRO')) %>%
  filter(!(ID305B == 'VAW-I03L_JKS03A02' & ASSESS_REG == 'VRO'))

pin(lakesPBfin, name = 'AUreservoir_EVJ', description = "2020IR final AU reservoir with EVJ splits", board = "rsconnect")



# Estuary
# we are going to throw estuary up there as well to help out Steve but no Emma developed apps will use this layer

estuaryP <- st_read('C:/HardDriveBackup/GIS/Assessment/2020IR_final/2020IR_GISData/va_aus_estuarine.shp') %>%
  st_transform(4326)   # transform to WQS84 for spatial intersection 
# intersect segments with Assessment regions to get appropriate ASSESS_REG argument for data organization
estuaryPB <- st_join(st_zm(estuaryP), assessmentLayer, join = st_intersects) 

pin(estuaryPB, name = 'AUestuarine', description = "2020IR final AU estuarine", board = "rsconnect")

