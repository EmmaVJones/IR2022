library(fuzzyjoin)
library(pins)
library(config)

# get configuration settings
conn <- config::get("connectionSettings")

# use API key to register board
board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))

# Bring in known DEQ station info
stations <- pin_get('ejones/WQM-Sta-GIS-View', board = 'rsconnect')


# Pull off some extra stuff in PCB$LocID field to make fuzzy join better
PCB <- mutate(PCB, StationID = gsub('-S$', '', LocID)) %>%
  dplyr::select(StationID, everything())

# find stations that need more work
PCBfix <- filter(PCB, ! StationID %in% stations$Station_Id) %>%
  dplyr::select(LocID, StationID) %>%
  stringdist_left_join(dplyr::select(stations, 'Station_Id'), by = c('StationID' = 'Station_Id'), max_dist = 1) %>%
  distinct(StationID, .keep_all = T)

PCBfin <- filter(PCB, StationID %in% stations$Station_Id) %>%
  mutate(StationID_join = 'Direct Match to WQM_Sta_GIS_View') %>%
  bind_rows(
    filter(PCB, ! StationID %in% stations$Station_Id) %>%
      left_join(PCBfix, by= c('StationID', 'LocID')) %>%
      mutate(StationID = Station_Id,
             StationID_join = 'Fuzzy join (1 character) to WQM_Sta_GIS_View') %>%
      dplyr::select(StationID, everything(), -c(Station_Id))
  ) %>%
  arrange(StationID)


  
  
rm(conn); rm(PCB); rm(PCBfix); rm(stations)  

PCB <- PCBfin

rm(PCBfin)
