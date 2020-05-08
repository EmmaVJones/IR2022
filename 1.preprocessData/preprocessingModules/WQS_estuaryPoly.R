### Estuary methods- polygon

estuaryPolygonJoin <- function(estuarinePolys, 
                               disinctSites_sf, 
                               WQStable){
  
  # first make sure everyone in same crs
  estuarinePolys <- st_transform(estuarinePolys, st_crs(distinctSites_sf))
  
  # Identify which subbasins needs esturine work
  subB <-  c("Potomac River", "Rappahannock River", "Atlantic Ocean Coastal", "Chesapeake Bay Tributaries",
             "Chesapeake Bay - Mainstem", "James River - Lower",  "Appomattox River" ,   "Chowan River",
             "Atlantic Ocean - South" , "Dismal Swamp/Albemarle Sound")
  
  # Identify sites in said subB that will have estuary WQS attempted to be joined
  
  distinctSites_sf_e <- filter(distinctSites_sf, SUBBASIN %in% subB)
  
  
  # First work with Polygons since computationally faster
  
  # Spatially join to polygon layer and create table to store links
  estuaryPolyWQS <- st_join(distinctSites_sf_e, estuarinePolys, join = st_intersects) %>%
    filter(!is.na(OBJECTID)) 
  
  WQStable <- bind_rows(WQStable,
                        dplyr::select(estuaryPolyWQS, FDT_STA_ID, WQS_ID) %>% 
                          st_drop_geometry() %>%
                          rename('StationID' = 'FDT_STA_ID')) %>%
    drop_na()
  
  ## Now remove the sites that fell into estuary polygons from list of sites 
  #distinctSites_sf_e <- filter(distinctSites_sf_e, ! FDT_STA_ID %in% estuaryPolyWQS$FDT_STA_ID)
  
  return(WQStable)
}

