### Polygon Spatial join methods for AUs

polygonJoinAU <- function(polygonAUlayer,
                          distinctSites_AUtoDo_f, 
                          estuaryTorF){
  
  # first make sure everyone in same crs
  polys <- st_transform(polygonAUlayer, st_crs(distinctSites_AUtoDo_f))
  
  
  # Work with estuary sites only if estuaryTorF == T
  if(estuaryTorF == TRUE){
    # Identify which subbasins needs esturine work
    subB <-  c("Potomac River", "Rappahannock River", "Atlantic Ocean Coastal", "Chesapeake Bay Tributaries",
               "Chesapeake Bay - Mainstem", "James River - Lower",  "Appomattox River" ,   "Chowan River",
               "Atlantic Ocean - South" , "Dismal Swamp/Albemarle Sound")
    
    # Identify sites in said subB that will have estuary WQS attempted to be joined
    distinctSites_AUtoDo_f <- filter(distinctSites_AUtoDo_f, SUBBASIN %in% subB)
  }
  
  # Spatially join to polygon layer 
  polyAUs <- st_join(distinctSites_AUtoDo_f, polys, join = st_intersects) %>%
    filter(!is.na(ID305B)) 
  
  return(polyAUs)
}

