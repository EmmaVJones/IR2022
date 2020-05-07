### lake methods- polygon

lakePolygonJoin <- function(lakePolys,
                            disinctSites_sf,
                            WQStable){
  
  # first make sure everyone in same crs
  lakePolys <- st_transform(lakePolys, st_crs(distinctSites_sf))
  
  
  # First work with Polygons since computationally faster
  
  # Spatially join to polygon layer and create table to store links
  lakePolyWQS <- st_join(distinctSites_sf, lakePolys, join = st_intersects) %>%
    filter(!is.na(OBJECTID)) 
  
  WQStable <- bind_rows(WQStable,
                        dplyr::select(lakePolyWQS, FDT_STA_ID, UID) %>% 
                          st_drop_geometry() %>%
                          rename('StationID' = 'FDT_STA_ID')) %>%
    drop_na()
  

  return(WQStable)
}

