
assessmentLayer <- st_read('GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) # transform to WQS84 for spatial intersection

subbasinLayer <- st_read('GIS/deq_basins07.shp') %>%
  mutate(BASIN_CODE = case_when(BASIN_CODE == '3-' ~ '3',
                                BASIN_CODE == '8-' ~ '8',
                                BASIN_CODE == '9-' ~ '9',
                                TRUE ~ as.character(BASIN_CODE)),
         SUBBASIN = case_when(BASIN_CODE == '3' ~ 'Rappahannock River',
                              BASIN_CODE == '8-' ~ 'York River',
                              BASIN_CODE == '9' ~ 'New River', 
                              TRUE ~ as.character(SUBBASIN))) %>%
  st_transform( st_crs(4326)) # transform to WQS84 for spatial intersection




distinctSites_sf <- st_as_sf(distinctSites,
                            coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
                            remove = F, # don't remove these lat/lon cols from df
                            crs = 4326) %>% # add coordinate reference system, needs to be geographic for now bc entering lat/lng, 
  st_intersection(assessmentLayer ) %>%
  st_join(dplyr::select(subbasinLayer, BASIN_NAME, BASIN_CODE, SUBBASIN), join = st_intersects) %>%
  dplyr::select(-c(geometry, Shape_Leng, Shape_Area), geometry)

