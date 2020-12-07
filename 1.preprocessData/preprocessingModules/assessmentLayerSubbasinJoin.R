
assessmentLayer <- st_read('GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) # transform to WQS84 for spatial intersection

subbasinLayer <- st_read('GIS/DEQ_VAHUSB_subbasins_EVJ.shp')  %>%
  rename('SUBBASIN' = 'SUBBASIN_1')




distinctSites_sf <- st_as_sf(distinctSites,
                            coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
                            remove = F, # don't remove these lat/lon cols from df
                            crs = 4326) %>% # add coordinate reference system, needs to be geographic for now bc entering lat/lng, 
  st_intersection(assessmentLayer ) %>%
  st_join(dplyr::select(subbasinLayer, BASIN_NAME, BASIN_CODE, SUBBASIN), join = st_intersects) %>%
  dplyr::select(-c(geometry, Shape_Leng, Shape_Area), geometry)

if(nrow(distinctSites_sf) < nrow(distinctSites)){
  missingSites <- filter(distinctSites, ! FDT_STA_ID %in% distinctSites_sf$FDT_STA_ID) %>%
    st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
             remove = F, # don't remove these lat/lon cols from df
             crs = 4326) 
  
  closest <- mutate(assessmentLayer[0,], FDT_STA_ID =NA) %>%
    dplyr::select(FDT_STA_ID, everything())
  for(i in seq_len(nrow(missingSites))){
    closest[i,] <- assessmentLayer[which.min(st_distance(assessmentLayer, missingSites[i,])),] %>%
      mutate(FDT_STA_ID = missingSites[i,]$FDT_STA_ID) %>%
      dplyr::select(FDT_STA_ID, everything())
  }
  
  missingSites <- left_join(missingSites, closest %>% st_drop_geometry(),
                            by = 'FDT_STA_ID') %>%
    st_join(dplyr::select(subbasinLayer, BASIN_NAME, BASIN_CODE, SUBBASIN), join = st_intersects) %>%
    dplyr::select(-c(geometry), geometry) %>%
    dplyr::select(names(distinctSites_sf))
  
  
  distinctSites_sf <- rbind(distinctSites_sf, missingSites)
  
  
  # original method but so many sites are so close, seems silly to not give it a go
#  missingSites <- filter(distinctSites, ! FDT_STA_ID %in% distinctSites_sf$FDT_STA_ID) %>%
#    st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
#             remove = F, # don't remove these lat/lon cols from df
#             crs = 4326) %>%
#    mutate(HUC12 = NA, VAHU6 = NA,Portion = NA, MAP = NA, ASSESS_REG = NA, OFFICE_NM = NA, 
#           States = NA, HUType = NA, HUMod = NA, ToHUC = NA, META_ID = NA, Location = NA, 
#           VaName = NA, PC_Water = NA, Tidal = NA, VAHUSB = NA, FedName = NA, HUC10 = NA, 
#           VAHU5 = NA, Basin = NA) %>%
#    st_join(dplyr::select(subbasinLayer, BASIN_NAME, BASIN_CODE, SUBBASIN), join = st_intersects) %>%
#    dplyr::select(-c(geometry), geometry)
  
}

rm(closest); rm(missingSites); rm(i)
saveRDS(distinctSites_sf, './data/distinctSites_sf.RDS')
