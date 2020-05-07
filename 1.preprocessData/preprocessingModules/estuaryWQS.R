### Estuary methods


# Bring in estuary layer
estuarinePolys <- st_read('GIS/WQS_layers_05072020.gdb', layer = 'estuarinepolygons_05072020' , fid_column_name = "OBJECTID")%>%
  mutate(UID = paste0('EP_', as.character(BASIN), "_", sprintf("%06d",as.numeric(as.character(OBJECTID))))) %>% # using OBEJCTID as row number for now
  st_transform(4326)

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
                      dplyr::select(estuaryPolyWQS, FDT_STA_ID, UID) %>% 
                        st_drop_geometry() %>%
                        rename('StationID' = 'FDT_STA_ID')) %>%
  drop_na()
  
# Now remove the sites that fell into estuary polygons from list of sites to test against estuary lines
distinctSites_sf_e <- filter(distinctSites_sf_e, ! FDT_STA_ID %in% estuaryPolyWQS$FDT_STA_ID)

# clean up workspace
rm(estuarinePolys); rm(estuaryPolyWQS)


## now work with estuarine lines

snapAndOrganizeWQS 

snapAndOrganizeAU_ListOutput(irData_join, riverineAUs, 
                            bufferDistances = seq(10,50,by=10),
                            outDir = 'data/preAnalyzedRegionalAUdata/BRRO/Riverine/')


