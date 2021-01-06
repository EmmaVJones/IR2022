basinCodesConversion <- read_csv('data/basinCodeConversion.csv') %>%
  filter(BASIN != 7) %>%
  bind_rows(data.frame(BASIN = '7D', Basin_Code = 'Small Coastal'))

# Bring in subbasin options
subbasins <- st_read('GIS/DEQ_VAHUSB_subbasins_EVJ.shp') %>%
  rename('SUBBASIN' = 'SUBBASIN_1')


assessmentLayer <- st_read('data/GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) %>%
  group_by(VAHUSB, ASSESS_REG) %>%
  summarise() %>% ungroup()


# Make object to save available Subbasins for app 
subbasinOptionsByAUtype <- tibble(waterbodyType = as.character(),
                                   SubbasinOptions = as.character(),
                                   AssessmentRegion = as.character(),
                                   WQS_ID_Prefix = as.character())



# Riverine split up
riverineL <- st_read('C:/HardDriveBackup/GIS/Assessment/2020IR_final/2020IR_GISData/va_aus_riverine.shp') %>%
  st_transform(4326)   # transform to WQS84 for spatial intersection 
  # trying to work with featureclasses
  #st_read('C:/HardDriveBackup/GIS/Assessment/2020IR_final/2020IR_GISData/va_20ir_aus_final.gdb', layer = 'va_aus_riverine')# %>% # could not transform crs by bringing in layer as featureclass, so exported it to shapefile first
  # draft 2020 IR data
  #st_read('C:/HardDriveBackup/GIS/Assessment/2020IR_draft/va_2020_aus_riverine.shp') %>%
  #st_transform(4326)   # transform to WQS84 for spatial intersection 


# intersect subbasins with WQS to get appropriate subbasin argument for data organization
riverineLB <- st_join(st_zm(riverineL), dplyr::select(subbasins, BASIN_CODE, ASSESS_REG), join = st_intersects) %>%
  left_join(basinCodesConversion, by = c('BASIN_CODE' ='BASIN')) 


for(i in 1:length(unique(riverineLB$BASIN_CODE))){
  z <- filter(riverineLB, BASIN_CODE == as.character(unique(riverineLB$BASIN_CODE)[i])) %>%
    select(-ASSESS_REG)
  # identify which assessment regions have data for app, only way to verify that a given waterbody belongs to a region
  z1 <- st_join(z, assessmentLayer, join = st_intersects)
  
  #for(k in 1:length(unique(z1$VAHUSB))){
  #  z2 <- filter(z1, VAHUSB == as.character(unique(z1$VAHUSB)[k]))
  #  if(nrow(z2) > 0){
  #    st_write(z2, paste0('data/GIS/processedAUs_2020draft/RL_', 
  #                        as.character(unique(z2$BASIN_CODE)),'_',
  #                        as.character(unique(z2$VAHUSB)), '.shp'), driver = "ESRI Shapefile")
  #  }
  #}
  st_write(z1, paste0('data/GIS/processedAUs_2020final/AU_RL_', 
                                              as.character(unique(z1$BASIN_CODE)),'.shp'), driver = "ESRI Shapefile")
  # draft location
  #st_write(z1, paste0('data/GIS/processedAUs_2020draft/AU_RL_', 
  #                    as.character(unique(z1$BASIN_CODE)),'.shp'), driver = "ESRI Shapefile")
  
  subbasinAssessmentOptions <- tibble(waterbodyType = as.character('Riverine'),
                                      SubbasinOptions = as.character(unique(z$BASIN_CODE)),
                                      AssessmentRegion = as.character(unique(z$ASSESS_REG)),
                                      WQS_ID_Prefix = as.character('RL'))
  subbasinOptionsByAUtype <- bind_rows(subbasinOptionsByAUtype, subbasinAssessmentOptions)

}


rm(riverineLB);rm(riverineL)


# Lakes split up
# this version uses the lake splits Emma made based on Paula's map and the 2020 IR final lakes AU layer.
# this represents a better starting point for BRRO bc we know these massive AUs are going to be split into
# smaller AUs and doing so at the start of the assessment prevents AUs from pooling data
lakesL <- st_read('C:/HardDriveBackup/GIS/Assessment/2020IR_final/2020IR_GISData/va_aus_reservoir_EVJ.shp') %>%
  st_transform(4326)   # transform to WQS84 for spatial intersection 
  # 2020 draft
  #st_read('C:/HardDriveBackup/GIS/Assessment/2020IR_draft/va_2020_aus_reservoir.shp') %>%
  #st_transform(4326)   # transform to WQS84 for spatial intersection 

# intersect subbasins with WQS to get appropriate subabsin argument for data organization
lakesLB <- st_join(st_zm(lakesL), dplyr::select(subbasins, BASIN_CODE, ASSESS_REG), join = st_intersects) %>%
  left_join(basinCodesConversion, by = c('BASIN_CODE' ='BASIN')) 


for(i in 1:length(unique(lakesLB$BASIN_CODE))){
  z <- filter(lakesLB, BASIN_CODE == as.character(unique(lakesLB$BASIN_CODE)[i])) %>%
    select(-ASSESS_REG)
  # identify which assessment regions have data for app, only way to verify that a given waterbody belongs to a region
  z1 <- st_join(z, assessmentLayer, join = st_intersects)
  
  #for(k in 1:length(unique(z1$VAHUSB))){
  #  z2 <- filter(z1, VAHUSB == as.character(unique(z1$VAHUSB)[k]))
  #  if(nrow(z2) > 0){
  #    st_write(z2, paste0('data/GIS/processedAUs_2020draft/RL_', 
  #                        as.character(unique(z2$BASIN_CODE)),'_',
  #                        as.character(unique(z2$VAHUSB)), '.shp'), driver = "ESRI Shapefile")
  #  }
  #}
  # final Location
  st_write(z1, paste0('data/GIS/processedAUs_2020final/AU_LP_', 
                      as.character(unique(z1$BASIN_CODE)),'.shp'), driver = "ESRI Shapefile")
  # Draft location
  #st_write(z1, paste0('data/GIS/processedAUs_2020draft/AU_LP_', 
  #                    as.character(unique(z1$BASIN_CODE)),'.shp'), driver = "ESRI Shapefile")
  
  subbasinAssessmentOptions <- tibble(waterbodyType = as.character('Lacustrine'),
                                      SubbasinOptions = as.character(unique(z$BASIN_CODE)),
                                      AssessmentRegion = as.character(unique(z$ASSESS_REG)),
                                      WQS_ID_Prefix = as.character('LP'))
  subbasinOptionsByAUtype <- bind_rows(subbasinOptionsByAUtype, subbasinAssessmentOptions)
  
}

rm(lakesLB);rm(lakesL)



# estuarine split up
estuaryP <- st_read('C:/HardDriveBackup/GIS/Assessment/2020IR_final/2020IR_GISData/va_aus_estuarine.shp') %>%
  st_transform(4326)   # transform to WQS84 for spatial intersection 
  # draft 2020 IR layers
  #st_read('C:/HardDriveBackup/GIS/Assessment/2020IR_draft/va_2020_aus_estuarine.shp') %>%
  #st_transform(4326)   # transform to WQS84 for spatial intersection 

# intersect subbasins with WQS to get appropriate subabsin argument for data organization
estuaryPB <- st_join(st_zm(estuaryP), dplyr::select(subbasins, BASIN_CODE, ASSESS_REG), join = st_intersects) %>%
  left_join(basinCodesConversion, by = c('BASIN_CODE' ='BASIN')) 


for(i in 1:length(unique(estuaryPB$BASIN_CODE))){
  z <- filter(estuaryPB, BASIN_CODE == as.character(unique(estuaryPB$BASIN_CODE)[i])) %>%
    select(-ASSESS_REG)
  # identify which assessment regions have data for app, only way to verify that a given waterbody belongs to a region
  z1 <- st_join(z, assessmentLayer, join = st_intersects)
  
  #for(k in 1:length(unique(z1$VAHUSB))){
  #  z2 <- filter(z1, VAHUSB == as.character(unique(z1$VAHUSB)[k]))
  #  if(nrow(z2) > 0){
  #    st_write(z2, paste0('data/GIS/processedAUs_2020draft/RL_', 
  #                        as.character(unique(z2$BASIN_CODE)),'_',
  #                        as.character(unique(z2$VAHUSB)), '.shp'), driver = "ESRI Shapefile")
  #  }
  #}
  # final 2020 location
  st_write(z1, paste0('data/GIS/processedAUs_2020final/AU_EP_', 
                      as.character(unique(z1$BASIN_CODE)),'.shp'), driver = "ESRI Shapefile")
  
  ## draft 2020 location
  #st_write(z1, paste0('data/GIS/processedAUs_2020draft/AU_EP_', 
  #                    as.character(unique(z1$BASIN_CODE)),'.shp'), driver = "ESRI Shapefile")
  
  subbasinAssessmentOptions <- tibble(waterbodyType = as.character('Estuarine'),
                                      SubbasinOptions = as.character(unique(z$BASIN_CODE)),
                                      AssessmentRegion = as.character(unique(z$ASSESS_REG)),
                                      WQS_ID_Prefix = as.character('EP'))
  subbasinOptionsByAUtype <- bind_rows(subbasinOptionsByAUtype, subbasinAssessmentOptions)
  
}

rm(estuaryPB);rm(estuaryP)
