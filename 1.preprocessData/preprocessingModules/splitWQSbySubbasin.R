#assessmentRegions <- st_read( 'GIS/AssessmentRegions_simple.shp')


st_layers('GIS/WQS_layers_05082020.gdb')

# Make object to save available Subbasins for app 
subbasinOptionsByWQStype <- tibble(waterbodyType = as.character(),
                                   SubbasinOptions = as.character(),
                                   AssessmentRegion = as.character(),
                                   WQS_ID_Prefix = as.character())

# Bring in subbasin options
subbasins <- st_read('GIS/DEQ_VAHUSB_subbasins_EVJ.shp') %>%
  rename('SUBBASIN' = 'SUBBASIN_1')

# Riverine split up
riverineL <- st_read('GIS/WQS_layers_05082020.gdb', layer = 'riverine_05082020' , fid_column_name = "OBJECTID") %>%
  st_transform(4326)   # transform to WQS84 for spatial intersection 

# intersect subbasins with WQS to get appropriate subabsin argument for data organization
riverineLB <- st_join(st_zm(riverineL), dplyr::select(subbasins, BASIN_CODE, ASSESS_REG), join = st_intersects) %>%
  # and if segment doesnt make the cut for subbasins layer (the edges)
  mutate(BASIN_CODE = case_when(is.na(BASIN_CODE) ~ as.character(BASIN), 
                                TRUE ~ as.character(BASIN_CODE))) %>%
  left_join(basinCodesConversion, by = c('BASIN_CODE' ='BASIN')) %>%
  rename('Subbasin' = 'Basin_Code.y',
         'Basin_Code' = 'Basin_Code.x')


for(i in 1:length(unique(riverineLB$BASIN_CODE))){
  z <- filter(riverineLB, BASIN_CODE == as.character(unique(riverineLB$BASIN_CODE)[i]))
  # identify which assessment regions have data for app, only way to verify that a given waterbody belongs to a region
  #z1 <- st_intersection(filter(subbasins, BASIN_CODE == unique(riverineLB$BASIN_CODE)[i] ),
  #                      assessmentRegions)
  subbasinAssessmentOptions <- tibble(waterbodyType = as.character('Riverine'),
                                      SubbasinOptions = as.character(unique(z$BASIN_CODE)),
                                      AssessmentRegion = as.character(unique(z$ASSESS_REG)),
                                      WQS_ID_Prefix = as.character('RL'))
  subbasinOptionsByWQStype <- bind_rows(subbasinOptionsByWQStype, subbasinAssessmentOptions)
  st_write(z, paste0('GIS/processedWQS/RL_', 
                     unique(z$BASIN_CODE), '.shp'))
}

rm(riverineLB);rm(riverineL)




# Lacustrine split up
lakesL <- st_read('GIS/WQS_layers_05082020.gdb', layer = 'lakes_reservoirs_05082020' , fid_column_name = "OBJECTID") %>%
  st_transform(4326)   # transform to WQS84 for spatial intersection 

# intersect last cycle's lake AUs with assessmentLayer to get appropriate Basin argument for data organization
lakesLB <- st_join(st_zm(lakesL), dplyr::select(subbasins, BASIN_CODE, ASSESS_REG), join = st_intersects) %>%
  # and if segment doesnt make the cut for subbasins layer (the edges)
  mutate(BASIN_CODE = case_when(is.na(BASIN_CODE) ~ as.character(BASIN), 
                                TRUE ~ as.character(BASIN_CODE))) %>%
  left_join(basinCodesConversion, by = c('BASIN_CODE' ='BASIN')) %>%
  rename('Subbasin' = 'Basin_Code.y',
         'Basin_Code' = 'Basin_Code.x')

View(filter(lakesLB, is.na(Subbasin)))
View(filter(lakesLB, is.na(BASIN_CODE)))
View(filter(lakesLB, is.na(ASSESS_REG)))


for(i in 1:length(unique(lakesLB$BASIN_CODE))){
  z <- filter(lakesLB, BASIN_CODE == as.character(unique(lakesLB$BASIN_CODE)[i]))
  # identify which assessment regions have data for app, the only way to verify that a given waterbody belongs to a region
  #z1 <- st_intersection(filter(basin7, BASIN_CODE == unique(riverineLB$BASIN_CODE)[i] ),
  #                      assessmentRegions)
  subbasinAssessmentOptions <- tibble(waterbodyType = as.character('Lacustrine'),
                                      SubbasinOptions = as.character(unique(z$BASIN_CODE)),
                                      AssessmentRegion = as.character(unique(z$ASSESS_REG)),
                                      WQS_ID_Prefix = as.character('LP'))
  subbasinOptionsByWQStype <- bind_rows(subbasinOptionsByWQStype, subbasinAssessmentOptions)
  st_write(z, paste0('GIS/processedWQS/LP_', 
                     unique(z$BASIN_CODE), '.shp'))
}


rm(lakesLB);rm(lakesL)





# Estuary Lines split up

estuaryL <- st_read('GIS/WQS_layers_05082020.gdb', layer = 'estuarinelines_05082020' , fid_column_name = "OBJECTID") %>%
  st_transform(4326)   # transform to WQS84 for spatial intersection 

# intersect last cycle's lake AUs with assessmentLayer to get appropriate Basin argument for data organization
estuaryLB <- st_join(st_zm(estuaryL), dplyr::select(subbasins, BASIN_CODE, ASSESS_REG), join = st_intersects) %>%
  # and if segment doesnt make the cut for subbasins layer (the edges)
  mutate(BASIN_CODE = case_when(is.na(BASIN_CODE) ~ as.character(BASIN), 
                                TRUE ~ as.character(BASIN_CODE))) %>%
  left_join(basinCodesConversion, by = c('BASIN_CODE' ='BASIN')) %>%
  rename('Subbasin' = 'Basin_Code.y',
         'Basin_Code' = 'Basin_Code.x')


View(filter(estuaryLB, is.na(Subbasin)))
View(filter(estuaryLB, is.na(BASIN_CODE)))
View(filter(estuaryLB, is.na(ASSESS_REG)))


for(i in 1:length(unique(estuaryLB$BASIN_CODE))){
  z <- filter(estuaryLB, BASIN_CODE == as.character(unique(estuaryLB$BASIN_CODE)[i]))
  # identify which assessment regions have data for app, takes forever but only way to verify that a given waterbody belongs to a region
  # note difference for estuarine!
  #z1 <- st_intersection(z,assessmentRegions)
  subbasinAssessmentOptions <- tibble(waterbodyType = as.character('Estuarine'),
                                      SubbasinOptions = as.character(unique(z$BASIN_CODE)),
                                      AssessmentRegion = as.character(unique(z$ASSESS_REG)),
                                      WQS_ID_Prefix = as.character('EL'))
  subbasinOptionsByWQStype <- bind_rows(subbasinOptionsByWQStype, subbasinAssessmentOptions)
  st_write(z, paste0('GIS/processedWQS/EL_', 
                      unique(z$BASIN_CODE), '.shp'))
}


rm(estuaryLB);rm(estuaryL)




# Estuary Polygonss split up


estuaryP <- st_read('GIS/WQS_layers_05082020.gdb', layer = 'estuarinepolygons_05082020' , fid_column_name = "OBJECTID") %>%
  st_transform(4326)   # transform to WQS84 for spatial intersection 

# intersect last cycle's lake AUs with assessmentLayer to get appropriate Basin argument for data organization
estuaryPB <- st_join(st_zm(estuaryP), dplyr::select(subbasins, BASIN_CODE, ASSESS_REG), join = st_intersects) %>%
  # and if segment doesnt make the cut for subbasins layer (the edges)
  mutate(BASIN_CODE = case_when(is.na(BASIN_CODE) ~ as.character(BASIN), 
                                TRUE ~ as.character(BASIN_CODE))) %>%
  left_join(basinCodesConversion, by = c('BASIN_CODE' ='BASIN')) %>%
  rename('Subbasin' = 'Basin_Code.y',
         'Basin_Code' = 'Basin_Code.x')


View(filter(estuaryPB, is.na(Subbasin)))
View(filter(estuaryPB, is.na(BASIN_CODE)))
View(filter(estuaryPB, is.na(ASSESS_REG)))


for(i in 1:length(unique(estuaryPB$BASIN_CODE))){
  z <- filter(estuaryPB, BASIN_CODE == as.character(unique(estuaryPB$BASIN_CODE)[i]))
  # identify which assessment regions have data for app, takes forever but only way to verify that a given waterbody belongs to a region
  # note difference for estuarine!
  #z1 <- st_intersection(z,assessmentRegions)
  subbasinAssessmentOptions <- tibble(waterbodyType = as.character('Estuarine'),
                                      SubbasinOptions = as.character(unique(z$BASIN_CODE)),
                                      AssessmentRegion = as.character(unique(z$ASSESS_REG)),
                                      WQS_ID_Prefix = as.character('EP'))
  subbasinOptionsByWQStype <- bind_rows(subbasinOptionsByWQStype, subbasinAssessmentOptions)
  st_write(z, paste0('GIS/processedWQS/EP_', 
                     unique(z$BASIN_CODE), '.shp'))
}


rm(estuaryPB);rm(estuaryP)

rm(z); rm(z1); rm(subbasinAssessmentOptions); rm(i)


z <- filter(subbasinOptionsByWQStype, is.na(AssessmentRegion)) #manually check everyone is there already
# all looks good, can drop na's
subbasinOptionsByWQStype <- filter(subbasinOptionsByWQStype, ! is.na(AssessmentRegion))

write.csv(subbasinOptionsByWQStype, 'data/subbasinOptionsByWQStype&Region.csv', row.names = F)

rm(z)
# Now cross reference with Cleo's QAed information and drop subbasin/Assessment region options that don't pass her test
#basinAssessmentRegion <- read_csv('data/basinAssessmentReg_clb_EVJ.csv')  # Cleo QAed verison

#subbasinOptionsByWQStype1 <- left_join(subbasinOptionsByWQStype, basinAssessmentRegion, 
#                                       by = c('SubbasinOptions' = 'BASIN_CODE',
#                                              'AssessmentRegion' = 'ASSESS_REG')) %>%
#  filter(! VAHU6_NOTE %in% c('NOT IN THIS REGION', NA))

#write.csv(subbasinOptionsByWQStype, 'data/subbasinOptionsByWQStype&Region.csv', row.names = F)
