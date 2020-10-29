# This script builds a lookup table that allows users to easily filter from subbasin to VAHU6 for
#  many application uses, but namely thinking of DEQ assessment apps.


# Bring in subbasin options
subbasins <- st_read('GIS/DEQ_VAHUSB_subbasins_EVJ.shp') %>%
  rename('SUBBASIN' = 'SUBBASIN_1')


assessmentLayer <- st_read('data/GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326))

assessmentLayerSB <- st_join(assessmentLayer, dplyr::select(subbasins, BASIN_CODE), join = st_within)  %>% # note chose st_within instead of st_intersects bc intersects blows up n with edges
  left_join(basinCodesConversion, by = c('BASIN_CODE' ='BASIN')) %>%
  dplyr::select(HUC12, VAHU6, ASSESS_REG, OFFICE_NM, VaName, Tidal, VAHUSB, FedName, HUC10, VAHU5, Basin, BASIN_CODE, Basin_Code) %>%
  st_drop_geometry()

write.csv(assessmentLayerSB, 'data/subbasinToVAHU6conversion.csv', row.names = F)
