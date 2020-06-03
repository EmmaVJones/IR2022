# Create more accurate subbasin layer for state, fix problems in Valley with upper potomac


library(tidyverse)
library(sf)
library(mapview)

# Existing subbasin layer for state, not accurate in upper potomac/shenandoah in VRO, polygons combined when shouldn't be
basin7 <- st_read('GIS/deq_basins07.shp') %>%
  st_transform(4326) %>%
  mutate(BASIN_CODE = case_when(BASIN_CODE == '3-' ~ '3',
                                BASIN_CODE == '8-' ~ '8',
                                BASIN_CODE == '9-' ~ '9',
                                TRUE ~ as.character(BASIN_CODE)))

# Dissolve Assessment layer by VAHU6 to get accurate subbasin polygon for state
assessmentLayer <- 
  st_read('GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326))


subbasins <- mutate(assessmentLayer, subbasin = str_extract(VAHU6, ".{2}")) %>%
  group_by(subbasin, ASSESS_REG) %>%
  summarise() #summarize geometry by subbasin and Assessment region
  

# check against Cleo/Emma version
cleo <- read_csv('./data_old/basinAssessmentReg_clb_EVJ.csv') %>%
  filter(VAHU6_NOTE != 'NOT IN THIS REGION')

subbasins2 <- left_join(subbasins, cleo, by = c("subbasin" = "SubbasinVAHU6code", "ASSESS_REG"))


#st_write(subbasins2, 'GIS/DEQ_VAHU_subbasins_EVJ.shp')

rm(basin7); rm(assessmentLayer); rm(subbasins); rm(subbasins2); rm(cleo)



# almost perfect but issue in 7C and 7D

subbasins <- st_read('GIS/DEQ_VAHU_subbasins_EVJ.shp')

# export assessment units that are supposed to be in 7D and dissolve
x7D <- filter(assessmentLayer, VAHU6 %in% c('AO23','AO25','AO26')) %>%
  group_by(ASSESS_REG) %>%
  summarise() %>%
  mutate(subbasin = 'AO',
         MAP = 'cbaosc',
         BASIN_NAME = 'Chesapeake Bay/Atlantic Ocean and Small Coastal',
         BASIN_CODE = '7D',
         SUBBASIN = 'Atlantic Ocean - South',
         VAHU6_NOTE = 'AO23,AO25-AO26',
         ASSESS_REG.1 = 'TRO') %>%
  select(names(subbasins))

# clip this out of 7C
x7C <- filter(subbasins, BASIN_CODE == '7C')

x7CNew <- st_difference(x7C, x7D) %>%
  dplyr::select(names(subbasins))

#mapview(x7CNew)+mapview(x7D) # perfect!

subbasinsNew <- filter(subbasins, ! BASIN_CODE %in% c('7C','7D')) %>% # get rid of old
  ## add back in new
  rbind(x7CNew) %>%
  rbind(x7D)

#mapview(subbasinsNew)

#st_write(subbasinsNew,'GIS/DEQ_VAHUSB_subbasins_EVJ.shp')

rm(subbasinsNew); rm(x7D);rm(x7C);rm(x7CNew)
