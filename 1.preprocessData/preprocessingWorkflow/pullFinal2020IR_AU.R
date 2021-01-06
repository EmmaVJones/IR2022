# pull Latest 2020 IR AU data from internal GIS REST service
# faster than waiting around for official layers to be put on X:/ drive

library(tidyverse)
library(sf)
library(geojsonsf)
library(leaflet)

lakesLink <-"https://gis.deq.virginia.gov/arcgis/rest/services/staff/DEQInternalDataViewer/MapServer/102/query?where=OBJECTID%3E0&outFields=*&f=geojson"
# pull data as independent step in case it bombs out on select
lakes2020 <- geojson_sf(lakesLink) 
# rename to match older data formats
lakes2020 <- dplyr::select(lakes2020, OBJECTID, ID305B, ACRES, CYCLE, WATER_NAME, LOCATION, CATEGORY, AU_COMMENTS, IMP_CAUSE, SOURCE, AQUA_LIFE, DEEP_CHANN, 
                           DEEP_WATER, FISH_CONSU, MIGRATORY, OPEN_WATER, PWS, RECREATION, SHELLFISH, SW_SAV, WILDLIFE, SHAPE_Length, SHAPE_Area, geometry)
glimpse(lakes2020)
# change geometry from class GEOMETRY to MULTIPOLYGON???
lakes2020 <- st_cast(lakes2020, 'MULTIPOLYGON')

lakes2020 <- bind_cols(lakes2020, st_coordinates(lakes2020) %>% as_tibble()) %>%
  mutate(Latitude = Y, Longitude = X) %>% dplyr::select(-c(X, Y))
glimpse(lakes2020)

st_write(lakes2020, 'C:/HardDriveBackup/GIS/Assessment/2020IR_draft/va_2020_aus_reservoir.shp', driver = "ESRI Shapefile")
#Error in CPL_write_ogr(obj, dsn, layer, driver, as.character(dataset_options),  : 
#Not compatible with requested type: [type=character; target=integer].
#In addition: Warning message:
#  In abbreviate_shapefile_names(obj) :
#  Field names abbreviated for ESRI Shapefile driver

# but using layer still works in R, just can't seem to get it out
leaflet() %>%
  addTiles() %>%
  addPolygons(data=lakes2020)


lakeTest <- lakes2020 %>% 
  rename(LENGTH=SHAPE_Length,COMMENTS=AU_COMMENTS)%>% 
  mutate(COMMENTS=str_sub(COMMENTS,1,50), LOCATION=str_sub( LOCATION,1,50)) %>% 
  filter(!WATER_NAME %in% "Kerr Reservoir") 



  st_write(lakeTest, 'va_2020_aus_reservoir.shp')
