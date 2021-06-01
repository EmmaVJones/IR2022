source('global.R')

dat <- read_excel('data/Draft20192020CitMonData_20210520.xlsx') %>% 
  group_by(Data_Source, GROUP_STA_ID) %>% 
  mutate(nSamples = n()) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), 
           remove = F, # don't remove these lat/lon cols from df
           crs = 4269) # add projection, needs to be geographic for now bc entering lat/lng

dat_LevelIII <- filter_at(dat, vars(contains('LEVEL_')),  all_vars(. == max(.)))


dat_simple <- dat %>% 
  group_by(Data_Source, GROUP_STA_ID, Longitude, Latitude) %>% 
  summarise(nSamples = n()) %>% 
  ungroup() %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), 
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) # add projection, needs to be geographic for now bc entering lat/lng
# smarter to do this from dat_LevelIII object instead of filtering dat_simple to dat_LevelIII$GROUP_STA_ID bc names are replicated
dat_simple_LevelIII <- dat_LevelIII%>% 
  mutate(GROUP_STA_ID_1 = paste0(GROUP_STA_ID,'_wLevel3')) %>% 
  group_by(Data_Source, GROUP_STA_ID_1, Longitude, Latitude) %>% 
  summarise(nSamples = n()) %>% 
  ungroup() %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), 
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) # add projection, needs to be geographic for now bc entering lat/lng


assessmentRegions <- vahu6

# color palette for assessment polygons
pal <- colorFactor(
  palette = topo.colors(7),
  domain = assessmentRegions$ASSESS_REG)


CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE,
             options= leafletOptions(zoomControl = TRUE,minZoom = 3, maxZoom = 20,
                                     preferCanvas = TRUE)) %>%
  setView(-79.1, 37.7, zoom=7)  %>%
  addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
              fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
              group="Assessment Regions", label = ~ASSESS_REG) %>% hideGroup('Assessment Regions') %>%
  addCircleMarkers(data = dat_simple,
                   color='blue', fillColor='gray', radius = 4,
                   fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="CitMon Stations",
                   label = ~GROUP_STA_ID, layerId = ~GROUP_STA_ID,
                   popup = leafpop::popupTable(dat_simple, zcol=c('Data_Source', 'GROUP_STA_ID', 'nSamples'))) %>%
  addCircleMarkers(data = dat_simple_LevelIII,
                   color='black', fillColor='yellow', radius = 4,
                   fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="CitMon Stations with Level III data",
                   label = ~GROUP_STA_ID_1, layerId = ~GROUP_STA_ID_1,
                   popup = leafpop::popupTable(dat_simple_LevelIII, zcol=c('Data_Source', 'GROUP_STA_ID_1', 'nSamples'))) %>%
  inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
  addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),                       
                   overlayGroups = c("CitMon Stations","CitMon Stations with Level III data",'Assessment Regions'),
                   #overlayGroups = c("Level III Ecoregions", "County", 'Assessment Regions'),
                   options=layersControlOptions(collapsed=T),
                   position='topleft') 

# SOme quick stats
assessmentRegionsSimple <- assessmentRegions %>% 
  group_by(ASSESS_REG) %>% 
  summarise()

dat_simpleAssess <- st_intersection(dat_simple , assessmentRegionsSimple)
dat_simple_LevelIIIAssess <- st_intersection(dat_simple_LevelIII , assessmentRegionsSimple)

dat_simpleAssessBreakdown <- dat_simpleAssess %>% 
  group_by(ASSESS_REG) %>% 
  summarise(`Stations Per Region` = n()) %>% 
  st_drop_geometry() %>% 
  mutate(`Total Stations` = sum(`Stations Per Region`, na.rm = T)) %>% 
  mutate(`Percent of Stations` = signif(`Stations Per Region`/`Total Stations` * 100, digits=1))

dat_simple_LevelIIIAssessBreakdown <- dat_simple_LevelIIIAssess %>% 
  group_by(ASSESS_REG) %>% 
  summarise(`Stations Per Region with Level III` = n()) %>% 
  st_drop_geometry() %>% 
  mutate(`Total Stations With LevelIII` = sum(`Stations Per Region with Level III`, na.rm = T)) %>% 
  mutate(`Percent of Stations with Level III` = signif(`Stations Per Region with Level III`/`Total Stations With LevelIII` * 100, digits=1))


report <- left_join(dat_simpleAssessBreakdown, dat_simple_LevelIIIAssessBreakdown, by = "ASSESS_REG")
