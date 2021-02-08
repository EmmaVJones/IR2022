source('global.R')

conventionals_DWQS <- readRDS('data/conventionals_D.RDS') %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) %>%
  mutate(StationID= FDT_STA_ID)

assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
assessmentLayer <- st_read('data/GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) 
subbasins <- st_read('data/GIS/DEQ_VAHUSB_subbasins_EVJ.shp') %>%
  rename('SUBBASIN' = 'SUBBASIN_1')

assessmentType1 <- 'Riverine' #'Estuarine'#
AUDEQregionSelection1 <- "BRRO"#"TRO"#"NRO"##"CO"#
  # filter(subbasinOptionsByWQStype, waterbodyType %in% assessmentType1) %>%
  # distinct(AssessmentRegion) %>% 
  # pull()
AUsubbasinSelection1 <-  "James-Middle"#"York"#"Potomac-Lower"#"Roanoke"# 'Small Coastal'#"James-Middle"#"James-Middle"
filter(subbasinOptionsByWQStype, waterbodyType %in% assessmentType1) %>%
  filter(AssessmentRegion %in% AUDEQregionSelection1) %>%
  {if(AUDEQregionSelection1 == 'TRO') # no AU polygons in this combo even though in WQS
    filter(., Basin_Code != 'Chowan-Dismal')
    else .} %>%
  distinct(Basin_Code) %>%
  pull()

basinCodesAU1 <- filter(subbasinOptionsByWQStype, waterbodyType %in% assessmentType1) %>%
    filter(AssessmentRegion %in% AUDEQregionSelection1) %>%
    filter(Basin_Code %in% AUsubbasinSelection1) %>%
    distinct(SubbasinOptions) %>% 
    pull()


typeName1 <- filter(WQSlayerConversion, waterbodyType %in% assessmentType1) %>%
  distinct(WQS_ID) %>% 
  pull() 



if(length(basinCodesAU1) > 1){ # in case more than 1 basin code in basin
  AUs <- paste0('data/GIS/processedAUs_2020draft/AU_', typeName1[1],'_',basinCodesAU1,'.shp' ) %>%                       # change to final
    map(st_read) %>%
    reduce(rbind) %>%
    st_transform(4326) %>%
    st_zm()
} else {
  AUs <- st_zm(
    st_read(paste0('data/GIS/processedAUs_2020draft/AU_', typeName1[1],'_',basinCodesAU1,'.shp' ))) %>%           # change to final
    st_transform(4326) }


if(assessmentType1 == 'Riverine'){otherLayers <- ";2020%20Draft%20ADB%20WQA%20Layers;2020%20Rivers%20(Any%20Use)"}
if(assessmentType1 == 'Lacustrine'){otherLayers <- ";2020%20Draft%20ADB%20WQA%20Layers;2020%20Reservoirs%20(Any%20Use)"}
if(assessmentType1 == 'Estuarine'){otherLayers <- ";2020%20Draft%20ADB%20WQA%20Layers;2020%20Estuaries%20(Any%20Use)"}


# Data already analyzed by some user
userReviews <-  loadData("AUlookupTable")



# All sites that were analyzed in preprocessing
original_input <- read.csv('data/preAnalyzedAUdata.csv') %>% # read_csv was not working with parsing errors
  rename('Buffer Distance' = 'Buffer.Distance') %>%
  mutate(`Spatially Snapped` = case_when(is.na(`Buffer Distance`) ~ F,
                                         TRUE ~ TRUE),
         Comments = NA) %>% # mark what needs to be reviewed and add comment field
  mutate(`Buffer Distance` = ifelse(`Buffer Distance` == 'In polygon',NA, as.character(`Buffer Distance`)), # and change polygons back to NA to not mess up color pal
         ToHUC = as.numeric(as.character(ToHUC))) %>% # make sure factors don't get wonky
  filter(! FDT_STA_ID %in% userReviews$FDT_STA_ID) %>% # drop stations users have reviewed
  #dplyr::select(names(userReviews))
  bind_rows(userReviews) %>%
  filter(FDT_STA_ID != 'FakeStation') #drop fake line of data that forces userReviews into proper data format



# All sites limited to waterbody type and subbasin and region
snap_input_region <- original_input %>%
  filter(ASSESS_REG %in% AUDEQregionSelection1) %>% # region filter
  {if(AUDEQregionSelection1 == 'CO')
    . 
    # extra step to make sure central office weird AU naming schema isn't dropped bc not all have _
    else filter(., gsub("_", "", str_extract(ID305B_1, ".{1}_")) %in% 
                  str_extract(filter(WQSlayerConversion, waterbodyType %in% assessmentType1) %>%
                                distinct(WQS_ID) %>%
                                {if(assessmentType1 == 'Estuarine')
                                  filter(., WQS_ID == 'EP') 
                                  else .} %>%
                                pull(), ".{1}" ) ) }  %>% # complicated assessment type filter
  # add back in sites that did not snap to anything bc they are lost on the previous filter based on ID305B_1 field
  bind_rows(
    original_input %>%
      filter(ASSESS_REG %in% AUDEQregionSelection1) %>%
      filter(is.na(ID305B_1))
  ) %>%
  # first make sure everyone has lat/lng info
  left_join(dplyr::select(conventionals_DWQS, FDT_STA_ID, Latitude, Longitude), by = 'FDT_STA_ID' ) %>% # just using this for spatial data since some missing from original_input
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = T, # remove these lat/lon cols from df
           crs = 4326)
#subbasin intersection (string searching FDT_STA_ID is too unreliable here bc mix of 2A and 2-)
snap_input_region1 <-  st_intersection(snap_input_region, dplyr::select(subbasins,subbasin)) %>%
  st_drop_geometry() %>% # back to tibble
  rename('Buffer Distance' = 'Buffer.Distance', 'Spatially Snapped' = 'Spatially.Snapped')
# add back in stations that were dropped by joining to subbasins because they fall outside polygon boundary
if(nrow(snap_input_region) != nrow(snap_input_region1)){
  snap_input_region <-  bind_rows(snap_input_region1,
                                  filter(snap_input_region %>% st_drop_geometry(), !FDT_STA_ID %in% snap_input_region1$FDT_STA_ID) %>%
                                    # if outside the spatial framework then just take the basin information from CEDS data
                                    left_join(dplyr::select(subbasins, BASIN_CODE, subbasin, ASSESS_REG) %>% st_drop_geometry(),
                                              by = c('VAHUSB' = 'subbasin', 'ASSESS_REG')) %>%
                                    mutate(BASIN_CODE.x = BASIN_CODE.y) %>%
                                    rename('BASIN_CODE' = 'BASIN_CODE.x') %>%
                                    dplyr::select(-BASIN_CODE.y) ) %>%
    # after fixing all subbasin issues outside the polygon borders for region then filter to chosen subbasin
    filter(BASIN_CODE %in% basinCodesAU1)
} else {snap_input_region <- snap_input_region1 %>%
  # be sure to filter to chosen subbasin
  filter(BASIN_CODE %in% basinCodesAU1)  }
  ## careful this method drops stations that fall outside polygons
  ##st_intersection(., filter(subbasins, BASIN_CODE %in% basinCodesAU1) %>%
  ##                  dplyr::select(subbasin)) %>%

## # Fix messed up string search bc VACB ID305B doesn't follow conventions
## if(assessmentType1 == 'Estuarine' & AUDEQregionSelection1 == 'CO'){
##   snap_input_region <- rbind(snap_input_region,
##                                               filter(original_input, str_extract(ID305B_1, ".{4}") == 'VACB') %>%
##                                                 left_join(dplyr::select(conventionals_DWQS, FDT_STA_ID, Latitude, Longitude), by = 'FDT_STA_ID' ) %>% # just using this for spatial data since some missing from original_input
##                                                 mutate(subbasin = 'CB') %>% 
##                                                 dplyr::select(names(snap_input_region))    )}


# limit conventionals_DWQS to just chosen subbasin
conventionals_DAU_Region <- st_intersection(conventionals_DWQS,
                                                             filter(subbasins, BASIN_CODE %in% basinCodesAU1)) %>%
  mutate(`DEQ GIS Web App Link` =  paste0(webLinkpart1, StationID, webLinkpart2, otherLayers, webLinkpart3)) %>%
  dplyr::select(`DEQ GIS Web App Link`, everything())

# Make dataset of all sites for highlighting purposes, preliminary list
sitesUnique <- snap_input_region %>%
  full_join(dplyr::select(conventionals_DWQS, FDT_STA_ID, Latitude, Longitude), by = 'FDT_STA_ID') %>%
  left_join(dplyr::select(conventionals_DAU_Region, FDT_STA_ID, `DEQ GIS Web App Link` ) %>% 
              st_drop_geometry(), by = 'FDT_STA_ID') %>%
  filter(!is.na(Latitude) | !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326)

# Make dataset of multiple segments snapped to single site IN REGION
tooMany <- filter(snap_input_region, n > 1) %>%
  filter(`Spatially Snapped` == T) %>% # only want ones user needs to deal with
  group_by(FDT_STA_ID) %>% mutate(colorFac = row_number()) %>% ungroup() 
# Make a dataset of actual segments for plotting
tooMany_sf <- filter(AUs(), ID305B %in% tooMany$ID305B_1) %>%
  left_join(tooMany, by = c('ID305B' = 'ID305B_1')) %>%
  dplyr::select(FDT_STA_ID, ID305B, `Buffer Distance`, n, everything())
# Make dataset of sites associated with too many segments IN REGION
tooMany_sites <- filter(sitesUnique, FDT_STA_ID %in% tooMany$FDT_STA_ID) %>%
  distinct(FDT_STA_ID, .keep_all = T)# %>%
#dplyr::select(-c(WQS_ID, `Buffer Distance`, n))

# Make dataset of sites that snapped to a single AU and IN REGION
snapSingle <- filter(sitesUnique, n == 1) %>%
  filter(FDT_STA_ID %in% filter(snap_input_region, n == 1)$FDT_STA_ID) %>%#
  filter(`Spatially Snapped` == T) # only want ones user needs to deal with
if(all(is.na(snapSingle$`Buffer Distance`))){ # the filter doesn't work if all NA for some reason
  snapSingle <- mutate(snapSingle, `Buffer Distance` = as.factor(`Buffer Distance`))
} else{
  if(AUDEQregionSelection1 != 'CO') # special case again for CO, CO only appears in Estuarine so doesn't change other things
    snapSingle <- filter(snapSingle, `Buffer Distance` != 'No connections within 80 m') %>%
      mutate(`Buffer Distance` = as.factor(`Buffer Distance`))
  else{snapSingle <-  mutate(snapSingle, `Buffer Distance` = as.factor(`Buffer Distance`))} }
  

# Make a dataset of actual segments that snapped to a single site for plotting
snapSingle_sf <- filter(AUs, ID305B %in% snapSingle$ID305B_1) %>%
  left_join(dplyr::select( snapSingle, FDT_STA_ID, `Buffer Distance`, n, ID305B_1) %>% st_drop_geometry(), 
            by = c('ID305B' = 'ID305B_1')) %>%
  dplyr::select(FDT_STA_ID, ID305B, `Buffer Distance`, n, everything())


# Make dataset of sites associated with no segments IN REGION
snapNone <-  filter(sitesUnique, is.na(ID305B_1)) %>%
  filter(FDT_STA_ID %in% filter(snap_input_region, is.na(ID305B_1) | `Buffer Distance` != 'No connections within 80 m')$FDT_STA_ID) %>%
  filter(`Spatially Snapped` == T) %>% # only want ones user needs to deal with
  filter(FDT_STA_ID %in% snap_input_region$FDT_STA_ID) # limit assignment to just what falls in a region
# Make empty dataset of sites that assessors touched
sitesAdjusted <-  sitesUnique[0,]  %>%
  mutate(Comments = as.character())
# Make dataset for user to download
#finalAU <- original_input
finalAU <- userReviews %>%
  rbind(  sitesAdjusted %>% 
            st_drop_geometry() %>%
            dplyr::select(-c(subbasin, Latitude,Longitude,`DEQ GIS Web App Link`)) )  # drop extra data



# whne clicked Accept
 
namesToSmash <- c("2BJMS267.05")#, "2BJMS259.12" )
acceptCommentAU <- 'my comment'

sitesUpdated <- filter(sitesUnique, FDT_STA_ID %in% namesToSmash) %>%
  dplyr::select(-c(subbasin, Latitude,Longitude,`DEQ GIS Web App Link`)) %>% # drop extra data
  mutate(`Buffer Distance` = paste0('Manual Review | ', `Buffer Distance`),
         `Spatially Snapped` = FALSE, # update this field so next import it will not be brought into app
         Comments = paste0('Manual Accept | ',acceptCommentAU))# %>%
# dplyr::select(FDT_STA_ID, ID305B_1, `Buffer Distance`, Comments)


# add the current site(s) to the adjusted list 
#if(nrow(sitesAdjusted) == 0){
#  sitesAdjusted
#} else {
sitesAdjusted <- rbind(sitesAdjusted, sitesUpdated) # rbind works better for sf objects
#}

dropMe <- unique(sitesUpdated$FDT_STA_ID)

## Remove Site from "to do' list
# remove from snap to > 1 AU sites and segments
tooMany_sites <- filter(tooMany_sites, !(FDT_STA_ID %in% dropMe)) # drop sites
tooMany_sf <- filter(tooMany_sf, !(FDT_STA_ID %in% dropMe)) # drop segments


# and if part of snap to 1 AU, fix that data
snapSingle <- filter(snapSingle, !(FDT_STA_ID%in% dropMe)) # drop sites
snapSingle_sf <- filter(snapSingle_sf, !(FDT_STA_ID %in% dropMe)) # drop segments

# and if part of snap to 0 AU, fix that data
snapNone <- filter(snapNone, !(FDT_STA_ID %in% dropMe)) # drop sites

# update output dataset
finalAU <-# filter(finalAU, !(FDT_STA_ID %in% dropMe)) %>% bind_rows(sitesUpdated)
  bind_rows(finalAU, sitesUpdated %>% st_drop_geometry()) 



# need to force FDT_DEPTH to double





































# Data breakdown
z <- filter(original_input, `Spatially Snapped` == TRUE) %>%
  left_join(dplyr::select(conventionals_DWQS, FDT_STA_ID, Latitude, Longitude), by = 'FDT_STA_ID' ) %>% # just using this for spatial data since some missing from original_input
  distinct(FDT_STA_ID, .keep_all = T) %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = T, # remove these lat/lon cols from df
           crs = 4326) 
#subbasin intersection (string searching FDT_STA_ID is too unreliable here bc mix of 2A and 2-)
z1 <-  st_intersection(z, dplyr::select(subbasins,subbasin)) %>%
  st_drop_geometry() %>% # back to tibble
  rename('Buffer Distance' = 'Buffer.Distance', 'Spatially Snapped' = 'Spatially.Snapped')
# add back in stations that were dropped by joining to subbasins because they fall outside polygon boundary
if(nrow(z) != nrow(z1)){
  z <-   bind_rows(z1,
                   filter(z %>% st_drop_geometry(), !FDT_STA_ID %in% z1$FDT_STA_ID) %>%
                     # if outside the spatial framework then just take the basin information from CEDS data
                     left_join(dplyr::select(subbasins, BASIN_CODE, subbasin, ASSESS_REG) %>% st_drop_geometry(),
                               by = c('VAHUSB' = 'subbasin', 'ASSESS_REG')) %>%
                     mutate(BASIN_CODE.x = BASIN_CODE.y) %>%
                     rename('BASIN_CODE' = 'BASIN_CODE.x') %>%
                     dplyr::select(-BASIN_CODE.y) ) 
                                         
    
    
    
    # bind_rows(z1,
    #               filter(z %>% st_drop_geometry(), !FDT_STA_ID %in% z1$FDT_STA_ID) %>%
    #                 mutate(subbasinJoin = str_extract(FDT_STA_ID, ".{2}")) %>% # grab first two characters to identify subbasins
    #                 mutate(subbasinJoin = case_when(subbasinJoin == '9-' ~ '9',
    #                                                 subbasinJoin == '3-' ~ '3',
    #                                                 subbasinJoin == '8-' ~ '8', 
    #                                                 TRUE ~ as.character(subbasinJoin))) %>% # alter X- format to something that can join
    #                 left_join(dplyr::select(subbasins, BASIN_CODE, subbasin) %>% st_drop_geometry(),
    #                           by = c('subbasinJoin' = 'BASIN_CODE')) %>%
    #                 distinct(FDT_STA_ID, .keep_all = TRUE) %>% # run a distinct here to avoid duplicate rows, just chooses first alphabetically
    #                 dplyr::select(-subbasinJoin))
                                 
} else {z <- z1}
## careful this method drops stations that fall outside polygons
##st_intersection(., filter(subbasins, BASIN_CODE %in% basinCodesAU1) %>%
##                  dplyr::select(subbasin)) %>%

z <- z %>% 
  #st_drop_geometry() %>% # back to tibble
  #rename('Buffer Distance' = 'Buffer.Distance', 'Spatially Snapped' = 'Spatially.Snapped') %>% 
  mutate(Type = gsub("_", "", str_extract(ID305B_1, ".{1}_"))) %>% 
  mutate(`Assessment Type` = case_when(Type == 'E' ~ 'Estuarine', 
                                       Type == 'R' ~ 'Riverine', 
                                       Type == 'L' ~ 'Lacustrine', 
                                       is.na(Type) ~ 'Not sure yet',
                                       TRUE ~ as.character(Type))) %>%
  distinct(FDT_STA_ID, .keep_all = T)

# remove distinct above to get this information
# nrow(filter(z, is.na(ID305B_1))) # 24 sites with no ID305B suggestions
# nrow(filter(z, n >1)) # 20 sites with more than 1 ID305B suggestion
# nrow(filter(z, n == 1 & !is.na(ID305B_1))) # 356 sites with more than 1 ID305B suggestion


z %>% group_by(ASSESS_REG) %>% summarise(`Total Sites for Review` = n()) 
z %>% group_by(ASSESS_REG, BASIN_CODE) %>% summarise(`Sites for Review` = n()) 
View(
z %>% group_by(ASSESS_REG, BASIN_CODE, `Assessment Type`) %>% summarise(`Sites for Review` = n())  %>%
  left_join(z %>% group_by(ASSESS_REG) %>% summarise(`Total Sites for Review` = n()),
            by = 'ASSESS_REG'))
write.csv(z %>% group_by(ASSESS_REG, BASIN_CODE, `Assessment Type`) %>% summarise(`Sites for Review` = n())  %>%
            left_join(z %>% group_by(ASSESS_REG) %>% summarise(`Total Sites for Review` = n()),
                      by = 'ASSESS_REG'),
          'AUbreakdown.csv', row.names = F)


test <- filter(z, ASSESS_REG == 'TRO' & BASIN_CODE == '8') %>%
  group_by(ASSESS_REG, BASIN_CODE, `Assessment Type`) %>%
  mutate(`Sites for Review` = n()) %>% 
  dplyr::select(ASSESS_REG, BASIN_CODE, `Assessment Type`, `Sites for Review`, everything())


test1 <- filter(z, ASSESS_REG == 'BRRO') %>% 
  dplyr::select(FDT_STA_ID, ID305B_1, SUBBASIN, BASIN_CODE, `Buffer Distance`, `Assessment Type`, everything())


































mutate(subbasinJoin = str_extract(FDT_STA_ID, ".{2}")) %>% # grab first two characters to identify subbasins
  mutate(subbasinJoin = case_when(subbasinJoin == '9-' ~ '9',
                                  subbasinJoin == '3-' ~ '3',
                                  subbasinJoin == '8-' ~ '8', 
                                  TRUE ~ as.character(subbasinJoin))) %>% # alter X- format to something that can join
  left_join(dplyr::select(subbasins, BASIN_CODE, subbasin) %>% st_drop_geometry(),
            by = c('subbasinJoin' = 'BASIN_CODE')) %>%
  distinct(FDT_STA_ID, .keep_all = TRUE) %>% # run a distinct here to avoid duplicate rows, just chooses first alphabetically
  dplyr::select(-subbasinJoin)) %>%