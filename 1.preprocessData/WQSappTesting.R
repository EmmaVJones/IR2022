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



WQSwaterbodyType1 <-'Estuarine'#'Riverine' #  'Estuarine'#

WQSDEQregionSelection1 <- "CO"#"SWRO"#"TRO"#'BRRO'#
  #filter(subbasinOptionsByWQStype, waterbodyType %in% WQSwaterbodyType1) %>%
  #distinct(AssessmentRegion) %>% 
  #pull()

WQSsubbasinSelection1 <-  "Small Coastal"#"New"#'Chowan-Albermarle'# "Small Coastal"#"James-Middle"#
  #filter(subbasinOptionsByWQStype, waterbodyType %in% WQSwaterbodyType1) %>%
  #filter(AssessmentRegion %in% WQSDEQregionSelection1) %>%
  #distinct(Basin_Code) %>% 
  #pull() 

basinCodes1 <- filter(subbasinOptionsByWQStype, waterbodyType %in% WQSwaterbodyType1) %>%
    filter(AssessmentRegion %in% WQSDEQregionSelection1) %>%
    filter(Basin_Code %in% WQSsubbasinSelection1) %>%
    distinct(SubbasinOptions) %>% 
    pull() 


# helper to get right wqs layer in
typeName1 <- filter(WQSlayerConversion, waterbodyType %in% WQSwaterbodyType1) %>%
  distinct(WQS_ID) %>% 
  pull() 

if(length(basinCodes1) > 1){
  WQSs1 <- st_zm(st_read(paste0('data/GIS/processedWQS/',typeName1[1],'_', basinCodes1[1], '.shp') , 
                        fid_column_name = "OBJECTID")) %>%
    rbind(st_zm(st_read(paste0('data/GIS/processedWQS/',typeName1[1],'_', basinCodes1[2], '.shp') , 
                        fid_column_name = "OBJECTID"))) %>%
    rbind(st_zm(st_read(paste0('data/GIS/processedWQS/',typeName1[1],'_', basinCodes1[3], '.shp') , 
                        fid_column_name = "OBJECTID")))
} else { WQSs1 <- st_zm(st_read(paste0('data/GIS/processedWQS/',typeName1[1],'_', basinCodes1, '.shp') ,
                        fid_column_name = "OBJECTID"))    }
WQSs1 <- WQSs1 %>%
    st_transform(4326) %>%
    rename("GNIS_Name" = "GNIS_Nm",
           "WATER_NAME" = "WATER_N" ,
           "WQS_COMMENT" = "WQS_COM" ,
           "Basin_Code" = "Basn_Cd",
           "Edit_Date"  = "Edit_Dt",
           "Tier_III" = "Tir_III" ,
           "SECTION_DESCRIPTION" = 'SECTION',
           "created_user" = "crtd_sr",      
           "created_date" ="crtd_dt",
           "last_edited_user" = "lst_dtd_s",
           "last_edited_date" = "lst_dtd_d", "Shape_Length" = "Shp_Lng", 
           "BASIN_CODE" = "BASIN_C", "ASSESS_REG"="ASSESS_" , "Subbasin" = "Subbasn") %>%
    dplyr::select(WQS_ID, everything()) %>%
    {if(WQSwaterbodyType1 %in% c('Lacustrine', 'Estuarine'))
      rename(., "Shape_Area" = "Shap_Ar")
      else .}

if(length(basinCodes1) > 1){
  WQSsEL1 <- #withProgress(message = 'Reading in Additional Estuarine Spatial File',
               st_zm(st_read(paste0('data/GIS/processedWQS/',typeName1[2],'_', basinCodes1[1], '.shp') , fid_column_name = "OBJECTID")) %>%
               rbind(st_zm(st_read(paste0('data/GIS/processedWQS/',typeName1[2],'_', basinCodes1[2], '.shp') , fid_column_name = "OBJECTID"))) %>%
                 rbind(st_zm(st_read(paste0('data/GIS/processedWQS/',typeName1[2],'_', basinCodes1[3], '.shp') , fid_column_name = "OBJECTID")))
               #) 
  } else {WQSsEL1 <- #withProgress(message = 'Reading in Additional Estuarine Spatial File',
                               st_zm(
                                 st_read(paste0('data/GIS/processedWQS/',typeName1[2],'_', basinCodes1, '.shp') , fid_column_name = "OBJECTID"))  }
WQSsEL1 <- WQSsEL1 %>%
  st_transform(4326) %>%
    # match polygon structure
    rename("GNIS_Name" = "GNIS_Nm",
           "WATER_NAME" = "WATER_N" ,
           #"WQS_COMMENT" = "WQS_COMMEN" , 
           "WQS_COMMENT" = "WQS_COM" ,
           "Basin_Code" = "Basn_Cd",
           "Edit_Date"  = "Edit_Dt",
           "Tier_III" = "Tir_III" ,
           "SECTION_DESCRIPTION" = 'SECTION',
           "created_user" = "crtd_sr",      
           "created_date" ="crtd_dt",
           "last_edited_user" = "lst_dtd_s",
           "last_edited_date" = "lst_dtd_d", "Shape_Length" = "Shp_Lng", 
           "BASIN_CODE" = "BASIN_C", "ASSESS_REG"="ASSESS_" , "Subbasin" = "Subbasn") %>%  # match polygon structure
    mutate(Shape_Area = NA) %>%
    dplyr::select(WQS_ID, names(WQSs1))  


WQSlookup <- loadData("WQSlookupTable")

# Weblink component based on WQSwaterbodyType1
if(WQSwaterbodyType1 == 'Riverine'){otherLayers <- "Streams/Rivers%20WQS;Public%20water%20supply;Trout;All%20other%20streams/rivers"}
if(WQSwaterbodyType1 == 'Lacustrine'){otherLayers <- "Lakes/Reservoirs%20WQS;Public%20Water%20Supply;Trout;All%20other%20lakes/reservoirs"}
if(WQSwaterbodyType1 == 'Estuarine'){otherLayers <- "Estuaries%20WQS;Estuarine%20waters;Tidal%20flow%20paths"}

conventionals_DWQS_Region <- st_intersection(conventionals_DWQS, 
                                             filter(subbasins, BASIN_CODE %in% basinCodes1)) %>%
  mutate(`DEQ GIS Web App Link` =  paste0(webLinkpart1, StationID, webLinkpart2, otherLayers, webLinkpart3)) %>%
  dplyr::select(`DEQ GIS Web App Link`, everything())




snap_input <- readRDS('data/WQStable02032021.RDS') %>% # February 2021 update prior to official 2022 IR
  #  readRDS('data/WQStable.RDS') %>% # original effort
  filter(str_extract(WQS_ID, "^.{2}") %in% filter(WQSlayerConversion, waterbodyType %in% WQSwaterbodyType1)$WQS_ID) %>%
  filter(gsub("_","",str_extract(WQS_ID, ".{3}_")) %in% 
           str_pad(unique(filter(subbasinOptionsByWQStype, SubbasinOptions %in% basinCodes1)$SubbasinOptions), 
                   width = 2, side = 'left', pad = '0')) %>%
  # filter out any sites that happen to have existing WQS_ID
  filter(! StationID %in% WQSlookup$StationID) %>%
  group_by(StationID) %>%
  mutate(n = n()) %>% ungroup()
# Sites limited to just region of interest
snap_input_Region <- snap_input %>%
  left_join(conventionals_DWQS_Region, by = 'StationID') %>%
  filter(!is.na(Latitude) | !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = T, # don't remove these lat/lon cols from df
           crs = 4326) %>%
  st_intersection(filter(assessmentRegions, ASSESS_REG %in% WQSDEQregionSelection1)) %>%
  st_drop_geometry() %>% # back to tibble
  rename('Buffer Distance' = 'Buffer.Distance') %>%
  dplyr::select(StationID, WQS_ID, `Buffer Distance`, n) %>%
  # Add back any missing sites that are dropped because they don't fall into assessment region boundaries
  bind_rows(
    filter(snap_input, StationID %in% 
             filter(readRDS('data/missingSites.RDS'), 
                    ASSESS_REG %in% WQSDEQregionSelection1)$FDT_STA_ID) %>%
      left_join(conventionals_DWQS, by = 'StationID') %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`, n) )
# Make dataset of all sites for highlighting purposes, preliminary list
sitesUnique <- snap_input %>%
  full_join(conventionals_DWQS_Region, by = 'StationID') %>%
  # catch for sites outside a region
  {if(nrow(filter(.,is.na(FDT_STA_ID))) > 0)
    dplyr::select(., names(snap_input)) %>%
      left_join( conventionals_DWQS, by = 'StationID') %>%
      mutate(`DEQ GIS Web App Link` =  paste0(webLinkpart1, StationID, webLinkpart2, otherLayers, webLinkpart3)) %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, `DEQ GIS Web App Link`, everything())
    else .} %>%
  filter(!is.na(Latitude) | !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326)
# Make dataset of multiple segments snapped to single site IN REGION
tooMany <- filter(snap_input_Region, n > 1) %>%
  group_by(StationID) %>% mutate(colorFac = row_number()) %>% ungroup() 
# Make a dataset of actual segments for plotting
tooMany_sf <- filter(WQSs1, WQS_ID %in% tooMany$WQS_ID) %>%
  left_join(tooMany, by = 'WQS_ID') %>%
  dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything())
if(WQSwaterbodyType1 == 'Estuarine'){
  tooMany_sf_EL <- filter(WQSsEL1, WQS_ID %in% tooMany$WQS_ID) %>%              # bonus polyline feature for Estuarine
    left_join(tooMany, by = 'WQS_ID') %>%
    dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything())    
} else {tooMany_sf_EL <- WQSs1[0,] %>%
  left_join(tooMany, by = 'WQS_ID') %>%
  dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything())} # create dummy variable
# Make dataset of sites associated with too many segments IN REGION
tooMany_sites <- filter(sitesUnique, StationID %in% tooMany$StationID) %>%
  left_join(WQSs1 %>% st_drop_geometry(), by = 'WQS_ID') %>%
  {if(WQSwaterbodyType1 == 'Estuarine')
    rbind(left_join(filter(sitesUnique, StationID %in% tooMany$StationID), 
                    WQSsEL1 %>% st_drop_geometry(), by = 'WQS_ID'))
    else . } %>%
  distinct(StationID, .keep_all = T) %>%
  dplyr::select(-c(WQS_ID, `Buffer Distance`, n))
# Make dataset of sites that snapped to a single WQS and join WQS info  IN REGION
snapSingle <- filter(sitesUnique, n == 1 ) %>%
  {if(WQSwaterbodyType1 == 'Riverine')
    filter(., `Buffer Distance` != 'No connections within 80 m')
    else . } %>%
  filter(StationID %in% snap_input_Region$StationID) %>% # limit assignment to just what falls in a region
  left_join(WQSs1 %>% st_drop_geometry(), by = 'WQS_ID') %>%
  {if(WQSwaterbodyType1 == 'Estuarine')
    filter(., str_extract(WQS_ID, "^.{2}") == 'EP') %>% # keep just polygon result from above
      rbind(filter(sitesUnique, n == 1) %>%
              filter(StationID %in% snap_input_Region$StationID & str_extract(WQS_ID, "^.{2}") == 'EL') %>%
              left_join(WQSsEL1 %>% st_drop_geometry(), by = 'WQS_ID') )
    else . } %>%
  mutate(`Buffer Distance` = as.factor(`Buffer Distance`))
# Make a dataset of actual segments that snapped to a single site for plotting
snapSingle_sf <- filter(WQSs1, WQS_ID %in% snapSingle$WQS_ID) %>%
  left_join(dplyr::select( snapSingle, StationID, `Buffer Distance`, n, WQS_ID) %>% st_drop_geometry(), by = 'WQS_ID') %>%
  dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything())
if(WQSwaterbodyType1 == 'Estuarine'){
  snapSingle_sf_EL <- filter(WQSsEL1, WQS_ID %in% snapSingle$WQS_ID) %>% # bonus polyline feature for Estuarine
    left_join(dplyr::select( snapSingle, StationID, `Buffer Distance`, n, WQS_ID) %>% st_drop_geometry(), by = 'WQS_ID') %>%
    dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything())    
} else {snapSingle_sf_EL <- WQSs1[0,]  %>% # bonus polyline feature for Estuarine
  left_join(dplyr::select(snapSingle, StationID, `Buffer Distance`, n, WQS_ID) %>% st_drop_geometry(), by = 'WQS_ID') %>%
  dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything()) } # create dummy variable
# Make dataset of sites associated with no segments IN REGION
snapNone <- filter(sitesUnique,  `Buffer Distance` == 'No connections within 80 m') %>% #is.na(WQS_ID)) %>%
  filter(StationID %in% snap_input_Region$StationID) %>% # limit assignment to just what falls in a region
  left_join(WQSs1 %>% st_drop_geometry(), by = 'WQS_ID')
# Make empty dataset of sites that assessors touched
sitesAdjusted <-  sitesUnique[0,]  %>%
  dplyr::select(StationID, WQS_ID, `Buffer Distance`) %>%
  mutate(Comments = as.character())
# Make dataset for user to download
finalWQS <- WQSlookup


namesToSmash <- c('7CGMA001.25', '7CGMA001.25')

filter(WQSs1, WQS_ID %in% filter(snap_input, StationID %in% namesToSmash)$WQS_ID) %>%
  {if(WQSwaterbodyType1 == 'Estuarine')
    rbind(.,
          filter(WQSsEL1, WQS_ID %in%
                   filter(snap_input, StationID %in% namesToSmash)$WQS_ID) ) 
    else . } %>%
  st_drop_geometry() %>%
  dplyr::select(WQS_ID, everything())# %>%
 # datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '200px'))  






# Data breakdown

snap_input_OG <- readRDS('data/WQStable02032021.RDS') %>% # February 2021 update prior to official 2022 IR
  distinct(StationID, .keep_all = T) %>%
  left_join(conventionals_DWQS, by = 'StationID') %>%
  # drop missing lat lng sites to make spatial conversion work
  filter(!is.na(Latitude) | !is.na(Longitude)) %>% # must double check all these sites that they aren't acutally in conventionalsRaw, 
  #which I did and everything is cool to drop these sites bc they were not sampled in Roger's 2022 IR data pull
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = T, # don't remove these lat/lon cols from df
           crs = 4326) %>%
  st_intersection(assessmentRegions) %>%
  st_drop_geometry() %>% # back to tibble
  rename('Buffer Distance' = 'Buffer.Distance') %>%
  # Add back any missing sites that are dropped because they don't fall into assessment region boundaries
  bind_rows(
    filter(snap_input, StationID %in% 
             readRDS('data/missingSites.RDS')$FDT_STA_ID) %>%
      left_join(conventionals_DWQS, by = 'StationID') %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`) ) %>%
  distinct(StationID, .keep_all = T) %>%
  
  mutate(Type = str_extract(WQS_ID, "^.{2}"),
         basin = gsub("_","",str_extract(WQS_ID, ".{3}_")) ) %>%
  mutate(`Assessment Type` = case_when(Type %in% c('EL','EP') ~ 'Estuarine', 
                                       Type == 'RL' ~ 'Riverine', 
                                       Type == 'LP' ~ 'Lacustrine', 
                                       is.na(Type) ~ 'Not sure yet',
                                       TRUE ~ as.character(Type))) %>%
  dplyr::select(StationID, WQS_ID, `Buffer Distance`, `Assessment Type`, ASSESS_REG, basin) 
  

nrow(filter(snap_input_OG, str_detect(WQS_ID, '_NA'))) # 41 sites with no WQS_ID suggestions


snap_input_OG %>% group_by(ASSESS_REG) %>% summarise(`Total Sites for Review` = n()) 
snap_input_OG %>% group_by(ASSESS_REG, basin) %>% summarise(`Sites for Review` = n()) 
View(
  snap_input_OG %>% group_by(ASSESS_REG, basin, `Assessment Type`) %>% summarise(`Sites for Review` = n())  %>%
    left_join(snap_input_OG %>% group_by(ASSESS_REG) %>% summarise(`Total Sites for Review` = n()),
              by = 'ASSESS_REG'))
write.csv(snap_input_OG %>% group_by(ASSESS_REG, basin, `Assessment Type`) %>% summarise(`Sites for Review` = n())  %>%
            left_join(snap_input_OG %>% group_by(ASSESS_REG) %>% summarise(`Total Sites for Review` = n()),
                      by = 'ASSESS_REG'),
          'WQSbreakdown.csv', row.names = F)
