# app testing data
source('global.R')

DEQregionSelection <- 'BRRO'
basinSelection <- "James-Middle"
HUC6Selection <- 'JM02'


conventionals <- pin_get("conventionals2022IRdraft", board = "rsconnect") %>%
  filter(FDT_DATE_TIME >= "2015-01-01 00:00:00 UTC" )
vahu6 <- st_as_sf(pin_get("vahu6", board = "rsconnect")) # bring in as sf object
WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")


stationTable <- read_csv('userDataToUpload/processedStationData/stationTableResults.csv')
# Remove stations that don't apply to application
lakeStations <- filter_at(stationTable, vars(starts_with('TYPE')), any_vars(. == 'L'))
stationTable <- filter(stationTable, !STATION_ID %in% lakeStations$STATION_ID) %>%
  # add WQS information to stations
  left_join(WQSlookup, by = c('STATION_ID'='StationID')) %>%
  mutate(CLASS_BASIN = paste(CLASS,substr(BASIN, 1,1), sep="_")) %>%
  mutate(CLASS_BASIN = ifelse(CLASS_BASIN == 'II_7', "II_7", as.character(CLASS))) %>%
  # Fix for Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard)
  left_join(WQSvalues, by = 'CLASS_BASIN') %>%
  dplyr::select(-c(CLASS.y,CLASS_BASIN)) %>%
  rename('CLASS' = 'CLASS.x') 
# last cycle had code to fix Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard) but not sure if necessary



regionalAUs <- st_zm(st_as_sf(pin_get(paste0(DEQregionSelection, 'workingAUriverine'), board = 'rsconnect'))) 
regionalAUsForTesting <- regionalAUs






the_data <- filter(vahu6, ASSESS_REG %in% DEQregionSelection) %>%
    left_join(dplyr::select(subbasinToVAHU6, VAHU6, Basin, BASIN_CODE, Basin_Code))
basin_filter <- filter(the_data, Basin_Code %in% basinSelection)
huc6_filter <- filter(basin_filter, VAHU6 %in% HUC6Selection)

AUs <- suppressWarnings(st_intersection(regionalAUs,  huc6_filter)) #filter(vahu6, VAHU6 %in% huc6_filter()$VAHU6)))})

stationSummary <- filter(conventionals, Huc6_Vahu6 %in% huc6_filter$VAHU6) %>%
  distinct(FDT_STA_ID, .keep_all = TRUE) %>%
  select(FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:STA_CBP_NAME) %>% 
  mutate(#`In Stations Table` = ifelse(FDT_STA_ID %in% unique(stationTable$STATION_ID), 'yes','no'),
         #`In Selected Region` = ifelse(FDT_STA_ID %in% filter(stationTable, REGION %in% DEQregionSelection)$STATION_ID, 'yes','no'),
    `Analyzed By App` = #ifelse(`In Stations Table` == 'yes'# && `In Selected Region` == 'yes', 'yes','no'))
      ifelse(FDT_STA_ID %in% unique(stationTable$STATION_ID), 'yes','no'))




## Assessment Unit Review Tab

conventionals_HUC <- filter(conventionals, Huc6_Vahu6 %in% huc6_filter$VAHU6) %>%
    left_join(dplyr::select(stationTable, STATION_ID:VAHU6,
                            WQS_ID:CLASS_DESCRIPTION),
                            #WQS_ID:`Max Temperature (C)`), 
              by = c('FDT_STA_ID' = 'STATION_ID')) %>%
    filter(!is.na(ID305B_1))

AUselection <- unique(conventionals_HUC$ID305B_1)[1]

#selectedAU <-  filter(regionalAUs, ID305B %in% AUselection) %>% st_set_geometry(NULL) %>% as.data.frame()
stationSelection <- filter(conventionals_HUC, ID305B_1 %in% AUselection | ID305B_2 %in% AUselection | 
                ID305B_3 %in% AUselection | ID305B_4 %in% AUselection | ID305B_5 %in% AUselection | 
                ID305B_6 %in% AUselection | ID305B_7 %in% AUselection | ID305B_8 %in% AUselection | 
                ID305B_9 %in% AUselection | ID305B_10 %in% AUselection) %>%
    distinct(FDT_STA_ID) %>%
  pull()
stationSelection <- stationSelection[1]

AUData <- filter_at(conventionals_HUC, vars(starts_with("ID305B")), any_vars(. %in% AUselection) )

stationData <- filter(AUData, FDT_STA_ID %in% stationSelection)

stationInfo <- filter(stationTable, STATION_ID == stationSelection) %>% 
    select(STATION_ID:VAHU6, WQS_ID:Trout) %>% 
    t() %>% as.data.frame() %>% rename(`Station and WQS Information` = 'V1')


#stationMap
point <- dplyr::select(stationData[1,],  FDT_STA_ID, starts_with('ID305B'), Latitude, Longitude ) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), 
             remove = F, # don't remove these lat/lon cols from df
             crs = 4269) # add projection, needs to be geographic for now bc entering lat/lng
segmentChoices <- dplyr::select(point, starts_with('ID305B')) %>% st_drop_geometry() %>% as.character()  
segment <- filter(regionalAUs, ID305B %in% segmentChoices)
map1 <- mapview(segment,zcol = 'ID305B', label= segment$ID305B, layer.name = 'Assessment Unit (ID305B_1)',
                popup= leafpop::popupTable(segment, zcol=c("ID305B","MILES","CYCLE","WATER_NAME")), legend= FALSE) + 
  mapview(point, color = 'yellow', lwd = 5, label= point$FDT_STA_ID, layer.name = c('Selected Station'),
          popup=NULL, legend= FALSE)
map1@map %>% setView(point$Longitude, point$Latitude, zoom = 12)

  
  
  
# Station Table Output
StationTableOutput <- cbind(StationTableStartingData(stationData),
                            tempExceedances(stationData) %>% quickStats('TEMP'),
                            DOExceedances_Min(stationData) %>% quickStats('DO'), 
                            pHExceedances(stationData) %>% quickStats('PH'),
                            bacteriaAssessmentDecision(stationData, 'E.COLI', 'ECOLI_RMK', 10, 410, 126) %>%
                              dplyr::select(ECOLI_EXC:ECOLI_STAT),
                            bacteriaAssessmentDecision(stationData, 'ENTEROCOCCI', 'RMK_31649', 10, 130, 35) %>%
                              dplyr::select(ENTER_EXC:ENTER_STAT)
                            ) %>%
  mutate(COMMENTS = NA) %>%
  dplyr::select(-ends_with('exceedanceRate')) %>% # to match Bulk Upload template but helpful to keep visible til now for testing
  dplyr::select(STATION_ID:COMMENTS) # for now bc bacteria needs help still



# Temperature WQS change math
#x <- stationData
#temperature_changeWQS <- WQSvalues$CLASS_DESCRIPTION[7]

#if(temperature_changeWQS != unique(x$CLASS_DESCRIPTION)){
#    changedWQS <- filter(WQSvalues, CLASS_DESCRIPTION %in% temperature_changeWQS)
#    temperature_oneStation <- dplyr::select(x, -c(`Description Of Waters`:CLASS_DESCRIPTION)) %>%
#        mutate(CLASS = changedWQS$CLASS, 
#               `Description Of Waters` = changedWQS$`Description Of Waters` ) %>%
#        left_join(changedWQS, by = c('CLASS', 'Description Of Waters')) 
#} else {temperature_oneStation <- x} 

#tempExceedances(temperature_oneStation) %>%
#  rename("FDT_TEMP" = 'parameter', 'Criteria' = 'limit', 'Parameter Rounded to WQS Format' = 'parameterRound') %>%
#  filter(exceeds == TRUE) %>%
#  dplyr::select(-exceeds)
#z <- tempExceedances(temperature_oneStation) %>% quickStats('TEMP') %>% dplyr::select(-TEMP_STAT)
#datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "300px", dom='t')) 
