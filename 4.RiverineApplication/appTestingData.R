# app testing data
source('global.R')

DEQregionSelection <- 'BRRO'
basinSelection <- "James-Middle"#'James-Upper'#"Roanoke"#"Roanoke"#'James-Upper'#
HUC6Selection <- "JM01"#'JM16'#'RU09'#'RL12'#


conventionals <- pin_get("conventionals2022IRdraft", board = "rsconnect") %>%
  filter(FDT_DATE_TIME >= "2015-01-01 00:00:00 EDT" )
vahu6 <- st_as_sf(pin_get("vahu6", board = "rsconnect")) # bring in as sf object
WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")
# placeholder for now, shouldn't be a spatial file
historicalStationsTable <- st_read('data/GIS/2020_wqms.shp') %>%
  st_drop_geometry()#read_csv('data/stationsTable2022begin.csv') # last cycle stations table (forced into new station table format)
WCmetals <- pin_get("WCmetals-2020IRfinal",  board = "rsconnect")
Smetals <- pin_get("Smetals-2020IRfinal",  board = "rsconnect")
WQMstationFull <- pin_get("WQM-Station-Full", board = "rsconnect")
VSCIresults <- pin_get("VSCIresults", board = "rsconnect") %>%
  filter( between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2]) )# %>% # get ecoregion info
  #left_join(dplyr::select(WQMstationFull, WQM_STA_ID, EPA_ECO_US_L3CODE, EPA_ECO_US_L3NAME, WQS_BASIN_CODE) %>%
  #            distinct(WQM_STA_ID, .keep_all = TRUE), by = c('StationID' = 'WQM_STA_ID'))
VCPMI63results <- pin_get("VCPMI63results", board = "rsconnect") %>%
  filter( between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2]) ) #%>% # get ecoregion info
  #left_join(dplyr::select(WQMstationFull, WQM_STA_ID, EPA_ECO_US_L3CODE, EPA_ECO_US_L3NAME, WQS_BASIN_CODE) %>%
   #           distinct(WQM_STA_ID, .keep_all = TRUE), by = c('StationID' = 'WQM_STA_ID'))
VCPMI65results <- pin_get("VCPMI65results", board = "rsconnect") %>%
  filter( between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2]) ) #%>% # get ecoregion info
  #left_join(dplyr::select(WQMstationFull, WQM_STA_ID, EPA_ECO_US_L3CODE, EPA_ECO_US_L3NAME, WQS_BASIN_CODE) %>%
  #            distinct(WQM_STA_ID, .keep_all = TRUE), by = c('StationID' = 'WQM_STA_ID'))
ammoniaAnalysis <- readRDS('userDataToUpload/processedStationData/ammoniaAnalysis.RDS')






stationTable <- read_csv('userDataToUpload/processedStationData/stationTableResults.csv',
                         col_types = cols(COMMENTS = col_character())) # force to character bc parsing can incorrectly guess logical based on top 1000 rows
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
  rename('CLASS' = 'CLASS.x') %>%
  left_join(dplyr::select(WQMstationFull, WQM_STA_ID, EPA_ECO_US_L3CODE, EPA_ECO_US_L3NAME) %>%
              distinct(WQM_STA_ID, .keep_all = TRUE), by = c('STATION_ID' = 'WQM_STA_ID'))
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
  dplyr::select(FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:Data_Source, Latitude, Longitude) %>% 
  mutate(#`In Stations Table` = ifelse(FDT_STA_ID %in% unique(stationTable$STATION_ID), 'yes','no'),
         #`In Selected Region` = ifelse(FDT_STA_ID %in% filter(stationTable, REGION %in% DEQregionSelection)$STATION_ID, 'yes','no'),
    `Analyzed By App` = #ifelse(`In Stations Table` == 'yes'# && `In Selected Region` == 'yes', 'yes','no'))
      ifelse(FDT_STA_ID %in% unique(stationTable$STATION_ID), 'yes','no'))


# Stations carried over
carryoverStations <- filter(stationTable, VAHU6 %in% huc6_filter$VAHU6 & str_detect(COMMENTS, "This station has no data"))  




## Assessment Unit Review Tab

conventionals_HUC <- filter(conventionals, Huc6_Vahu6 %in% huc6_filter$VAHU6) %>%
    left_join(dplyr::select(stationTable, STATION_ID:VAHU6,
                            WQS_ID:EPA_ECO_US_L3NAME),
                            #WQS_ID:`Max Temperature (C)`), 
              by = c('FDT_STA_ID' = 'STATION_ID')) %>%
    filter(!is.na(ID305B_1)) %>%
  pHSpecialStandardsCorrection()

#AUselection <- unique(c(conventionals_HUC$ID305B_1, 
#                        dplyr::select(carryoverStations, ID305B_1:ID305B_10) %>% as.character()))

AUselection <- unique(c(conventionals_HUC$ID305B_1,
                        dplyr::select(carryoverStations, ID305B_1:ID305B_10) %>% 
                          mutate_at(vars(starts_with("ID305B")), as.character) %>%
                          pivot_longer(ID305B_1:ID305B_10, names_to = 'ID305B', values_to = 'keep') %>%
                          pull(keep) ))
AUselection <- AUselection[!is.na(AUselection) & !(AUselection %in% c("NA", "character(0)", "logical(0)"))][2]



#selectedAU <-  filter(regionalAUs, ID305B %in% AUselection) %>% st_set_geometry(NULL) %>% as.data.frame()
stationSelection <- filter(conventionals_HUC, ID305B_1 %in% AUselection | ID305B_2 %in% AUselection | 
                ID305B_3 %in% AUselection | ID305B_4 %in% AUselection | ID305B_5 %in% AUselection | 
                ID305B_6 %in% AUselection | ID305B_7 %in% AUselection | ID305B_8 %in% AUselection | 
                ID305B_9 %in% AUselection | ID305B_10 %in% AUselection) %>%
    distinct(FDT_STA_ID) %>%
  pull()
# add in carryover stations
if(nrow(carryoverStations) > 0){
  carryoverStationsInAU <- filter(carryoverStations,  ID305B_1 %in% AUselection | ID305B_2 %in% AUselection | 
                                    ID305B_3 %in% AUselection | ID305B_4 %in% AUselection | ID305B_5 %in% AUselection | 
                                    ID305B_6 %in% AUselection | ID305B_7 %in% AUselection | ID305B_8 %in% AUselection | 
                                    ID305B_9 %in% AUselection | ID305B_10 %in% AUselection) %>%
    distinct(STATION_ID) %>%
    pull()
  if(length(carryoverStationsInAU) > 0){
    stationSelection <- c(stationSelection, carryoverStationsInAU)  } }
stationSelection <- stationSelection[1]

AUData <- filter_at(conventionals_HUC, vars(starts_with("ID305B")), any_vars(. %in% AUselection) ) 

stationData <- filter(AUData, FDT_STA_ID %in% stationSelection)

stationInfo <- filter(stationTable, STATION_ID == stationSelection) %>% 
    select(STATION_ID:VAHU6, WQS_ID:Trout) %>% 
    t() %>% as.data.frame() %>% rename(`Station and WQS Information` = 'V1')




# AU preview map
#stations <- filter(stationTable, VAHU6 %in% huc6_filter$VAHU6) %>%
#  dplyr::select(STATION_ID, LATITUDE, LONGITUDE) %>%
#  left_join(dplyr::select(stationSummary, STATION_ID = FDT_STA_ID, `Analyzed By App`), by = "STATION_ID") %>%
#  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
#           remove = F, # don't remove these lat/lon cols from df
#           crs = 4269) # add projection, needs to be geographic for now bc entering lat/lng

#stations <- dplyr::select(stationSummary, STATION_ID = FDT_STA_ID, LATITUDE = Latitude, LONGITUDE = Longitude, `Analyzed By App`) %>%
#  bind_rows(filter(stationTable, VAHU6 %in% huc6_filter$VAHU6 & !STATION_ID %in% stationSummary$FDT_STA_ID) %>%
#              dplyr::select(STATION_ID, LATITUDE, LONGITUDE) %>%
#              mutate(`Analyzed By App` = 'IM carryover with no data in window')) %>%
#  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
#           remove = F, # don't remove these lat/lon cols from df
#           crs = 4269) # add projection, needs to be geographic for now bc entering lat/lng

#z <- AUs
#z$ID305B <- factor(z$ID305B) # drop extra factor levels so colors come out right

#m <- mapview(huc6_filter, color = 'yellow',lwd= 5, label= NULL, layer.name = c('Selected HUC6'),
#             popup= leafpop::popupTable(huc6_filter, zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG")), legend= FALSE) + 
#  mapview(z, label= z$ID305B, layer.name = c('AUs in Selected HUC6'), zcol = "ID305B", 
#          popup= leafpop::popupTable(z, zcol=c("ID305B","MILES","CYCLE","WATER_NAME","LOCATION" )), legend= FALSE) +
#  mapview(stations, label = stations$STATION_ID, layer.name = c('Stations in Selected HUC6'), zcol = "Analyzed By App", 
#          popup= leafpop::popupTable(stations, zcol=c("STATION_ID", "Analyzed By App")), legend= FALSE) 
#m@map 


#stationMap
#point <- dplyr::select(stationData[1,],  FDT_STA_ID, starts_with('ID305B'), Latitude, Longitude ) %>%
#    st_as_sf(coords = c("Longitude", "Latitude"), 
#             remove = F, # don't remove these lat/lon cols from df
#             crs = 4269) # add projection, needs to be geographic for now bc entering lat/lng
#point <- dplyr::select(stationInfo,  STATION_ID, starts_with('ID305B'), LATITUDE, LONGITUDE ) %>%
#  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
#           remove = F, # don't remove these lat/lon cols from df
#           crs = 4269) # add projection, needs to be geographic for now bc entering lat/lng
#segmentChoices <- dplyr::select(point, starts_with('ID305B')) %>% st_drop_geometry() %>% as.character()  
#segment <- filter(regionalAUs, ID305B %in% segmentChoices)
#map1 <- mapview(segment,zcol = 'ID305B', label= segment$ID305B, layer.name = 'Assessment Unit (ID305B_1)',
#                popup= leafpop::popupTable(segment, zcol=c("ID305B","MILES","CYCLE","WATER_NAME")), legend= FALSE) + 
#  mapview(point, color = 'yellow', lwd = 5, label= point$STATION_ID, layer.name = c('Selected Station'),
#  #mapview(point, color = 'yellow', lwd = 5, label= point$FDT_STA_ID, layer.name = c('Selected Station'),
#          popup=NULL, legend= FALSE)
#map1@map %>% setView(point$LONGITUDE, point$LATITUDE, zoom = 12)
#map1@map %>% setView(point$Longitude, point$Latitude, zoom = 12)

  
  
  
# Station Table Output
#run longer analyses first
#ecoli1 <- bacteriaAssessmentDecision(stationData, 'ECOLI', 'RMK_ECOLI', 10, 410, 126)
#enter1 <- bacteriaAssessmentDecision(stationData, 'ENTEROCOCCI', 'RMK_ENTEROCOCCI', 10, 130, 35)

#stationTableOutput <- cbind(StationTableStartingData(stationData()),
#                                     tempExceedances(stationData()) %>% quickStats('TEMP'),
#                                     DOExceedances_Min(stationData()) %>% quickStats('DO'), 
#                                     pHExceedances(stationData()) %>% quickStats('PH'),
#                                     ecoli %>% dplyr::select(ECOLI_EXC:ECOLI_STAT),
#                                     enter %>% dplyr::select(ENTER_EXC:ENTER_STAT)) %>%
#  mutate(COMMENTS = NA) %>%
#  dplyr::select(-ends_with('exceedanceRate')) %>% # to match Bulk Upload template but helpful to keep visible til now for testing
#  dplyr::select(STATION_ID:COMMENTS) # for now bc bacteria needs help still



# Temperature WQS change math

#temperature_oneStation <- changeWQSfunction(stationData, WQSvalues$CLASS_DESCRIPTION[7])

#tempExceedances(temperature_oneStation) %>%
#  rename("FDT_TEMP" = 'parameter', 'Criteria' = 'limit', 'Parameter Rounded to WQS Format' = 'parameterRound') %>%
#  filter(exceeds == TRUE) %>%
#  dplyr::select(-exceeds)
#z <- tempExceedances(temperature_oneStation) %>% quickStats('TEMP') %>% dplyr::select(-TEMP_STAT)
#datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "300px", dom='t')) 



# pH WQS change math with special standards correction
#oneStation_original <- stationData %>%
#  mutate(CLASS_DESCRIPTION = case_when(str_detect(as.character(SPSTDS), '6.5-9.5') ~ 'SPSTDS = 6.5-9.5',
#                                       TRUE ~ CLASS_DESCRIPTION))
#choices <- c(WQSvalues$CLASS_DESCRIPTION, 'SPSTDS = 6.5-9.5')
#oneStation <- changeWQSfunction(oneStation_original, choices[9])


# Ecoli build
#windowChoice_ <- unique(enter1[['associatedDecisionData']][[1]]$`Date Window Starts`)[1]
oneStationDecisionData <- ecoli1[['associatedDecisionData']][[1]]
z <- dplyr::select(stationData, FDT_STA_ID, FDT_DATE_TIME, ECOLI, RMK_ECOLI)%>% 
  mutate(FDT_DATE_TIME = as.Date(FDT_DATE_TIME, format = '%Y-%m-%D %H:%M:S'))

windowData <- filter(oneStationDecisionData, as.character(`Date Window Starts`) %in% as.character(z$FDT_DATE_TIME[2])) %>%#input$rawData_rows_selected]) %>%
  dplyr::select( associatedData) %>%
  unnest(cols = c(associatedData)) %>%
  mutate(newSTV = 410, geomeanLimit = 126,
         `Date Time` = as.POSIXct(strptime(FDT_DATE_TIME, format="%Y-%m-%d")))#as.POSIXct(windowData$`Date Time`, format="%Y-%m-%d", tz='GMT') + as.difftime(1, units="days")


z <- oneStationDecisionData %>%
  dplyr::select(-associatedData) 


# Fix look of single measure
if(nrow(windowData) == 1){
  windowData <- bind_rows(windowData,
                          tibble(`Date Time` = c(windowData$`Date Time`- days(5), windowData$`Date Time` + days(5))))
}


plot_ly(data=windowData) %>%
  add_markers(x= ~`Date Time`, y= ~Value,mode = 'scatter', name="E. coli (CFU / 100 mL)", marker = list(color= '#535559'),
              hoverinfo="text",text=~paste(sep="<br>",
                                           paste("Date: ",`Date Time`),
                                           paste("E. coli: ",Value,"CFU / 100 mL"))) %>%
  add_lines(data=windowData, x=~`Date Time`, y=~geomean, mode='line', line = list(color = 'orange', dash= 'dash'),
            hoverinfo = "text", text= ~paste("Window Geomean: ", format(geomean,digits=3)," CFU / 100 mL", sep=''), 
            name="Window Geomean") %>%
  add_lines(data=windowData, x=~`Date Time`,y=~newSTV, mode='line', line = list(color = '#484a4c',dash = 'dot'),
            hoverinfo = "text", text= "New STV: 410 CFU / 100 mL", name="New STV: 410 CFU / 100 mL") %>%
  add_lines(data=windowData, x=~`Date Time`,y=~geomeanLimit, mode='line', line = list(color = 'black', dash= 'dash'),
            hoverinfo = "text", text= "Geomean Criteria: 126 CFU / 100 mL", name="Geomean Criteria: 126 CFU / 100 mL") %>%
  layout(showlegend=FALSE,
         yaxis=list(title="E. coli (CFU / 100 mL)"),
         xaxis=list(title="Sample Date",tickfont = list(size = 10))) 


#windowData <- filter(enter1[['associatedDecisionData']][[1]], `Date Window Starts` %in% windowChoice_) %>%
#  dplyr::select( associatedData) %>%
#  unnest(cols = c(associatedData)) %>%
#  mutate(#`Date Window Starts` = as.POSIXct(unique(windowStart$`Date Window Starts`, format="%m/%d/%y")),
#         #`Date Window Ends` = as.POSIXct(unique(windowStart$`Date Window Ends`, format="%m/%d/%y")),
#    newSTV = 130, geomean = 35,     
#    #newSTV = 410, geomean = 126,
#         `Date Time` = as.POSIXct(strptime(FDT_DATE_TIME, format="%Y-%m-%d")))#as.POSIXct(windowData$`Date Time`, format="%Y-%m-%d", tz='GMT') + as.difftime(1, units="days")

#plot_ly(data=windowData) %>%
#  add_markers(x= ~`Date Time`, y= ~Value,mode = 'scatter', name="E. coli (CFU / 100 mL)", marker = list(color= '#535559'),
#              hoverinfo="text",text=~paste(sep="<br>",
#                                           paste("Date: ",`Date Time`),
#                                           paste("E. coli: ",Value,"CFU / 100 mL"))) %>%
#  add_lines(data=windowData, x=~`Date Time`, y=~geomean, mode='line', line = list(color = 'orange', dash= 'dash'),
#            hoverinfo = "text", text= ~paste("Window Geomean: ", format(geomean,digits=3)," CFU / 100 mL", sep=''), 
#            name="Window Geomean") %>%
#  add_lines(data=windowData, x=~`Date Time`,y=~newSTV, mode='line', line = list(color = '#484a4c',dash = 'dot'),
#            hoverinfo = "text", text= "New STV: 410 CFU / 100 mL", name="New STV: 410 CFU / 100 mL") %>%
#  add_lines(data=windowData, x=~`Date Time`,y=~geomean, mode='line', line = list(color = 'black', dash= 'dash'),
#            hoverinfo = "text", text= "Geomean: 126 CFU / 100 mL", name="Geomean: 126 CFU / 100 mL") %>%
#  layout(showlegend=FALSE,
#         yaxis=list(title="E. coli (CFU / 100 mL)"),
#         xaxis=list(title="Sample Date",tickfont = list(size = 10))) 


## Old bacteria 

#bacteria_ExceedancesSTV_OLD(stationData %>%
#                              dplyr::select(FDT_DATE_TIME,E.COLI)%>% # Just get relevant columns, 
#                              filter(!is.na(E.COLI)) #get rid of NA's
#                            , 235 ) %>%
#  filter(exceeds == T) %>%
#  mutate(FDT_DATE_TIME = as.Date(FDT_DATE_TIME), E.COLI = parameter) %>%
#  dplyr::select(FDT_DATE_TIME, E.COLI, limit, exceeds)

#bacteria_ExceedancesGeomeanOLD(stationData %>% 
#                                 dplyr::select(FDT_DATE_TIME,E.COLI)%>% # Just get relevant columns, 
#                                 filter(!is.na(E.COLI)), #get rid of NA's
#                               'E.COLI', 126) %>%
#  dplyr::select(FDT_DATE_TIME, E.COLI, sampleMonthYear, geoMeanCalendarMonth, limit, samplesPerMonth) %>%
#  filter(samplesPerMonth > 4, geoMeanCalendarMonth > limit) # minimum sampling rule for geomean to apply


#bacteria_ExceedancesSTV_OLD(stationData %>%
#                                   dplyr::select(FDT_DATE_TIME,ENTEROCOCCI)%>% # Just get relevant columns, 
#                                   filter(!is.na(ENTEROCOCCI)) #get rid of NA's
#                                 , 130 ) %>%
#  filter(exceeds == T) %>%
#  mutate(FDT_DATE_TIME = as.Date(FDT_DATE_TIME), ENTEROCOCCI = parameter) %>%
#  dplyr::select(FDT_DATE_TIME, ENTEROCOCCI, limit, exceeds)


#bacteria_ExceedancesGeomeanOLD(stationData %>% 
#                                      dplyr::select(FDT_DATE_TIME,ENTEROCOCCI)%>% # Just get relavent columns, 
#                                      filter(!is.na(ENTEROCOCCI)), #get rid of NA's
#                                    'ENTEROCOCCI', 35) %>%
#  dplyr::select(FDT_DATE_TIME, ENTEROCOCCI, sampleMonthYear, geoMeanCalendarMonth, limit, samplesPerMonth) %>%
#  filter(samplesPerMonth > 4, geoMeanCalendarMonth > limit) # minimum sampling rule for geomean to apply


# PWS data stuff
#oneStation <- filter(conventionals, Huc6_Vahu6 %in% c('JM01','JM02', 'JM03', 'JM04', 'JM05', 'JM06')) %>%
#  left_join(dplyr::select(stationTable, STATION_ID:VAHU6,
#                          WQS_ID:CLASS_DESCRIPTION),
#            #WQS_ID:`Max Temperature (C)`), 
#            by = c('FDT_STA_ID' = 'STATION_ID')) %>%
#  filter(!is.na(ID305B_1)) %>%
#  pHSpecialStandardsCorrection() %>%
#  filter(!is.na(CHLORIDE)) %>% #NITRATE)) %>%
#  filter(FDT_STA_ID == '2-POL020.03')#'2-RED003.65')#
#  mutate(PWSlimit = 250)
#defaultPWS <- unique(oneStation$PWS) %in% c("Yes")



#dat <- oneStation %>% mutate(PWSlimit = 250)
#dat$SampleDate <-  as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y")
#         
#if(nrow(dat) == 1){
#  dat <- bind_rows(dat,
#    tibble(SampleDate = c(dat$SampleDate- days(5), dat$SampleDate + days(5)),
#           PWSlimit = c(250, 250)))
#}

#box1 <- data.frame(SampleDate = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(50, maxheight, maxheight, 50))
#box2 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(25, 50, 50, 25))
#box3 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(10, 25, 25, 10))
#box4 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0, 10, 10, 0))

#plot_ly(data=dat)%>%
#  add_polygons(x = ~SampleDate, y = ~y, data = box1, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
#               hoverinfo="text", name =paste('High Probability of Stress to Aquatic Life')) %>%
#  add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
#               hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
#  add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
#               hoverinfo="text", name =paste('Low Probability of Stress to Aquatic Life')) %>%
#  add_polygons(data = box4, x = ~x, y = ~y, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
#               hoverinfo="text", name =paste('No Probability of Stress to Aquatic Life')) %>%
#  add_lines(data=dat, x=~SampleDate,y=~PWSlimit, mode='line', line = list(color = 'black'),
#            hoverinfo = "text", text= "PWS Criteria (250 mg/L)", name="PWS Criteria (250 mg/L)") %>%
#  add_markers(data=dat, x= ~SampleDate, y= ~CHLORIDE,mode = 'scatter', name="Dissolved Chloride (mg/L)",marker = list(color= '#535559'),
#              hoverinfo="text",text=~paste(sep="<br>",
#                                           paste("Date: ",SampleDate),
#                                           paste("Depth: ",FDT_DEPTH, "m"),
#                                           paste("Dissolved Chloride: ",CHLORIDE,"mg/L")))%>%
#  layout(showlegend=FALSE,
#         yaxis=list(title="Dissolved Chloride (mg/L)"),
#         xaxis=list(title="Sample Date",tickfont = list(size = 10)))













# PWS table
#if(is.na(unique(stationData$PWS))){
#  PWSconcat <- tibble(STATION_ID = unique(stationData$FDT_STA_ID),
#                          PWS= 'PWS Standards Do Not Apply To Station')
#  DT::datatable(PWSconcat, escape=F, rownames = F, options= list(scrollX = TRUE, pageLength = nrow(PWSconcat), dom='t'))
#  
#} else {
#  PWSconcat <- cbind(tibble(STATION_ID = unique(stationData$FDT_STA_ID)),
#                     assessPWS(stationData, NITRATE, RMK_NITRATE, 10, 'PWS_Nitrate'),
#                     assessPWS(stationData, CHLORIDE, RMK_CHLORIDE, 250, 'PWS_Chloride'),
#                     assessPWS(stationData, SULFATE_TOTAL, RMK_SULFATE_TOTAL, 250, 'PWS_Total_Sulfate')) %>%
#    dplyr::select(-ends_with('exceedanceRate')) }
#DT::datatable(PWSconcat, escape=F, rownames = F, options= list(scrollX = TRUE, pageLength = nrow(PWSconcat), dom='t')) %>% 
#  formatStyle(c("PWS_Nitrate_EXC","PWS_Nitrate_SAMP","PWS_Nitrate_STAT"), "PWS_Nitrate_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>%
#  formatStyle(c("PWS_Chloride_EXC","PWS_Chloride_SAMP","PWS_Chloride_STAT"), "PWS_Chloride_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>%
#  formatStyle(c("PWS_Total_Sulfate_EXC","PWS_Total_Sulfate_SAMP","PWS_Total_Sulfate_STAT"), "PWS_Total_Sulfate_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) 















## Ammonia Calculations
#stationData <- filter(conventionals, FDT_STA_ID %in% '2-JMS279.41') %>% # 2BJMS279.41 good cbay station example with lots of data
#  #filter(conventionals, FDT_STA_ID %in% '4ABSA000.62') %>% # good example with lots of data, lake station so depth is important and hourly averages
#  left_join(dplyr::select(stationTable, STATION_ID:VAHU6,
#                          WQS_ID:EPA_ECO_US_L3NAME),
#            #WQS_ID:`Max Temperature (C)`), 
#            by = c('FDT_STA_ID' = 'STATION_ID')) %>%
#  #filter(!is.na(ID305B_1)) %>% # 4ABSA000.62 doesn't have an AU?????
#  pHSpecialStandardsCorrection()

#x <- freshwaterNH3limit(stationData, trout = TRUE, mussels = TRUE, earlyLife = TRUE)  
##x$acuteExceedance[c(2, 40)] <- c(TRUE, TRUE)
#x$chronicExceedance[c(4)] <- c(TRUE)
#x$fourDayExceedance[c(2)] <- c(TRUE)



# Acute criteria are a one-hour average concentration not to be exceeded more than once every three years on the average

## This function organizes results from freshwaterNH3limit() into a single assessment decision
#freshwaterNH3Assessment <- function(x, # x is station run through freshwaterNH3limit(), which handles citmon/nonagency data appropriately
#                                    assessmentType){ # c('acute', 'chronic', 'four-day') one of these options to change criteria tested and window length
#  
#  
#  # Adjust presets based on assessmentType
#  if(assessmentType == 'acute'){limitColumn <- quo(acuteExceedance)}
#  if(assessmentType == 'chronic'){limitColumn <- quo(chronicExceedance)} ###
#  if(assessmentType == 'four-day'){limitColumn <- quo(fourDayExceedance)} ####
#  
#  # Identify any exceedances
#  exceedances <- filter(x, !! limitColumn) # find any exceedances                       
#  
#  if(nrow(exceedances) > 0){
#    
#    # Test if > 1 exceedance in any 3 year window, start each window with sampling event
#    # Loop through each row of exceedance df to test if any other exceedance in a 3 year window
#    exceedancesIn3YrWindow <- tibble()
#    for( i in 1 : nrow(exceedances)){
#      windowBegin <- exceedances$FDT_DATE_TIME[i]
#      windowEnd <- exceedances$FDT_DATE_TIME[i] + years(3)
#      
#      # Find exceeding data in window defined above
#      exceedancesIn3YrWindowData <- filter(exceedances, between(FDT_DATE_TIME, windowBegin, windowEnd) ) %>% 
#        ungroup()
#      
#      
#    exceedancesIn3YrWindowi <- tibble(`Window Begin` = windowBegin, `Window End` = windowEnd) %>%
#      bind_cols(summarise(exceedancesIn3YrWindowData, nExceedancesInWindow = n())) %>%  # count number of exceedances in 3 year window
#      bind_cols(tibble(associatedExceedanceData = list(exceedancesIn3YrWindowData))) # dataset with just exceedances in each exceeding window
#    exceedancesIn3YrWindow <- bind_rows(exceedancesIn3YrWindow, exceedancesIn3YrWindowi) 
#  }
#  
#  # Summarize results for user
#  # More than one 3 year window with exceedance
#  if(nrow(exceedancesIn3YrWindow) > 1){
#    return(
#      list(
#        tibble(AMMONIA_EXC = ifelse(max(exceedancesIn3YrWindow$nExceedancesInWindow) == 1, nrow(exceedancesIn3YrWindow), max(exceedancesIn3YrWindow$nExceedancesInWindow)),
#               AMMONIA_STAT = 'IM',
#               `Assessment Decision` = paste0('Dataset contains more than one 3 year window with at least one ', assessmentType , ' exceedance.')),
#        `Exceedance Results` = exceedancesIn3YrWindow)     )
#  } else { # only one exceedance in any 3 year window
#    return(
#      list(
#        tibble(AMMONIA_EXC = max(exceedancesIn3YrWindow$nExceedancesInWindow),
#               AMMONIA_STAT = 'Review',
#               `Assessment Decision` = paste0('Dataset contains one 3 year window with at least one ', assessmentType, ' exceedance.')),
#        `Exceedance Results` = exceedancesIn3YrWindow)     )
    
  }
#} else { # No exceedances
#  return(
#    list(
#      tibble(AMMONIA_EXC = 0,
#             AMMONIA_STAT = 'S',
#             `Assessment Decision` = paste0('Dataset contains no ', assessmentType, ' exceedances.')),
#      `Exceedance Results` = NA)  )
#}
#}

#freshwaterAssessments <- list(acute = freshwaterNH3Assessment(x, 'acute'),
#                              chronic = freshwaterNH3Assessment(x, 'chronic'),
#                              fourDay = freshwaterNH3Assessment(x, 'four-day'))
  
# Function to consolidate 3 assessments to fit one row
#ammoniaDecision <- function(freshwaterAssessments # list of freshwater assessments to consolidate into a single decision
#                            ){ 
#  consolidatedResults <- map_df(freshwaterAssessments, 1)
#  
#  review <- filter(consolidatedResults, AMMONIA_STAT %in% c('IM', 'Review'))
#  if(nrow(review) > 1){
#    
#    stationTableOutput <- review %>%
#      slice_max(AMMONIA_EXC, n = 1)
#    
#    # special case if max results in tie, just choose 1
#    if(nrow(stationTableOutput) > 1){
#      stationTableOutput <- stationTableOutput[1,] }
#    
#    # Add the other assessment decisions into comment field
#    extra <- paste(filter(consolidatedResults, ! `Assessment Decision` %in% stationTableOutput$`Assessment Decision`)$`Assessment Decision`, collapse = ' ')
#    stationTableOutput <- mutate(stationTableOutput, `Assessment Decision` = paste(`Assessment Decision`, extra))
#  } else { # no exceedances of any type in this scenario
#    stationTableOutput <- consolidatedResults[1,]
#    # Add the other assessment decisions into comment field
#    extra <- paste(filter(consolidatedResults, ! `Assessment Decision` %in% stationTableOutput$`Assessment Decision`)$`Assessment Decision`, collapse = ' ')
#    stationTableOutput <- mutate(stationTableOutput, `Assessment Decision` = paste(`Assessment Decision`, extra))
#    
#  }
#  return(stationTableOutput)
#}
  
  
  
### app testing part

stationData <- #filter(conventionals, FDT_STA_ID %in% '2-XDD000.40') %>%
  #filter(conventionals, FDT_STA_ID %in% '2-JMS279.41') %>% # 2BJMS279.41 good cbay station example with lots of data
  filter(conventionals, FDT_STA_ID %in% '4ABSA000.62') %>% # good example with lots of data, lake station so depth is important and hourly averages
  left_join(dplyr::select(stationTable, STATION_ID:VAHU6,
                          WQS_ID:EPA_ECO_US_L3NAME),
            #WQS_ID:`Max Temperature (C)`), 
            by = c('FDT_STA_ID' = 'STATION_ID')) %>%
  #filter(!is.na(ID305B_1)) %>% # 4ABSA000.62 doesn't have an AU?????
  pHSpecialStandardsCorrection()

# For testing 4 day stuff
#stationData <- filter(stationData, !is.na(AMMONIA))
#stationData$FDT_DATE_TIME[c(2, 4, 6, 8)] <- as.POSIXct(c("2015-04-28 11:50:00 EDT", "2015-05-12 11:30:00 EDT", "2015-06-11 11:10:00 EDT", "2015-07-22 11:00:00 EDT"))
#stationData <- stationData[1:9,]


#z <- freshwaterNH3limit(stationData, trout = TRUE, mussels = TRUE, earlyLife = TRUE)  

z <- filter(ammoniaAnalysis, StationID %in% unique(stationData$FDT_STA_ID)) %>%
  map(1) 
oneStationAnalysis <- z$AmmoniaAnalysis 

oneStationDecision <- ammoniaDecision(list(acute = freshwaterNH3Assessment(oneStationAnalysis, 'acute'),
                                           chronic = freshwaterNH3Assessment(oneStationAnalysis, 'chronic'),
                                           fourDay = freshwaterNH3Assessment(oneStationAnalysis, 'four-day')))

#filter(oneStationAnalysis, acuteExceedance == TRUE) %>%
#  dplyr::select(FDT_DATE_TIME:FDT_FIELD_PH, 'AMMONIA Rounded to WQS Format' = AMMONIA, acuteNH3limit)

freshwaterNH3Assessment(oneStationAnalysis, 'acute')[[1]]

chronicData <- dplyr::select(oneStationAnalysis, FDT_DATE_TIME, `30dayAmmoniaAvg`:chronicNH3limit, chronicExceedance, associatedWindowData) %>% 
  filter(!is.na(chronicExceedance))


chronicSelection <- chronicData[2,]
#chronicSelection <- as_tibble(chronicSelection$associatedWindowData[[1]])

windowData <- dplyr::select(chronicSelection, associatedWindowData) %>%
  unnest(cols = c(associatedWindowData)) %>%
  mutate(`30dayAmmoniaAvg` = chronicSelection$`30dayAmmoniaAvg`,
         chronicNH3limit = chronicSelection$chronicNH3limit)



windowChoice <- unique(as.Date(chronicData$FDT_DATE_TIME))[2]

windowCriteria <- filter(chronicData, as.Date(FDT_DATE_TIME) %in% windowChoice)
windowData <- dplyr::select(windowCriteria, associatedWindowData) %>%
  unnest(cols = c(associatedWindowData)) %>%
  mutate(`30dayAmmoniaAvg` = windowCriteria$`30dayAmmoniaAvg`,
         TempAvg = windowCriteria$TempAvg,
         pHAvg = windowCriteria$pHAvg,
         `Date Time` = as.POSIXct(strptime(FDT_DATE_TIME, format="%Y-%m-%d")))#as.POSIXct(windowData$`Date Time`, format="%Y-%m-%d", tz='GMT') + as.difftime(1, units="days")



# 4 day part of ammonia module

fourDayData <- dplyr::select(oneStationAnalysis, FDT_DATE_TIME, fourDayAmmoniaAvg, fourDayAvglimit, fourDayExceedance, fourDayWindowData)  %>%
    filter(!is.na(fourDayAmmoniaAvg))

dplyr::select(fourDayData, 
                     "4 Day Window Begin Date" = FDT_DATE_TIME, 
                     '4 Day Averaged Ammonia Rounded to WQS Format' = fourDayAmmoniaAvg,
                     '4 Day Ammonia Criteria' = fourDayAvglimit)
  

fourDaySelection <- fourDayData[2,]#[input$avg4DayData_rows_selected, ]

fourDayWindowData <- dplyr::select(fourDaySelection, fourDayWindowData) %>%
  unnest(cols = c(fourDayWindowData)) %>%
  mutate(`fourDayAmmoniaAvg` = fourDaySelection$fourDayAmmoniaAvg,
         fourDayAvgLimit = fourDaySelection$fourDayAvglimit) 
fourDayWindowData$`Date Time` <- as.Date(fourDayWindowData$FDT_DATE_TIME, format="%m/%d/%y")


plot_ly(data=fourDayWindowData) %>%
  add_markers(x= ~`Date Time`, y= ~AMMONIA, mode = 'scatter', name="Ammonia (mg/L as N)", marker = list(color= '#535559'),
              hoverinfo="text",text=~paste(sep="<br>",
                                           paste("Date: ",`Date Time`),
                                           paste("Ammonia: ",AMMONIA,"mg/L as N"))) %>%
  add_lines(data=fourDayWindowData, x=~`Date Time`, y=~`fourDayAmmoniaAvg`, mode='line', line = list(color = 'orange', dash= 'dash'),
            hoverinfo = "text", text= ~paste("4 day Window Ammonia Average: ", `fourDayAmmoniaAvg`," mg/L as N", sep=''), 
            name="4 Day Window Ammonia Average") %>%
  add_lines(data=fourDayWindowData, x=~`Date Time`,y=~fourDayAvgLimit, mode='line', line = list(color = '#484a4c',dash = 'dot'),
            hoverinfo = "text", text= ~paste("4 Day Window Ammonia Criteria", fourDayAvgLimit," mg/L as N", sep=''), 
            name="4 Day Window Ammonia Criteria") %>% 
  layout(showlegend=FALSE,
         yaxis=list(title="Ammonia (mg/L as N)"),
         xaxis=list(title="Sample Date",tickfont = list(size = 10))) 
