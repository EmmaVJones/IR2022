# app testing data
source('global.R')

DEQregionSelection <- 'BRRO'
basinSelection <- 'James-Upper'#"Roanoke"#"James-Middle"#"Roanoke"#'James-Upper'#
HUC6Selection <- "JU11"#'RL12'#"JM02"#'JM16'#'RU09'#'RL12'#


conventionals <- pin_get("conventionals2022IRdraft", board = "rsconnect") %>%
  filter(FDT_DATE_TIME >= "2015-01-01 00:00:00 UTC" )
vahu6 <- st_as_sf(pin_get("vahu6", board = "rsconnect")) # bring in as sf object
WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")
# placeholder for now, shouldn't be a spatial file
historicalStationsTable <- st_read('data/GIS/2020_wqms.shp') %>%
  st_drop_geometry()#read_csv('data/stationsTable2022begin.csv') # last cycle stations table (forced into new station table format)
WCmetals <- pin_get("WCmetals-2020IRfinal",  board = "rsconnect")
Smetals <- pin_get("Smetals-2020IRfinal",  board = "rsconnect")
WQMstationFull <- pin_get("WQM-Station-Full", board = "rsconnect")
VSCIresults <- pin_get("VSCIresults", board = "rsconnect") %>%
  filter( between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2]) ) %>% # get ecoregion info
  left_join(dplyr::select(WQMstationFull, WQM_STA_ID, EPA_ECO_US_L3CODE, EPA_ECO_US_L3NAME) %>%
              distinct(WQM_STA_ID, .keep_all = TRUE), by = c('StationID' = 'WQM_STA_ID'))
VCPMI63results <- pin_get("VCPMI63results", board = "rsconnect") %>%
  filter( between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2]) ) %>% # get ecoregion info
  left_join(dplyr::select(WQMstationFull, WQM_STA_ID, EPA_ECO_US_L3CODE, EPA_ECO_US_L3NAME) %>%
              distinct(WQM_STA_ID, .keep_all = TRUE), by = c('StationID' = 'WQM_STA_ID'))
VCPMI65results <- pin_get("VCPMI65results", board = "rsconnect") %>%
  filter( between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2]) ) %>% # get ecoregion info
  left_join(dplyr::select(WQMstationFull, WQM_STA_ID, EPA_ECO_US_L3CODE, EPA_ECO_US_L3NAME) %>%
              distinct(WQM_STA_ID, .keep_all = TRUE), by = c('StationID' = 'WQM_STA_ID'))




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
                            WQS_ID:CLASS_DESCRIPTION),
                            #WQS_ID:`Max Temperature (C)`), 
              by = c('FDT_STA_ID' = 'STATION_ID')) %>%
    filter(!is.na(ID305B_1)) %>%
  pHSpecialStandardsCorrection()

AUselection <- unique(c(conventionals_HUC$ID305B_1, 
                        dplyr::select(carryoverStations, ID305B_1:ID305B_10) %>% as.character()))
AUselection <- AUselection[!is.na(AUselection) & !(AUselection %in% c("NA", "character(0)", "logical(0)"))][3]

#selectedAU <-  filter(regionalAUs, ID305B %in% AUselection) %>% st_set_geometry(NULL) %>% as.data.frame()
stationSelection <- filter(conventionals_HUC, ID305B_1 %in% AUselection | ID305B_2 %in% AUselection | 
                ID305B_3 %in% AUselection | ID305B_4 %in% AUselection | ID305B_5 %in% AUselection | 
                ID305B_6 %in% AUselection | ID305B_7 %in% AUselection | ID305B_8 %in% AUselection | 
                ID305B_9 %in% AUselection | ID305B_10 %in% AUselection) %>%
    distinct(FDT_STA_ID) %>%
  pull()
stationSelection <- c(stationSelection, carryoverStations$STATION_ID)
stationSelection <- stationSelection[3]

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
