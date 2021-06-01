source('global.R')

# Pull data from server
conventionals <- conventionals <- pin_get('conventionals2022IRdraftWithSecchi', board = "rsconnect") %>%
  #pin_get("conventionals2022IRdraft", board = "rsconnect") %>%
  filter(FDT_DATE_TIME >= "2015-01-01 00:00:00 UTC" )
#vahu6 <- st_as_sf(pin_get("vahu6", board = "rsconnect")) # bring in as sf object
WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")
# placeholder for now, shouldn't be a spatial file
historicalStationsTable <-  st_read('data/GIS/va20ir_wqms.shp') %>%
  st_drop_geometry()#read_csv('data/stationsTable2022begin.csv') # last cycle stations table (forced into new station table format)
WQMstationFull <- pin_get("WQM-Station-Full", board = "rsconnect")
WCmetals <- pin_get("WCmetals-2022IRfinal",  board = "rsconnect")
# Separate object for analysis, tack on METALS and RMK designation to make the filtering of certain lab comment codes easier
WCmetalsForAnalysis <- WCmetals %>% 
  dplyr::select(Station_Id, FDT_DATE_TIME, FDT_DEPTH, # include depth bc a few samples taken same datetime but different depths
                METAL_Antimony = `STORET_01095_ANTIMONY, DISSOLVED (UG/L AS SB)`, RMK_Antimony = RMK_01097, 
                METAL_Arsenic = `STORET_01000_ARSENIC, DISSOLVED  (UG/L AS AS)`, RMK_Arsenic = RMK_01002, 
                METAL_Barium = `STORET_01005_BARIUM, DISSOLVED (UG/L AS BA)`, RMK_Barium = RMK_01005, 
                METAL_Cadmium = `STORET_01025_CADMIUM, DISSOLVED (UG/L AS CD)`, RMK_Cadmium = RMK_01025,
                METAL_Chromium = `STORET_01030_CHROMIUM, DISSOLVED (UG/L AS CR)`, RMK_Chromium = RMK_01030, 
                # Chromium III and ChromiumVI dealt with inside metalsAnalysis()
                METAL_Copper = `STORET_01040_COPPER, DISSOLVED (UG/L AS CU)`, RMK_Copper = RMK_01040, 
                METAL_Lead = `STORET_01049_LEAD, DISSOLVED (UG/L AS PB)`, RMK_Lead = RMK_01049, 
                METAL_Mercury = `STORET_50091_MERCURY-TL,FILTERED WATER,ULTRATRACE METHOD UG/L`, RMK_Mercury = RMK_50091,
                METAL_Nickel = `STORET_01065_NICKEL, DISSOLVED (UG/L AS NI)`, RMK_Nickel = RMK_01067, 
                METAL_Uranium = `URANIUM_TOT`, RMK_Uranium = `RMK_7440-61-1T`, 
                METAL_Selenium = `STORET_01145_SELENIUM, DISSOLVED (UG/L AS SE)`, RMK_Selenium = RMK_01145, 
                METAL_Silver = `STORET_01075_SILVER, DISSOLVED (UG/L AS AG)`, RMK_Silver = RMK_01075, 
                METAL_Thallium = `STORET_01057_THALLIUM, DISSOLVED (UG/L AS TL)`, RMK_Thallium = RMK_01057,
                METAL_Zinc = `STORET_01090_ZINC, DISSOLVED (UG/L AS ZN)`, RMK_Zinc = RMK_01092,
                METAL_Hardness = `STORET_DHARD_HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED`, RMK_Hardness = RMK_DHARD) %>% 
  group_by(Station_Id, FDT_DATE_TIME, FDT_DEPTH) %>% 
  mutate_if(is.numeric, as.character) %>% 
  pivot_longer(cols = METAL_Antimony:RMK_Hardness, #RMK_Antimony:RMK_Hardness, 
               names_to = c('Type', 'Metal'),
               names_sep = "_",
               values_to = 'Value') %>% 
  ungroup() %>% group_by(Station_Id, FDT_DATE_TIME, FDT_DEPTH, Metal) %>% 
  pivot_wider(id_cols = c(Station_Id, FDT_DATE_TIME, FDT_DEPTH, Metal), names_from = Type, values_from = Value) %>% # pivot remark wider so the appropriate metal value is dropped when filtering on lab comment codes
  filter(! RMK %in% c('IF', 'J', 'O', 'QF', 'V')) %>% # lab codes dropped from further analysis
  pivot_longer(cols= METAL:RMK, names_to = 'Type', values_to = 'Value') %>% # get in appropriate format to flip wide again
  pivot_wider(id_cols = c(Station_Id, FDT_DATE_TIME, FDT_DEPTH), names_from = c(Type, Metal), names_sep = "_", values_from = Value) %>% 
  mutate_at(vars(contains('METAL')), as.numeric) %>%# change metals values back to numeric
  rename_with(~str_remove(., 'METAL_')) # drop METAL_ prefix for easier analyses
Smetals <- pin_get("Smetals-2022IRfinal",  board = "rsconnect")
IR2020WCmetals <- pin_get("WCmetals-2020IRfinal",  board = "rsconnect")
IR2020Smetals <- pin_get("Smetals-2020IRfinal",  board = "rsconnect")


# Bring in local data (for now)
ammoniaAnalysis <- readRDS('userDataToUpload/processedStationData/ammoniaAnalysis.RDS')
markPCB <- read_excel('data/2022 IR PCBDatapull_EVJ.xlsx', sheet = '2022IR Datapull EVJ') %>%
  mutate(SampleDate = as.Date(SampleDate))
fishPCB <- read_excel('data/FishTissuePCBsMetals_EVJ.xlsx', sheet= 'PCBs')
fishMetals <- read_excel('data/FishTissuePCBsMetals_EVJ.xlsx', sheet= 'Metals') %>%
  rename("# of Fish" = "# of fish...4", "Species_Name"  = "Species_Name...5", 
         "species_name" = "Species_Name...47", "number of fish" = "# of fish...48")
fishMetalsScreeningValues <- read_csv('data/FishMetalsScreeningValues.csv') %>%
  group_by(`Screening Method`) %>% 
  pivot_longer(cols = -`Screening Method`, names_to = 'Metal', values_to = 'Screening Value') %>%
  arrange(Metal)
lakeNutStandards <- read_csv('data/9VAC25-260-187lakeNutrientStandards.csv')

# user brings in station table on first page
stationTable1 <- read_csv('userDataToUpload/processedStationData/stationTableResults.csv',
                         col_types = cols(COMMENTS = col_character(),
                                          LACUSTRINE = col_character())) %>%# force to character bc parsing can incorrectly guess logical based on top 1000 rows
  filter_at(vars(starts_with('TYPE')), any_vars(. == 'L')) %>% # keep only lake stations
  # add WQS information to stations
  left_join(WQSlookup, by = c('STATION_ID'='StationID')) %>%
  mutate(CLASS_BASIN = paste(CLASS,substr(BASIN, 1,1), sep="_")) %>%
  mutate(CLASS_BASIN = ifelse(CLASS_BASIN == 'II_7', "II_7", as.character(CLASS))) %>%
  # Fix for Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard)
  left_join(WQSvalues, by = 'CLASS_BASIN') %>%
  dplyr::select(-c(CLASS.y,CLASS_BASIN)) %>%
  rename('CLASS' = 'CLASS.x') %>%
  left_join(dplyr::select(WQMstationFull, WQM_STA_ID, EPA_ECO_US_L3CODE, EPA_ECO_US_L3NAME) %>%
              distinct(WQM_STA_ID, .keep_all = TRUE), by = c('STATION_ID' = 'WQM_STA_ID')) %>% # last cycle had code to fix Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard) but not sure if necessary
  lakeNameStandardization() %>% # standardize lake names
  
  # extra special step
  mutate(Lake_Name = case_when(STATION_ID %in% c('2-TRH000.40') ~ 'Thrashers Creek Reservoir',
                               STATION_ID %in% c('2-LSL000.16') ~ 'Lone Star Lake F (Crystal Lake)',
                               STATION_ID %in% c('2-LSL000.04') ~ 'Lone Star Lake G (Crane Lake)',
                               STATION_ID %in% c('2-LSL000.20') ~ 'Lone Star Lake I (Butler Lake)',
                               STATION_ID %in% c('2-NWB002.93','2-NWB004.67', '2-NWB006.06') ~ 'Western Branch Reservoir',
                               STATION_ID %in% c('2-LDJ000.60') ~ 'Lake Nottoway (Lee Lake)',
                               TRUE ~ as.character(Lake_Name))) %>%
  
  
  left_join(lakeNutStandards, by = c('Lake_Name')) %>%
  # lake drummond special standards
  mutate(`Chlorophyll a (ug/L)` = case_when(Lake_Name %in% c('Lake Drummond') ~ 35,
                                            TRUE ~ as.numeric(`Chlorophyll a (ug/L)`)),
         `Total Phosphorus (ug/L)` = case_when(Lake_Name %in% c('Lake Drummond') ~ 40,
                                               TRUE ~ as.numeric(`Total Phosphorus (ug/L)`))) %>%
  mutate(lakeStation = TRUE)



# User clicks region and brings back entire state (this just expedites original app rendering and spreads
# out data requests)
regionalAUs1 <- st_zm(st_as_sf(pin_get('AUreservoir_EVJ', board = 'rsconnect'))) %>%
  lakeNameStandardization()


DEQregionSelection1 <- 'BRRO'
lakeSelection1 <- "Smith Mountain Lake"#"Claytor Lake"#"Townes Reservoir" #"Falling Creek Reservoir"#"Hogan Lake"#"Smith Mountain Lake"
 # filter(regionalAUs, ASSESS_REG %in% DEQregionSelection) %>% 
#  distinct(Lake_Name) %>% arrange(Lake_Name) %>% pull()


AUs1 <- filter(regionalAUs1, Lake_Name %in% lakeSelection1 & ASSESS_REG %in% DEQregionSelection1)
#the_data <- filter(regionalAUs, ASSESS_REG %in% DEQregionSelection) 
#lake_AUs <- filter(the_data, Lake_Name %in% lakeSelection)
lake_filter1 <- filter_at(stationTable1, vars(starts_with('ID305B')), any_vars(. %in% AUs1$ID305B)) 


# Stations carried over
carryoverStations <- filter(lake_filter1, str_detect(COMMENTS, "This station has no data"))  




# Lake Map
#lakeStations1 <- lake_filter1 %>%
#  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
#           remove = F, # don't remove these lat/lon cols from df
#           crs = 4326) # add projection, needs to be geographic for now bc entering lat/lng
# 
# z <- suppressWarnings(st_coordinates(sf::st_centroid(AUs1 %>% group_by(Lake_Name) %>% summarise())))
# 
# CreateWebMap(maps = c("Topo","Imagery"), collapsed = TRUE) %>%
#   {if(nrow(AUs1)>1)
#     setView(., z[1], z[2], zoom = 10) 
#     else setView(., z[1], z[2], zoom = 12) } %>%
#   addPolygons(data= AUs1, group = 'Selected Lake',
#               popup=leafpop::popupTable(AUs1, zcol=c('Lake_Name',"ID305B","ASSESS_REG"))) %>%
#   {if(nrow(lakeStations1) > 0)
#     addCircleMarkers(., data = lakeStations1, color='black', fillColor='yellow', radius = 4,
#                      fillOpacity = 0.5,opacity=0.8,weight = 1,stroke=T, group="Monitored Stations",
#                      label = ~STATION_ID, layerId = ~STATION_ID) 
#     else . } %>%
#   addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
#                    overlayGroups = c('Monitored Stations', 'Selected Lake'),
#                    options=layersControlOptions(collapsed=T),
#                    position='topleft')
            
## Assessment Unit Review
#dplyr::select(lake_filter1, Lake_Name, VAHU6, Lakes_187B) %>%
#  group_by(Lake_Name) %>%
#  summarise(VAHU6 = toString(sort(unique(VAHU6))),
#            `Section 187` = toString(sort(unique(Lakes_187B))))

conventionalsLake1 <- #filter(conventionals, FDT_STA_ID %in% filter(stationTable1, is.na(Lakes_187B))$STATION_ID) %>% # just non 187 lakes
  filter(conventionals, FDT_STA_ID %in% lake_filter1$STATION_ID) %>%
  left_join(dplyr::select(stationTable1, STATION_ID:VAHU6, lakeStation,
                          WQS_ID:`Total Phosphorus (ug/L)`),
            #WQS_ID:`Max Temperature (C)`), 
            by = c('FDT_STA_ID' = 'STATION_ID')) %>%
  filter(!is.na(ID305B_1)) %>%
  pHSpecialStandardsCorrection() %>% #correct pH to special standards where necessary
  thermoclineDepth() # adds thermocline information and SampleDate


AUselectionOptions1 <- unique(dplyr::select(lake_filter1, ID305B_1:ID305B_10) %>% 
         mutate_at(vars(starts_with("ID305B")), as.character) %>%
         pivot_longer(ID305B_1:ID305B_10, names_to = 'ID305B', values_to = 'keep') %>%
         pull(keep) )
selectedAU1 <- AUselectionOptions1[!is.na(AUselectionOptions1) & !(AUselectionOptions1 %in% c("NA", "character(0)", "logical(0)"))][1]
#selectedAU1 <- 'VAW-L12L_ROA01A02'

stationSelectionOptions1 <- filter_at(lake_filter1, vars(starts_with("ID305B")), any_vars(. %in% selectedAU1)) %>%
  distinct(STATION_ID) %>%
  arrange(STATION_ID) %>%
  pull()
stationSelection1 <- stationSelectionOptions1[1]

AUData1 <- filter_at(conventionalsLake1, vars(starts_with("ID305B")), any_vars(. %in% selectedAU1) )

# just for nutrients
AUData11 <- filter(AUData1, ID305B_1 %in% selectedAU1)

stationData1 <- filter(AUData1, FDT_STA_ID %in% stationSelection1) 


stationInfo1 <- filter(stationTable1, STATION_ID == stationSelection1) %>% 
  select(STATION_ID:VAHU6, WQS_ID:`Total Phosphorus (ug/L)`)

# point <- dplyr::select(stationInfo1,  STATION_ID, starts_with('ID305B'), LATITUDE, LONGITUDE ) %>%
#   st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
#            remove = F, # don't remove these lat/lon cols from df
#            crs = 4326) # add projection, needs to be geographic for now bc entering lat/lng
# segmentChoices <- dplyr::select(point, starts_with('ID305B')) %>% st_drop_geometry() %>% as.character()  
# segment <- filter(regionalAUs1, ID305B %in% segmentChoices)
# map1 <- mapview(segment,zcol = 'ID305B', label= segment$ID305B, layer.name = 'Assessment Unit (ID305B_1)',
#                 popup= leafpop::popupTable(segment, zcol=c("ID305B","Acres","CYCLE","WATER_NAME")), legend= FALSE) + 
#   mapview(point, color = 'yellow', lwd = 5, label= point$STATION_ID, layer.name = c('Selected Station'),
#           popup=NULL, legend= FALSE)
# map1@map %>% setView(point$LONGITUDE, point$LATITUDE, zoom = 12)




## Nutrient build time

#The nutrient criteria for the man-made lakes and reservoirs listed in Section 187 of the WQS only apply in the top 1 meter of the lacustrine zone. If total phosphorus or chlorophyll a data are collected outside the lacustrine zone, in the riverine or transitional zone, the data from these two zones will not be used in the assessment for lake or reservoir impairment due to nutrients.  





## Ecoli has to run daily AU medians before can be run through scripts

# save individual ecoli results for later, human understandable
# AUmedians <- AUData1 %>%
#   filter(ID305B_1 %in% selectedAU1) %>% # run ecoli by only 1 AU at a time
#   group_by(SampleDate) %>%
#   filter(!is.na(ECOLI)) %>%
#   mutate(EcoliDailyMedian = median(ECOLI, na.rm = TRUE)) %>%
#   dplyr::select(ID305B_1, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, SampleDate, EcoliDailyMedian, ECOLI, RMK_ECOLI, LEVEL_ECOLI) %>%
#   arrange(SampleDate) %>% ungroup()
#   
# 
# # need to run analysis on only one point per day, function understandable
# AUmediansForAnalysis <- AUmedians %>%
#   filter(! LEVEL_ECOLI %in% c('Level I', 'Level II')) %>%
#   mutate(ECOLI_Station = ECOLI,
#          ECOLI = EcoliDailyMedian,
#          FDT_STA_ID = unique(ID305B_1),
#          FDT_DATE_TIME = SampleDate) %>%
#   dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, SampleDate, ECOLI, ECOLI_Station, RMK_ECOLI, LEVEL_ECOLI) %>%
#   distinct(SampleDate, .keep_all = T) 
# 
# z <- bacteriaAssessmentDecision(AUmediansForAnalysis, 'ECOLI', 'LEVEL_ECOLI', 10, 410, 126)# %>%
#   #dplyr::select(StationID:ECOLI_STATECOLI_VERBOSE)

# App module stuff

# dat <- AUmedians %>%
#   mutate(newSTV = 410, geomean = 126, oldSTV = 235)
# plot_ly(data=dat) %>%
#   add_markers(x= ~SampleDate, y= ~ECOLI,mode = 'scatter', name="E. coli (CFU / 100 mL)", 
#               color = ~FDT_STA_ID, #list(color= '#535559'),
#               colors = "Dark2",
#               hoverinfo="text",text=~paste(sep="<br>",
#                                            paste("FDT_STA_ID: ",FDT_STA_ID),
#                                            paste("Date: ",SampleDate),
#                                            paste("Depth: ",FDT_DEPTH, "m"),
#                                            paste("E. coli: ",ECOLI,"CFU / 100 mL"),
#                                            paste("Level: ",LEVEL_ECOLI)))%>%
#   add_lines(data=dat, x=~SampleDate,y=~newSTV, mode='line', line = list(color = '#484a4c',dash = 'dot'),
#             hoverinfo = "text", text= "New STV: 410 CFU / 100 mL", name="New STV: 410 CFU / 100 mL") %>%
#   add_lines(data=dat, x=~SampleDate,y=~oldSTV, mode='line', line = list(color = 'black'),
#             hoverinfo = "text", text= "Old SSM: 235 CFU / 100 mL", name="Old SSM: 235 CFU / 100 mL") %>%
#   add_lines(data=dat, x=~SampleDate,y=~geomean, mode='line', line = list(color = 'black', dash= 'dash'),
#             hoverinfo = "text", text= "Geomean Criteria: 126 CFU / 100 mL", name="Geomean Criteria: 126 CFU / 100 mL") %>%
#   layout(showlegend=FALSE,
#          yaxis=list(title="E. coli (CFU / 100 mL)"),
#          xaxis=list(title="Sample Date",tickfont = list(size = 10)))

# oneAUDecisionData1 <- z[['associatedDecisionData']][[1]]
# AU1 <- filter(AUData1, ! is.na(ECOLI))
# rawData <- dplyr::select(AUmediansForAnalysis, SampleDate, ECOLI) %>% 
#   rename('Daily E.coli Median' = 'ECOLI')
# 
# 
# windowDat <- filter(oneAUDecisionData1, as.character(`Date Window Starts`) %in% as.character(as.Date(rawData$SampleDate)[5])) %>%#input$rawData_rows_selected]))) %>% 
#   dplyr::select( associatedData) %>%
#   unnest(cols = c(associatedData)) %>%
#   mutate(newSTV = 410, geomeanLimit = 126,
#          `Date Time` = as.POSIXct(strptime(FDT_DATE_TIME, format="%Y-%m-%d")))
# windowData1 <- bind_rows(windowDat,
#           tibble(`Date Time` = c(min(windowDat$`Date Time`)- days(5), max(windowDat$`Date Time`) + days(5)),
#                  newSTV = 410, geomeanLimit = 126)) 
# 
# plot_ly(data=windowData1) %>%
#   add_markers(x= ~`Date Time`, y= ~Value,mode = 'scatter', name="E. coli (CFU / 100 mL)", marker = list(color= '#535559'),
#               hoverinfo="text",text=~paste(sep="<br>",
#                                            paste("Date: ",`Date Time`),
#                                            paste("Daily Median E. coli: ",Value,"CFU / 100 mL"))) %>%
#   add_lines(data=windowData1, x=~`Date Time`, y=~geomean, mode='line', line = list(color = 'orange', dash= 'dash'),
#             hoverinfo = "text", text= ~paste("Window Geomean: ", format(geomean,digits=3)," CFU / 100 mL", sep=''), 
#             name="Window Geomean") %>%
#   add_lines(data=windowData1, x=~`Date Time`,y=~newSTV, mode='line', line = list(color = '#484a4c',dash = 'dot'),
#             hoverinfo = "text", text= "New STV: 410 CFU / 100 mL", name="New STV: 410 CFU / 100 mL") %>%
#   add_lines(data=windowData1, x=~`Date Time`,y=~geomeanLimit, mode='line', line = list(color = 'black', dash= 'dash'),
#             hoverinfo = "text", text= "Geomean Criteria: 126 CFU / 100 mL", name="Geomean Criteria: 126 CFU / 100 mL") %>%
#   layout(showlegend=FALSE,
#          yaxis=list(title="E. coli (CFU / 100 mL)"),
#          xaxis=list(title="Sample Date",tickfont = list(size = 10)))
# 
# oneAUDecisionData1[5,] %>%
#   dplyr::select(-associatedData) 


### TSI plot
datOG <- TSIcalculation(stationData1) 
dat <- datOG %>%
  dplyr::select( associatedData) %>%
  unnest(cols = c(associatedData)) %>%
  mutate(`Overall TSI SD` = datOG$TSI_SD,
         `Overall TSI Chl a` = datOG$TSI_chla,
         `Overall TSI TP` = datOG$TSI_TP)


box1 <- data.frame(FDT_DATE_TIME = c(min(dat$FDT_DATE_TIME), min(dat$FDT_DATE_TIME), max(dat$FDT_DATE_TIME),max(dat$FDT_DATE_TIME)), y = c(80, 100, 100, 80))
box2 <- data.frame(x = c(min(dat$FDT_DATE_TIME), min(dat$FDT_DATE_TIME), max(dat$FDT_DATE_TIME),max(dat$FDT_DATE_TIME)), y = c(60, 80, 80, 60))
box3 <- data.frame(x = c(min(dat$FDT_DATE_TIME), min(dat$FDT_DATE_TIME), max(dat$FDT_DATE_TIME),max(dat$FDT_DATE_TIME)), y = c(40, 60, 60, 40))
box4 <- data.frame(x = c(min(dat$FDT_DATE_TIME), min(dat$FDT_DATE_TIME), max(dat$FDT_DATE_TIME),max(dat$FDT_DATE_TIME)), y = c(0, 40, 40, 0))

plot_ly(data=box1)%>%
  add_polygons(x = ~FDT_DATE_TIME, y = ~y, data = box1, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
               hoverinfo="text", name =paste('Trophic State: Hypereutrophic')) %>%
  add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
               hoverinfo="text", name =paste('Trophic State: Eutrophic')) %>%
  add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
               hoverinfo="text", name =paste('Trophic State: Mesotrophic')) %>%
  add_polygons(data = box4, x = ~x, y = ~y, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
               hoverinfo="text", name =paste('Trophic State: Oligotrophic')) %>%
  add_lines(data=dat, x=~FDT_DATE_TIME,y=~`Overall TSI SD`, mode='line', line = list(color = 'black'),
            hoverinfo = "text", text="Overall TSI SD", name="Overall TSI SD") %>%
  add_markers(data=dat, x= ~FDT_DATE_TIME, y= ~TSI_SD,mode = 'scatter', name="TSI_SD", marker = list(color= '#535559'),
              hoverinfo="text",text=~paste(sep="<br>",
                                           paste("Date: ",FDT_DATE_TIME),
                                           paste("Depth: ",FDT_DEPTH, "m"),
                                           paste("TSI_SD: ",TSI_SD)))%>%
  layout(showlegend=FALSE,
         yaxis=list(title="TSI Secchi Depth (unitless)"),
         xaxis=list(title="Sample Date",tickfont = list(size = 10)))



FmetalsSV <- dplyr::select(fishMetals, Station_ID, Collection_Date_Time, Sample_ID,  `# of Fish`, Species_Name, length, weight, Beryllium:Lead) %>%
  dplyr::select(-contains('RMK_')) %>%
  group_by( Station_ID, Collection_Date_Time, Sample_ID, `# of Fish`, Species_Name, length, weight) %>%
  pivot_longer(cols= Beryllium:Lead, names_to = "Metal", values_to = 'Measure') %>%
  left_join(metalsSV, by = 'Metal') %>%
  filter(Measure > `Screening Value`) %>%
  arrange(Metal)
