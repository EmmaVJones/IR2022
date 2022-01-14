
#####################################   UPDATE EACH NEW TOOL REBUILD #############################################
# Establish Assessment Period 
assessmentPeriod <- as.POSIXct(c("2015-01-01 00:00:00 UTC","2020-12-31 23:59:59 UTC"),tz='UTC')
assessmentCycle <- '2022'
##################################################################################################################

# WQS information for functions
# From: 9VAC25-260-50. Numerical Criteria for Dissolved Oxygen, Ph, and Maximum Temperature
# https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section50/
WQSvalues <- tibble(CLASS_BASIN = c('I',"II","II_7","III","IV","V","VI","VII"),
                    CLASS = c('I',"II","II","III","IV","V","VI","VII"),
                    `Description Of Waters` = c('Open Ocean', 'Tidal Waters in the Chowan Basin and the Atlantic Ocean Basin',
                                                'Tidal Waters in the Chesapeake Bay and its tidal tributaries',
                                                'Nontidal Waters (Coastal and Piedmont Zone)','Mountainous Zone Waters',
                                                'Stockable Trout Waters','Natural Trout Waters','Swamp Waters'),
                    `Dissolved Oxygen Min (mg/L)` = c(5,4,NA,4,4,5,6,NA),
                    `Dissolved Oxygen Daily Avg (mg/L)` = c(NA,5,NA,5,5,6,7,NA),
                    `pH Min` = c(6,6,6.0,6.0,6.0,6.0,6.0,3.7),
                    `pH Max` = c(9.0,9.0,9.0,9.0,9.0,9.0,9.0,8.0),
                    `Max Temperature (C)` = c(NA, NA, NA, 32, 31, 21, 20, NA)) %>%
  mutate(CLASS_DESCRIPTION = paste0(CLASS, " | ", `Description Of Waters`))

concatinateUnique <- function(stuff){
  if(length(stuff)==1){
    if(is.na(stuff)){return(NA)
    }else{
      return(paste(unique(stuff), collapse= ', ')) }
  } 
  if(length(stuff) > 1){return(paste(unique(stuff), collapse= ', '))}
}

changeDEQRegionName <- function(stuff){
  # have to do this bc different places in conventionals report the assessment region over sample region
  if(length(stuff) == 1){
    if(is.na(stuff)){return("NA")}
    if(stuff == "Valley"){return('VRO')}
    if(stuff == "Northern"){return('NRO')}
    if(stuff == "Piedmont"){return('PRO')}
    if(stuff == "Blue Ridge"){return('BRRO')}
    if(stuff == "Tidewater"){return('TRO')}
    if(stuff == "Southwest" ){return('SWRO')}
    if(stuff == 'NA'){return('NA')}
  } else {return(concatinateUnique(stuff))}
}

# Lake name standardization
lakeNameStandardization <- function(x){
  x %>% 
    mutate(Lake_Name = case_when(WATER_NAME %in% c('Abel Lake Reservoir (Long Branch)') ~ 'Abel Lake',
                                 WATER_NAME %in% c('Big Cherry Reservior') ~ 'Big Cherry Lake',
                                 WATER_NAME %in% c('Dan River','Buffalo Creek','Bluestone Creek') ~ 'Kerr Reservoir',
                                 WATER_NAME %in% c('Smith Lake (Aquia Reservoir)') ~ 'Aquia Reservoir (Smith Lake)',
                                 WATER_NAME %in% c('Claytor Lake (New River)', 'Claytor Lake (Peak Creek)',
                                                   'Claytor Lake Lower (New River)') ~ 'Claytor Lake',
                                 WATER_NAME %in% c('Fairystone Lake (Goblin Town Creek)') ~ 'Fairystone Lake', 
                                 WATER_NAME %in% c('Harwood Mill Reservoir (PWS)') ~ 'Harwood Mills Reservoir',
                                 WATER_NAME %in% c('Goose Creek') ~ 'Goose Creek Reservoir',
                                 WATER_NAME %in% c('Lake Anna', 'Lake Anna/Contrary Creek', 'Lake Anna/Freshwater Creek', 
                                                   'Lake Anna/Gold Mine Creek', 'Lake Anna/Pamunkey Creek', 
                                                   'Lake Anna/Plentiful Creek', 'Terrys Run/Lake Anna') ~ 'Lake Anna',
                                 WATER_NAME %in% c('Lake Cohoon (PWS)') ~ 'Lake Cohoon',     
                                 WATER_NAME %in% c('Conner Lake') ~ 'Lake Conner',
                                 WATER_NAME %in% c('Lake Kilby (PWS)') ~ 'Lake Kilby',
                                 WATER_NAME %in% c('Lake Meade (PWS)','Pitch Kettle Creek - Lake (PWS)') ~ 'Lake Meade',
                                 WATER_NAME %in% c('Lake Moomaw (Jackson River)','Lake Moomaw Middle (Jackson River)') ~ 'Lake Moomaw',
                                 WATER_NAME %in% c('Lake Prince - Reservoir (PWS)') ~ 'Lake Prince',
                                 WATER_NAME %in% c('Lake Shenandoah') ~ 'Shenandoah Lake',
                                 WATER_NAME %in% c('Lake Smith (PWS)') ~ 'Lake Smith',
                                 WATER_NAME %in% c('Lake Whitehurst (PWS)') ~ 'Lake Whitehurst',
                                 WATER_NAME %in% c('Leesville Lake', 'Leesville Lake (Pigg R.)', 
                                                   'Leesville Lake Middle (Roanoke R.)') ~ 'Leesville Lake',
                                 WATER_NAME %in% c('Lee Hall Reservoir- Upper, Middle','Lee Hall Reservoir-Lower') ~ 'Lee Hall Reservoir',
                                 WATER_NAME %in% c('Little Creek Reservoir - (PWS)') ~ 'Little Creek Reservoir (VBC)',
                                 WATER_NAME %in% c('Little Creek Reservoir (PWS)') ~ 'Little Creek Reservoir (JCC)',
                                 WATER_NAME %in% c('Lonestar Lake F (PWS)') ~ 'Lone Star Lake F (Crystal Lake)', 
                                 WATER_NAME %in% c('Lone Star Lake G (PWS)') ~ 'Lone Star Lake G (Crane Lake)', 
                                 WATER_NAME %in% c('Lone Star Lake I (PWS)') ~ 'Lone Star Lake I (Butler Lake)', 
                                 WATER_NAME %in% c('Martinsville (Beaver Creek) Reservoir',
                                                   'Martinsville Reservoir') ~ 'Martinsville Reservoir (Beaver Creek Reservoir)',
                                 WATER_NAME %in% c('Moormans River') ~ 'Sugar Hollow Reservoir',
                                 WATER_NAME %in% c('North River') ~ 'Staunton Dam Lake',
                                 WATER_NAME %in% c('Philpott Reservoir (Goblin Town Creek)', 'Philpott Reservoir Lower (Smith River)',
                                                   'Philpott Reservoir (Smith River)') ~ "Philpott Reservoir",
                                 WATER_NAME %in% c('Roanoke River','Lake Gaston') ~ 'Lake Gaston',     
                                 WATER_NAME %in% c('Roaring Fork Reservoir') ~ 'Roaring Fork',
                                 WATER_NAME %in% c('S F Rivanna River Reservoir') ~ 'Rivanna Reservoir (South Fork Rivanna Reservoir)',
                                 str_detect(WATER_NAME, 'Smith Mtn. Lake') ~ 'Smith Mountain Lake', 
                                 WATER_NAME %in% c('Speights Run - Lake (PWS)') ~ 'Speights Run Lake',
                                 WATER_NAME %in% c('Troublesome Reservoir') ~ 'Troublesome Creek Reservoir',
                                 WATER_NAME %in% c('Waller Mill Reservoir [PWS]') ~ 'Waller Mill Reservoir',
                                 WATER_NAME %in% c('Unnamed pond near Tanyard Swamp') ~ 'Tanyard Swamp',
                                 WATER_NAME %in% c('Unsegmented lakes in G03') ~ 'West Run',
                                 TRUE ~ as.character(WATER_NAME))) 
}
#test <- regionalAUs %>%
#  lakeNameStandardization()



quickStats <- function(parameterDataset, parameter){
  if(nrow(parameterDataset) > 0 & any(!is.na(parameterDataset$limit))){
    results <- data.frame(EXC = nrow(filter(parameterDataset, exceeds == TRUE)),
                          SAMP = nrow(parameterDataset)) %>%
      # Implement Round to Even on Exceedance Frequency
      mutate(exceedanceRate = as.numeric(round::roundAll((EXC/SAMP)*100,digits=0, "r0.C"))) # round to nearest whole number per Memo to Standardize Rounding for Assessment Guidance
    
    if(results$EXC >= 1){outcome <- 'Review'} # for Mary
    if(results$EXC >= 1 & results$exceedanceRate < 10.5){outcome <- 'Review'}
    if(results$exceedanceRate > 10.5 & results$EXC >= 2 & results$SAMP > 10){outcome <- '10.5% Exceedance'}
    if(results$EXC < 1 &results$exceedanceRate < 10.5 & results$SAMP > 10){outcome <- 'S'}
    if(results$EXC >= 1 & results$SAMP <= 10){outcome <- 'Review'}
    if(results$EXC < 1 & results$SAMP <= 10 & results$SAMP > 1){outcome <- 'S'} # & results$SAMP >1 new 12/21/2020 can't say supporting on 1 sample
    if(results$EXC < 1 & results$SAMP <= 10 & results$SAMP == 1){outcome <- 'Review'} # & results$SAMP >1 new 12/21/2020 can't say supporting on 1 sample
    
    
    results <- mutate(results, STAT = outcome)
    names(results) <- c(paste(parameter,names(results)[1], sep = '_'),
                        paste(parameter,names(results)[2], sep = '_'),
                        paste(parameter,names(results)[3], sep = '_'),
                        paste(parameter,names(results)[4], sep = '_'))
    #rename based on parameter entered
    return(results)
  } else {
    if(nrow(parameterDataset) == 0){
      z <- data.frame(EXC = NA, SAMP= nrow(parameterDataset), exceedanceRate= NA, STAT= NA)
      names(z) <- paste(parameter,names(z), sep='_')
      return(z)
    } else {
      z <- data.frame(EXC = NA, SAMP= nrow(parameterDataset), exceedanceRate= NA, STAT= paste(parameter, 'WQS info missing from analysis'))
      names(z) <- paste(parameter,names(z), sep='_')
      return(z)
    }
    
  }
}


StationTableStartingData <- function(x){
  x %>%
    dplyr::select(FDT_STA_ID, ID305B_1:VAHU6) %>%
    rename('STATION_ID' = 'FDT_STA_ID') %>%
    distinct(STATION_ID, .keep_all = T)
}

#stations <- unique(estuarineStations$STATION_ID)#unique(stationData$FDT_STA_ID)
#x <- estuarineStations#stationData
#previousStationTable <-  stationTable
#previousStationTableCycle <- 2020
#previousStationTable2 <- historicalStationsTable2
#previousStationTable2Cycle <- 2018
#rm(previousStationTable); rm(previousStationTable2); rm(previousStationTable2Cycle);rm(previousStationTableCycle); rm(lastComment); rm(lastComment2)

stationTableComments <- function(stations, previousStationTable, 
                                 previousStationTableCycle,
                                 previousStationTable2,
                                 previousStationTable2Cycle){
  lastComment <- filter(previousStationTable, STATION_ID %in% stations) %>%
    dplyr::select(STATION_ID, COMMENTS)
  names(lastComment) <- c('STATION_ID', paste(previousStationTableCycle, 'IR COMMENTS'))
  lastComment2 <- filter(previousStationTable2, STATION_ID %in% stations) %>%
    dplyr::select(STATION_ID, COMMENTS)
  names(lastComment2) <- c('STATION_ID', paste(previousStationTable2Cycle, 'IR COMMENTS'))
  return(left_join(lastComment, lastComment2, by= 'STATION_ID'))
}



# Calculate daily thermocline depth and designate Epilimnion vs Hypolimnion
thermoclineDepth <- function(stationData){
  stationData <- stationData %>%
    mutate(SampleDate = as.Date(FDT_DATE_TIME)) %>%
    group_by(FDT_STA_ID, SampleDate) 
  
  dailyThermDepth <- dplyr::select(stationData, FDT_STA_ID, SampleDate, FDT_DEPTH, FDT_TEMP_CELCIUS) %>%
    mutate(DepthDiff = c(NA, diff(FDT_DEPTH)),
           TempDiff = c(NA, diff(FDT_TEMP_CELCIUS))) %>%
    filter(DepthDiff == 1) # get rid of changes less than 1 meter depth
  # Alt route in case shallow lake
  if(nrow(dailyThermDepth) > 0){
    dailyThermDepth <- filter(dailyThermDepth, TempDiff <= -1)
    # one more catch if no thermocline established
    if(nrow(dailyThermDepth) > 0){
      dailyThermDepth <- summarise(dailyThermDepth, ThermoclineDepth = min(FDT_DEPTH) - 0.5) %>% ungroup() 
    } else {
      dailyThermDepth <- summarise(stationData, ThermoclineDepth = NA) %>% ungroup()  }
  } else {
    dailyThermDepth <- summarise(stationData, ThermoclineDepth = NA) %>% ungroup() }
    
  
  full_join(stationData, dailyThermDepth, by = c('FDT_STA_ID', 'SampleDate')) %>%
    mutate(LakeStratification= ifelse(FDT_DEPTH < ThermoclineDepth,"Epilimnion","Hypolimnion"))%>% ungroup() 
}
# stationData %>% thermoclineDepth()


#Max Temperature Exceedance Function
tempExceedances <- function(x){
  dplyr::select(x,FDT_DATE_TIME, FDT_DEPTH, FDT_TEMP_CELCIUS, LEVEL_FDT_TEMP_CELCIUS, `Max Temperature (C)`)%>% # Just get relevant columns, 
    filter(!(LEVEL_FDT_TEMP_CELCIUS %in% c('Level II', 'Level I'))) %>% # get lower levels out
    filter(!is.na(FDT_TEMP_CELCIUS))%>% #get rid of NA's
    rename(parameter = !!names(.[3]), limit = !!names(.[5])) %>% # rename columns to make functions easier to apply
    # Round to Even Rule
    mutate(parameterRound = signif(parameter, digits = 2), # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section50/
           exceeds = ifelse(parameterRound > limit, T, F)) # Identify where above max Temperature, 
}
# tempExceedances(x) %>%
#  quickStats('TEMP')



# Minimum DO Exceedance function
DOExceedances_Min <- function(x){
  # special step for lake stations, remove samples based on lake assessment guidance 
  if(unique(x$lakeStation) == TRUE){
    if(!is.na(unique(x$Lakes_187B)) & unique(x$Lakes_187B) == 'y'){
      x <- filter(x, LakeStratification %in% c("Epilimnion", NA)) %>% # only use epilimnion or unstratified samples for analysis
        dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, DO_mg_L, LEVEL_DO, `Dissolved Oxygen Min (mg/L)`, LakeStratification) # Just get relevant columns,
    } else {
      x <- dplyr::select(x, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, DO_mg_L, LEVEL_DO, `Dissolved Oxygen Min (mg/L)`, LakeStratification) }# Just get relevant columns,
  } else {
    x <- dplyr::select(x, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, DO_mg_L, LEVEL_DO, `Dissolved Oxygen Min (mg/L)`) # Just get relevant columns, 
  }
  
  x %>%
    filter(!(LEVEL_DO %in% c('Level II', 'Level I'))) %>% # get lower levels out
    filter(!is.na(DO_mg_L)) %>% 
    rename(parameter = !!names(.[4]), limit = !!names(.[6])) %>% # rename columns to make functions easier to apply
    # Round to Even Rule
    mutate(parameterRound = signif(parameter, digits = 2), # two significant figures based on  https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section50/
           exceeds = ifelse(parameterRound < limit, T, F))# Identify where below min DO 
}
#DOExceedances_Min(x) %>% quickStats('DO')


# Daily Average exceedance function
DO_Assessment_DailyAvg <- function(x){ 
  # Don't apply this function to lake stations
  #if(unique(x$lakeStation) == FALSE){
  ### special step for lake stations, remove samples based on lake assessment guidance 
  ###if(!is.na(unique(x$Lakes_187B)) & unique(x$Lakes_187B) == 'y'){
  ###  x <- filter(x, LakeStratification %in% c("Epilimnion", NA)) } # only use epilimnion or unstratified samples for analysis
    
    dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME, FDT_DEPTH,DO_mg_L,LEVEL_DO,`Dissolved Oxygen Min (mg/L)`,`Dissolved Oxygen Daily Avg (mg/L)`)%>% # Just get relevant columns, 
      filter(!(LEVEL_DO %in% c('Level II', 'Level I'))) %>% # get lower levels out
      filter(!is.na(DO_mg_L)) %>% #get rid of NA's
      mutate(date = as.Date(FDT_DATE_TIME, format="%m/%d/%Y"), 
             limit = `Dissolved Oxygen Daily Avg (mg/L)`) %>% 
      group_by(date) %>%
      mutate(n_Samples_Daily = n()) %>% # how many samples per day?
      filter(n_Samples_Daily > 1) %>%
      # Daily average with average rounded to even
      mutate(DO_DailyAverage = signif(mean(DO_mg_L), digits = 2),  # two significant figures based on  https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section50/
             exceeds = ifelse(DO_DailyAverage < `Dissolved Oxygen Daily Avg (mg/L)`,T,F)) %>% 
      ungroup() %>% 
      dplyr::select(-c(FDT_DATE_TIME, `Dissolved Oxygen Min (mg/L)`)) 
}
#DO_Assessment_DailyAvg(x) %>% quickStats(., 'DO_Daily_Avg') 

# pH range Exceedance Function
pHSpecialStandardsCorrection <- function(x){
  z <- filter(x, str_detect(as.character(SPSTDS), '6.5-9.5'))
  if(nrow(z) > 0){
    return(
      mutate(x, `pH Min` = case_when(str_detect(as.character(SPSTDS), '6.5-9.5') ~ 6.5, TRUE ~ `pH Min`),
             `pH Max` = case_when(str_detect(as.character(SPSTDS), '6.5-9.5') ~ 9.5, TRUE ~ `pH Max`)))
    }else{return(x)}
}

pHExceedances <- function(x){
  # special step for lake stations, remove samples based on lake assessment guidance 
  if(unique(x$lakeStation) == TRUE){
    if(!is.na(unique(x$Lakes_187B)) & unique(x$Lakes_187B) == 'y'){
      x <- filter(x, LakeStratification %in% c("Epilimnion", NA)) %>% # only use epilimnion or unstratified samples for analysis
        dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, FDT_FIELD_PH, LEVEL_FDT_FIELD_PH, `pH Min`, `pH Max`, LakeStratification) # Just get relevant columns,
    } else {
      x <- dplyr::select(x, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, FDT_FIELD_PH, LEVEL_FDT_FIELD_PH, `pH Min`, `pH Max`, LakeStratification) }# Just get relevant columns,
  } else {
    x <- dplyr::select(x, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, FDT_FIELD_PH, LEVEL_FDT_FIELD_PH, `pH Min`, `pH Max`) }# Just get relevant columns, 
  
  pH <- filter(x, !(LEVEL_FDT_FIELD_PH %in% c('Level II', 'Level I'))) %>% # get lower levels out
    filter(!is.na(FDT_FIELD_PH)) #get rid of NA's
    
  # only run analysis if WQS exist for station
    if(any(is.na(pH$`pH Min`)) | any(is.na(pH$`pH Max`))){
      pH <- mutate(pH, interval = 1, exceeds = FALSE, limit = `pH Min`) # placeholder to run quickStats() without any WQS
    } else {
      pH <- pH %>%
        rowwise() %>% 
        # Round to Even Rule
        mutate(parameterRound = signif(FDT_FIELD_PH, digits = 2)) %>% # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section50/
        mutate(interval=findInterval(parameterRound,c(`pH Min`,`pH Max`), left.open=TRUE, rightmost.closed = TRUE)) %>% # Identify where pH outside of assessment range with round to even
        ungroup()%>%
        mutate(exceeds=ifelse(interval == 1, F, T), # Highlight where pH doesn't fall into assessment range
               limit = `pH Min`) # placeholder for quickStats function, carries over whether or not station has WQS attributed
    }
  return(pH)
}
#pHExceedances(x) %>% quickStats('PH')



# Consolidate water column metals assessment decisions calculated by Roger Stewart
# Fuction exactly the same as 2020 cycle and may need updates if dataset changes
metalsExceedances <- function(x, metalType){
  # if any data given to function
  if(nrow(x) > 0){ VIO <- length(which(x == 'NSP')) 
  }else {
    VIO <- NA  }
  
  x <- data.frame(VIO = VIO, STAT = ifelse(VIO > 0, 'Review', 'S'))
  names(x) <- paste(metalType,names(x), sep='_')
  return(x)
}


## Identify if further review needs to happen for PCB or metals data 
PCBmetalsDataExists <- function(x, parameterType){
  # if any data given to function
  if(nrow(x) > 0){ 
    x <- data.frame(EXC = NA, STAT = 'Review')
  }else {
    x <- data.frame(EXC = NA, STAT = NA) }
  
  names(x) <- paste(parameterType, names(x), sep='_')
  return(x)
}

# PCBmetalsDataExists(filter(markPCB, str_detect(SampleMedia, 'Water')) %>%
#                       filter(StationID %in% stationData$FDT_STA_ID), 'WAT_TOX')
# PCBmetalsDataExists(filter(fishMetals, Station_ID %in% stationData$FDT_STA_ID), 'FISH_MET')
# PCBmetalsDataExists(filter(fishPCB, `DEQ rivermile` %in%  stationData$FDT_STA_ID), 'FISH_TOX')

#### PWS Assessment Functions ---------------------------------------------------------------------------------------------------
# The app doesn't use this function for modules because you need to be able to toggle assessment on/off with WQS adjustment on the
# fly, but the automated functions need PWS filter programmed in to ease automating over thousands of sites

assessPWS <- function(x, fieldName, commentName, PWSlimit, outputName){
  if(unique(x$PWS) %in% c("Yes")){
    fieldName_ <- enquo(fieldName)
    commentName_ <- enquo(commentName)
    parameterData <- dplyr::select(x, FDT_DATE_TIME, !! fieldName_, !! commentName_) %>%
      filter(!( !! commentName_ %in% c('Level II', 'Level I'))) %>% # get lower levels out
      filter(!is.na(!!fieldName_ )) %>% #get rid of NA's
      mutate(`Parameter Rounded to WQS Format` = signif(!! fieldName_, digits = 2),  # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
             limit =  PWSlimit) %>%
      rename(parameter = !!names(.[4])) %>% # rename columns to make functions easier to apply
      mutate(exceeds = ifelse(parameter > limit, T, F)) # Identify where above WQS limit
    return(quickStats(parameterData, outputName)) #%>% dplyr::select(-ends_with('STAT')))    
  }
  return(quickStats(tibble(limit = NA), outputName))
  }

#assessPWS(x, NITRATE_mg_L, LEVEL_NITRATE, 10, 'PWS_Nitrate')
#assessPWS(x, CHLORIDE_mg_L, LEVEL_CHLORIDE, 250, 'PWS_Chloride')
#assessPWS(x, SULFATE_TOTAL_mg_L, LEVEL_SULFATE_TOTAL, 250, 'PWS_Total_Sulfate')


# Nutrients pseudo-assessment functions (for Riverine applications)

# Count samples
countNutrients <- function(x, fieldName, commentName, nutrientLimit){
  fieldName_ <- enquo(fieldName)
  commentName_ <- enquo(commentName)
  
  dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME, !! fieldName_, !! commentName_)%>% # Just get relevant columns
    filter(!( !! commentName_ %in% c('Level II', 'Level I'))) %>% # get lower levels out
    filter(!is.na(!!fieldName_ )) %>% #get rid of NA's
    rename(parameter = !!names(.[3])) %>% # rename columns to make functions easier to apply
    mutate(limit = nutrientLimit, 
           exceeds = ifelse(parameter > limit, T, F)) # Identify where above WQS limit
}
#countNutrients(x, PHOSPHORUS_mg_L, LEVEL_PHOSPHORUS, 0.2) %>% quickStats('NUT_TP') # but no longer use 0.2 riverine flag after 12/21/2020 email with Tish/Amanda
#countNutrients(x, CHLOROPHYLL_A_ug_L, LEVEL_CHLOROPHYLL_A, NA)  %>% quickStats('NUT_CHLA')



#### Lake Chlorophyll a Assessment Functions ---------------------------------------------------------------------------------------------------

chlA_analysis <- function(x){
  if(!is.na(unique(x$Lakes_187B))){
    if(unique(x$Lakes_187B) == 'y'){
      x <- filter(x, LACUSTRINE == 'Y')  } }
  
  chla <- filter(x, !is.na(CHLOROPHYLL_A_ug_L)) %>%
    filter(FDT_DEPTH <= 1) %>% # Guidance calls for top meter only
    filter(!( LEVEL_CHLOROPHYLL_A %in% c('Level II', 'Level I'))) %>% # get lower levels out
    dplyr::select(FDT_STA_ID, FDT_DEPTH, FDT_DATE_TIME, SampleDate, CHLOROPHYLL_A_ug_L, `Chlorophyll a (ug/L)`, LACUSTRINE)%>%
    mutate(Year= year(FDT_DATE_TIME), Month=month(FDT_DATE_TIME)) %>%
    filter(Month %in% c(4, 5, 6, 7, 8, 9, 10)) # make sure only assess valid sample months
  if(length(unique(chla$FDT_STA_ID)) > 1){
    chlaResults <- chla %>%
      group_by(Month, Year) %>%
      summarise(samplesPerMonth = n(),
             medianCHLOROPHYLL_A_ug_L = median(CHLOROPHYLL_A_ug_L, na.rm = T),
             `Chlorophyll a (ug/L)` = unique(`Chlorophyll a (ug/L)`)) %>%
      ungroup() %>%
      group_by(Year) %>%
      summarise(samplesPerYear = n(),
                pct90 = quantile(medianCHLOROPHYLL_A_ug_L, 0.9),
                `Chlorophyll a (ug/L)` = unique(`Chlorophyll a (ug/L)`)) %>%
      mutate(`90th Percentile Rounded to WQS Format` = signif(pct90, digits = 2),  # two significant figures based on WQS https://lis.virginia.gov/cgi-bin/legp604.exe?000+reg+9VAC25-260-187&000+reg+9VAC25-260-187
             chlA_Exceedance = ifelse(`90th Percentile Rounded to WQS Format` > `Chlorophyll a (ug/L)`, T, F),
             ID305B = unique(x$ID305B_1))  %>%
      dplyr::select(ID305B, Year, samplesPerYear, pct90, `90th Percentile Rounded to WQS Format`, everything())
  } else {
    chlaResults <- chla %>%
      group_by(Year) %>%
      mutate(samplesPerYear = n(),
             pct90 = quantile(CHLOROPHYLL_A_ug_L, 0.9),
             `90th Percentile Rounded to WQS Format` = signif(pct90, digits = 2),  # two significant figures based on WQS https://lis.virginia.gov/cgi-bin/legp604.exe?000+reg+9VAC25-260-187&000+reg+9VAC25-260-187
             chlA_Exceedance = ifelse(`90th Percentile Rounded to WQS Format` > `Chlorophyll a (ug/L)`, T, F)) %>%
      dplyr::select(FDT_STA_ID, Year, samplesPerYear, pct90, `90th Percentile Rounded to WQS Format`,`Chlorophyll a (ug/L)`, chlA_Exceedance, LACUSTRINE) %>%
      distinct(Year, .keep_all=T)
  }
  return(chlaResults)
}

#chlA_analysis(stationData1)

chlA_Assessment <- function(x){
  chlA_Results <- chlA_analysis(x) %>% ungroup()
  
  if(nrow(chlA_Results) > 0){
    if(is.na(unique(chlA_Results$`Chlorophyll a (ug/L)`))){ # bail out if nutrient standards didn't join properly
      return(tibble(NUT_CHLA_EXC= NA, NUT_CHLA_SAMP = NA,	NUT_CHLA_STAT = NA))}
    validYears <- filter(chlA_Results, samplesPerYear >= 6) # need at least 6 samples per year
    mostRecent2years <- slice_max(validYears, Year, n = 2) # get most recent two years of results
    if(nrow(mostRecent2years) == 2){ 
      if(all(unique(mostRecent2years$chlA_Exceedance) == FALSE)){ # no exceedances in last two years
        return(tibble(NUT_CHLA_EXC= 0, NUT_CHLA_SAMP = nrow(mostRecent2years),	NUT_CHLA_STAT = 'S') )
      } else { # at least one chlA_Exceedance exists
        if(all(unique(mostRecent2years$chlA_Exceedance)) == TRUE){ # both years exceed
          return(tibble(NUT_CHLA_EXC= nrow(mostRecent2years), NUT_CHLA_SAMP = nrow(mostRecent2years),	NUT_CHLA_STAT = 'IM'))
        } else { # run a tiebreak with third most recent year
          mostRecent3years <- slice_max(validYears, Year, n = 3) # get most recent three years of results
          mostRecent3yearsExceed <- filter(mostRecent3years, chlA_Exceedance == TRUE)
          if(nrow(mostRecent3yearsExceed) >= 2){
            return(tibble(NUT_CHLA_EXC= nrow(mostRecent3yearsExceed), NUT_CHLA_SAMP = nrow(mostRecent3years),	NUT_CHLA_STAT = 'IM'))
          } else {
            return(tibble(NUT_CHLA_EXC= nrow(mostRecent3yearsExceed), NUT_CHLA_SAMP = nrow(mostRecent3years),	NUT_CHLA_STAT = 'Review')) }
        }}} else {return(tibble(NUT_CHLA_EXC= NA, NUT_CHLA_SAMP = nrow(validYears),	NUT_CHLA_STAT = 'IN') ) }
    } else {    return(tibble(NUT_CHLA_EXC= NA, NUT_CHLA_SAMP = NA,	NUT_CHLA_STAT = NA) )  }
}

#chlA_Assessment(stationData1)
#chlA_Assessment(AUData1)



#### Lake Total Phosphorus Assessment Functions ---------------------------------------------------------------------------------------------------

TP_analysis <- function(x){
  if(!is.na(unique(x$Lakes_187B))){
    if(unique(x$Lakes_187B) == 'y'){
      x <- filter(x, LACUSTRINE == 'Y')  }
  }
  
  
  TP <- filter(x, !is.na(PHOSPHORUS_mg_L)) %>%
    filter(FDT_DEPTH <= 1) %>% # Guidance calls for top meter only
    filter(!( LEVEL_PHOSPHORUS %in% c('Level II', 'Level I'))) %>% # get lower levels out
    dplyr::select(FDT_STA_ID, FDT_DEPTH, FDT_DATE_TIME, SampleDate, PHOSPHORUS_mg_L, `Total Phosphorus (ug/L)`, LACUSTRINE)%>%
    mutate(Year= year(FDT_DATE_TIME), Month=month(FDT_DATE_TIME)) %>%
    filter(Month %in% c(4, 5, 6, 7, 8, 9, 10)) # make sure only assess valid sample months
  if(length(unique(TP$FDT_STA_ID)) > 1){
    TPResults <- TP %>%
      group_by(Month, Year) %>%
      summarise(samplesPerMonth = n(),
                medianPHOSPHORUS_mg_L = median(PHOSPHORUS_mg_L, na.rm = T),
                `Total Phosphorus (ug/L)` = unique(`Total Phosphorus (ug/L)`)) %>%
      ungroup() %>%
      group_by(Year) %>%
      summarise(samplesPerYear = n(),
                `Annual Median TP` = median(medianPHOSPHORUS_mg_L, na.rm = TRUE),
                `Total Phosphorus (ug/L)` = unique(`Total Phosphorus (ug/L)`)) %>%
      mutate(`Annual Median TP Rounded to WQS Format` = signif(`Annual Median TP`, digits = 1),  # one significant figures based on WQS https://lis.virginia.gov/cgi-bin/legp604.exe?000+reg+9VAC25-260-187&000+reg+9VAC25-260-187
             TP_Exceedance = ifelse(`Annual Median TP Rounded to WQS Format` > `Total Phosphorus (ug/L)`, T, F),
             ID305B = unique(x$ID305B_1))  %>%
      dplyr::select(ID305B, Year, samplesPerYear, `Annual Median TP`, `Annual Median TP Rounded to WQS Format`, everything())
  } else {
    TPResults <- TP %>%
      group_by(Year) %>%
      mutate(samplesPerYear = n(),
             `Annual Median TP` = median(PHOSPHORUS_mg_L, na.rm = TRUE),
             `Annual Median TP Rounded to WQS Format` = signif(`Annual Median TP`, digits = 1),  # one significant figures based on WQS https://lis.virginia.gov/cgi-bin/legp604.exe?000+reg+9VAC25-260-187&000+reg+9VAC25-260-187
             TP_Exceedance = ifelse(`Annual Median TP Rounded to WQS Format` > `Total Phosphorus (ug/L)`, T, F)) %>%
      dplyr::select(FDT_STA_ID, Year, samplesPerYear, `Annual Median TP`, `Annual Median TP Rounded to WQS Format`,`Total Phosphorus (ug/L)`, TP_Exceedance, LACUSTRINE) %>%
      distinct(Year, .keep_all=T)
  }
  return(TPResults)
}
#TP_analysis(stationData1)
#TP_analysis(AUData11)

TP_Assessment <- function(x){
  TP_Results <- TP_analysis(x) %>% ungroup()
  
  if(nrow(TP_Results) > 0){
    if(is.na(unique(TP_Results$`Total Phosphorus (ug/L)`))){ # bail out if nutrient standards didn't join properly
      return(tibble(NUT_TP_EXC= NA, NUT_TP_SAMP = NA,	NUT_TP_STAT = NA))}
    validYears <- filter(TP_Results, samplesPerYear >= 6) # need at least 6 samples per year
    mostRecent2years <- slice_max(validYears, Year, n = 2) # get most recent two years of results
    if(nrow(mostRecent2years) == 2){ 
      if(all(unique(mostRecent2years$TP_Exceedance)) == FALSE){ # no exceedances in last two years
        return(tibble(NUT_TP_EXC= 0, NUT_TP_SAMP = nrow(mostRecent2years),	NUT_TP_STAT = 'S') )
      } else { # at least one TP_Exceedance exists
        if(all(unique(mostRecent2years$TP_Exceedance)) == TRUE){ # both years exceed
          return(tibble(NUT_TP_EXC= nrow(mostRecent2years), NUT_TP_SAMP = nrow(mostRecent2years),	NUT_TP_STAT = 'IM'))
        } else { # run a tiebreak with third most recent year
          mostRecent3years <- slice_max(validYears, Year, n = 3) %>% # get most recent three years of results
            filter(TP_Exceedance == TRUE)
          if(nrow(mostRecent3years) >= 2){
            return(tibble(NUT_TP_EXC= nrow(mostRecent3years), NUT_TP_SAMP = nrow(mostRecent3years),	NUT_TP_STAT = 'IM'))
          } else {
            return(tibble(NUT_TP_EXC= nrow(mostRecent3years), NUT_TP_SAMP = nrow(mostRecent3years),	NUT_TP_STAT = 'Review')) }
        }}} else {return(tibble(NUT_TP_EXC= NA, NUT_TP_SAMP = nrow(validYears),	NUT_TP_STAT = 'IN') ) }
  } else {    return(tibble(NUT_TP_EXC= NA, NUT_TP_SAMP = NA,	NUT_TP_STAT = NA) )  }
}

#TP_Assessment(stationData1)
#TP_Assessment(AUData11)



### TSI equations
TSIcalculation <- function(x){
  if(unique(x$lakeStation) == TRUE){
    if(is.na(unique(x$Lakes_187B))){
      # first fill down secchi depth in case it isn't stored exactly at 0.3 meter
      secchiFix <- x %>%
        group_by(FDT_STA_ID, FDT_DATE_TIME) %>% 
        fill(SECCHI_DEPTH_M, .direction = "downup") %>%
        filter(FDT_DEPTH <= 0.3) %>%
        # remove all data except those from mid June through mid September (going with June 15 and Sept 15 since guidance does not specify)
        mutate(monthday = as.numeric(paste0(month(FDT_DATE_TIME), day(FDT_DATE_TIME)))) %>%
        filter(between(monthday, 615, 915 )) %>%
        dplyr::select(-monthday)
      
      # Calculate Secchi depth TSI
      SDdata <- filter(secchiFix, !is.na(SECCHI_DEPTH_M) ) %>%
          filter(!( LEVEL_SECCHI_DEPTH %in% c('Level II', 'Level I'))) %>% # get lower levels out
          dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH,SECCHI_DEPTH_M) %>%
        mutate(TSI_SD = 10*(6 - (log(SECCHI_DEPTH_M) / log(2))) )
      SD <- suppressWarnings(suppressMessages(
        SDdata %>%
          group_by(FDT_STA_ID) %>%
          summarise(meanSD = mean(SECCHI_DEPTH_M, na.rm = T), # take average of all secchi depths first
                    TSI_SD = 10*(6 - (log(meanSD) / log(2))) ) ))# log() is natural log in R
      
      # Calculate Chlorophyll a TSI
      chlaData <- filter(secchiFix, !is.na(CHLOROPHYLL_A_ug_L) ) %>%
          filter(!( LEVEL_CHLOROPHYLL_A %in% c('Level II', 'Level I'))) %>% # get lower levels out
          dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, CHLOROPHYLL_A_ug_L) %>%
        mutate(TSI_chla = 10*(6 - (2.04 - 0.68 * (log( CHLOROPHYLL_A_ug_L))) / log(2))) 
      chla <- suppressWarnings(suppressMessages(
        chlaData %>% 
          group_by(FDT_STA_ID) %>%
          summarise(meanchla = mean(CHLOROPHYLL_A_ug_L, na.rm = T), # take average of all chl a first
                    TSI_chla = 10*(6 - (2.04 - 0.68 * (log(meanchla))) / log(2)))  ))# log() is natural log in R
      
      # Calculate Total Phosphorus TSI
      TPdata <- filter(secchiFix, !is.na(PHOSPHORUS_mg_L) ) %>%
          filter(!( LEVEL_PHOSPHORUS %in% c('Level II', 'Level I'))) %>% # get lower levels out
          dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, PHOSPHORUS_mg_L) %>%
          mutate(PHOSPHORUS_ug_L = PHOSPHORUS_mg_L * 1000,# first convert mg/L to ug/L
                 TSI_TP = 10*(6 - (log( (48 / PHOSPHORUS_ug_L )) / log(2))) )   
      TP <- suppressWarnings(suppressMessages(
        TPdata %>%
          group_by(FDT_STA_ID) %>%
          summarise(meanTP = mean(PHOSPHORUS_ug_L, na.rm = T), # take average of all TP first
                    TSI_TP = 10*(6 - (log( (48 / meanTP)) / log(2))) ) ))# log() is natural log in R
      
      TSI <- full_join(SD, chla, by = c('FDT_STA_ID')) %>%
        full_join(TP, by = c('FDT_STA_ID')) %>%
        bind_cols(tibble(associatedData = list(full_join(SDdata, chlaData, by = c('FDT_STA_ID', 'FDT_DATE_TIME', 'FDT_DEPTH')) %>%
                                                 full_join(TPdata, by = c('FDT_STA_ID', 'FDT_DATE_TIME', 'FDT_DEPTH'))) ))
      
      return(TSI)
      
    } else {return(tibble(FDT_STA_ID = NA, meanSD = NA, TSI_SD = NA, meanchla = NA, TSI_chla = NA, meanTP = NA, TSI_TP = NA, associatedData = list(NA)))}
  } else {return(tibble(FDT_STA_ID = NA, meanSD = NA, TSI_SD = NA, meanchla = NA, TSI_chla = NA, meanTP = NA, TSI_TP = NA, associatedData = list(NA)))} 
}

#TSIcalculation(stationData1)

#x <- stationData1 %>%
#  bind_rows(mutate(stationData1, FDT_STA_ID = 'FAKE'))

TSIassessment <- function(x){
  TSI <- TSIcalculation(x)
  
  if(nrow(TSI) > 0){
    if(nrow(TSI) > 1){ # if more than one station in AU, first average TSI results then assess
      return(
        TSI %>% # using mean to be consistent with mean in TSIcalculation()
          summarise(TSI_SD = as.numeric(signif(mean(TSI_SD, na.rm = T),digits = 2)), 
                    TSI_chla = as.numeric(signif(mean(TSI_chla, na.rm = T), digits = 2)),
                    TSI_TP = as.numeric(signif(mean(TSI_TP, na.rm = T), digits = 2))) %>%
          mutate(ID305B = unique(x$ID305B_1)) %>%
          dplyr::select(ID305B, everything()))
    } else {
      return(TSI %>%
               mutate(ID305B = unique(x$ID305B_1),
                      TSI_SD = as.numeric(signif(TSI_SD, digits = 2)),
                      TSI_chla = as.numeric(signif(TSI_chla, digits = 2)), 
                      TSI_TP = as.numeric(signif(TSI_TP, digits = 2))) %>%
               dplyr::select(ID305B,TSI_SD, TSI_chla, TSI_TP))}
  } else {return(tibble(ID305B = NA, TSI_SD = NA, TSI_chla = NA, TSI_TP = NA))}
}
#TSIassessment(x)




# Metals exceedances

metalsExceedances <- function(x, metalType){
  # if any data given to function
  if(nrow(x) > 0){ EXC <- length(which(x == 'NSP' | x == 'OE')) 
  }else { EXC <- NA  }
  
  x <- tibble(EXC = EXC, STAT = ifelse(EXC > 0, 'Review', 'S'))
  names(x) <- paste(metalType,names(x), sep='_')
  return(x)
}


# Benthic Data flag
benthicAssessment <- function(x, VSCIresults){
  # this works because the all SCI options are run on all data, so if there is a VSCI result 
  # (even if in real life that is not the correct SCI to use), then benthic data exists for
  # a given station
  z <- filter(VSCIresults, StationID %in% unique(x$FDT_STA_ID))
  if(nrow(z) > 0){tibble(BENTHIC_STAT='Review')
  } else { tibble(BENTHIC_STAT=NA)}
}
#benthicAssessment(x, VSCIresults)



#### Ammonia Assessment Functions ---------------------------------------------------------------------------------------------------

# Used rolling windows but opted for loops with filtering instead of roll_apply over time series so teh data analyzed each window could
#  cascade outside the function and be unpacked by further analyses/visualizations if necessary

# Four day average analysis function

fourDayAverageAnalysis <- function(chronicWindowData, chronicWindowResults){
  fourDayResults <- tibble(`fourDayAmmoniaAvg` = as.numeric(NA),
                           WindowStart = as.POSIXct(NA),
                           `fourDayAvglimit` = as.numeric(NA),
                           fourDayExceedance = as.logical(NA),
                           fourDayWindowData = list())
  for(k in 1:nrow(chronicWindowData)){
    fourDayWindow <- filter(chronicWindowData, between(FDT_DATE_TIME, chronicWindowData$FDT_DATE_TIME[k], chronicWindowData$FDT_DATE_TIME[k] + days(4) ) )
    if(nrow(fourDayWindow) > 1){
      fourDayResultsi <- fourDayWindow %>%
        summarize(WindowStart = min(FDT_DATE_TIME),
                  `fourDayAmmoniaAvg` = as.numeric(signif(mean(AMMONIA_mg_L, na.rm = T), digits = 2))) %>% # two sigfigs for comparison to chronic criteria
        bind_cols(dplyr::select(chronicWindowResults,`fourDayAvglimit`)) %>%
        mutate(fourDayExceedance = `fourDayAmmoniaAvg` > `fourDayAvglimit`)
      fourDayResults <- bind_rows(fourDayResults, 
                                  fourDayResultsi %>% bind_cols(tibble(fourDayWindowData = list(fourDayWindow))) )
    } else {
      fourDayResults <- bind_rows(fourDayResults, 
                                  tibble(`fourDayAmmoniaAvg` = as.numeric(NA),
                                         WindowStart = fourDayWindow$FDT_DATE_TIME,
                                         `fourDayAvglimit` = as.numeric(NA),
                                         fourDayExceedance = as.logical(NA),
                                         fourDayWindowData = list(NA)) )    }
  }
  fourDayResults <- filter(fourDayResults, !is.na(fourDayAmmoniaAvg))
  return(fourDayResults)
}
#fourDayAverageAnalysis(chronicWindowData, chronicWindowResultsi)


# Calculate limits and return dataframe with original data and limits 9VAC25-260-155 https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section155/
freshwaterNH3limit <- function(x, # dataframe with station data
                               trout, # T/F condition
                               mussels,# T/F condition
                               earlyLife# T/F condition
){
  # If no data, return nothing
  if(nrow(x)==0){return(NULL)}
  
  # remove any data that shouldn't be considered
  x <- filter(x, !(LEVEL_FDT_TEMP_CELCIUS %in% c('Level II', 'Level I')) |
                !(LEVEL_FDT_FIELD_PH %in% c('Level II', 'Level I'))) %>% # get lower levels out
    # lake stations should only be surface sample
    {if(unique(x$lakeStation) == TRUE)
      filter(., FDT_DEPTH <= 1)
      else . } %>%
    filter(!is.na(AMMONIA_mg_L)) %>% #get rid of NA's
    dplyr::select(FDT_DATE_TIME, FDT_DEPTH, FDT_TEMP_CELCIUS, FDT_FIELD_PH, AMMONIA_mg_L)
  # If no data, return nothing
  if(nrow(x)==0){return(NULL)}
  
  # Trout & mussels present scenario
  if(trout == TRUE & mussels == TRUE){
    # Acute Criteria
    acute <- x %>%
      rowwise() %>%
      mutate(ammoniaRound = as.numeric(signif(AMMONIA_mg_L, digits = 2)),
             acuteNH3limit = as.numeric(signif(
               min(((0.275 / (1 + 10^(7.204 - FDT_FIELD_PH))) + (39.0 / (1 + 10^(FDT_FIELD_PH - 7.204)))),
                   (0.7249 * ( (0.0114 / (1 + 10^(7.204 - FDT_FIELD_PH))) + (1.6181 / (1 + 10^(FDT_FIELD_PH - 7.204)))) * (23.12 * 10^(0.036 * (20 - FDT_TEMP_CELCIUS))) )), digits = 2)),
             acuteExceedance = ammoniaRound > acuteNH3limit) 
    # Chronic is calculated on 30 day windows, so we need to average temperature and pH within each 30 day window before we can calculate a
    #  chronic criteria. The chronic criteria will be associated with each sample date that starts a 30 day period, but it applies to all 
    #  samples within the 30 day window. All raw data associated with each window is saved as a listcolumn for later review. 
    chronicWindowResults <- tibble()
    for( i in 1 : nrow(acute)){
      # Calculate window average measurements for chronic criteria
      chronicWindowData <- filter(acute, between(FDT_DATE_TIME, acute$FDT_DATE_TIME[i], acute$FDT_DATE_TIME[i] + days(30) ) ) %>% 
        ungroup() 
      if(nrow(chronicWindowData) > 1){ # need 2 or more data points to run a chronic or 4 day 
        chronicWindowResultsi <- chronicWindowData %>%
          summarise(WindowStart = min(FDT_DATE_TIME),
                    `30dayAmmoniaAvg` = as.numeric(signif(mean(AMMONIA_mg_L, na.rm = T), digits = 2)), # round to even for comparison to chronic criteria
                    TempAvg = mean(FDT_TEMP_CELCIUS, na.rm = T), #don't round to even bc more calculations to follow with data
                    pHAvg = mean(FDT_FIELD_PH, na.rm = T)) %>% #don't round to even bc more calculations to follow with data
          # Chronic Criteria mussels == T & earlyLife == T
          {if(earlyLife == TRUE)
            mutate(., chronicNH3limit = as.numeric(signif(
              0.8876 * ((0.0278 / (1 + 10^(7.688 - pHAvg))) + (1.1994 / (1 + 10^(pHAvg - 7.688)))) * (2.126 * 10^(0.028 * (20 - max(7, TempAvg)))), digits = 2)),
              `fourDayAvglimit`= as.numeric(signif(chronicNH3limit * 2.5, digits = 2)) )
            else mutate(., chronicNH3limit = as.numeric(NA),
                        `fourDayAvglimit`= as.numeric(NA)) } %>%
          # Identify if window Ammonia average is above chronic criteria
          mutate(chronicExceedance = `30dayAmmoniaAvg` > chronicNH3limit) %>%
          # attach associated raw data to analysis for later use
          bind_cols(tibble(associatedWindowData = list(chronicWindowData)))
        
        # 4 day average analysis
        fourDayResults <- fourDayAverageAnalysis(chronicWindowData, chronicWindowResultsi)
        
        chronicWindowResults <- bind_rows(chronicWindowResults, 
                                          left_join(chronicWindowResultsi, fourDayResults, by = c('WindowStart', 'fourDayAvglimit'))  ) 
      } else {
        chronicWindowResults <- bind_rows(chronicWindowResults, 
                                          tibble(
                                            WindowStart = min(chronicWindowData$FDT_DATE_TIME), `30dayAmmoniaAvg` = NA,
                                            TempAvg = NA, pHAvg = NA, chronicNH3limit = NA, fourDayAvglimit = NA, chronicExceedance = NA, 
                                            associatedWindowData = list(NULL), fourDayAmmoniaAvg = NA, fourDayExceedance = NA, fourDayWindowData = list(NULL) ) )
      } }
    return(left_join(acute, chronicWindowResults, by = c("FDT_DATE_TIME" = "WindowStart")) )  }
  
  # Trout present & mussels absent scenario
  if(trout == TRUE & mussels == FALSE){
    # Acute Criteria
    acute <- x %>%
      rowwise() %>%
      mutate(ammoniaRound = as.numeric(signif(AMMONIA_mg_L, digits = 2)),
             acuteNH3limit = as.numeric(signif(
               min(((0.275 / (1 + 10^(7.204 - FDT_FIELD_PH))) + (39.0 / (1 + 10^(FDT_FIELD_PH - 7.204)))),
                   (0.7249 * ( (0.0114 / (1 + 10^(7.204 - FDT_FIELD_PH))) + (1.6181 / (1 + 10^(FDT_FIELD_PH - 7.204)))) * (62.15 * 10^(0.036 * (20 - FDT_TEMP_CELCIUS))) )), digits = 2)),
             acuteExceedance = ammoniaRound > acuteNH3limit) 
    # Chronic is calculated on 30 day windows, so we need to average temperature and pH within each 30 day window before we can calculate a
    #  chronic criteria. The chronic criteria will be associated with each sample date that starts a 30 day period, but it applies to all 
    #  samples within the 30 day window
    chronicWindowResults <- tibble()
    for( i in 1 : nrow(acute)){
      # Calculate window average measurements for chronic criteria
      chronicWindowData <- filter(acute, between(FDT_DATE_TIME, acute$FDT_DATE_TIME[i], acute$FDT_DATE_TIME[i] + days(30) ) ) %>% 
        ungroup() 
      if(nrow(chronicWindowData) > 1){ # need 2 or more data points to run a chronic or 4 day 
        chronicWindowResultsi <- chronicWindowData %>%
          summarise(WindowStart = min(FDT_DATE_TIME),
                    `30dayAmmoniaAvg` = as.numeric(signif(mean(AMMONIA_mg_L, na.rm = T), digits = 2)), # round to even for comparison to chronic criteria
                    TempAvg = mean(FDT_TEMP_CELCIUS, na.rm = T), #don't round to even bc more calculations to follow with data
                    pHAvg = mean(FDT_FIELD_PH, na.rm = T)) %>% #don't round to even bc more calculations to follow with data
          # Chronic Criteria mussels == F & earlyLife == T
          {if(earlyLife == TRUE)
            mutate(., chronicNH3limit = as.numeric(signif(0.9405 * ((0.0278 / (1 + 10^(7.688 - pHAvg))) + (1.1994 / (1 + 10^(pHAvg - 7.688)))) * min(6.92, (7.547 * 10^(0.028 * (20 - TempAvg)))), digits = 2)),
                   `fourDayAvglimit`= as.numeric(signif(chronicNH3limit * 2.5, digits = 2)) )
            # Chronic Criteria mussels == F & earlyLife == F
            else mutate(., chronicNH3limit =  as.numeric(signif(0.9405 * ((0.0278 / (1 + 10^(7.688 - pHAvg))) + (1.1994 / (1 + 10^(pHAvg - 7.688)))) * (7.547 * 10^(0.028 * (20 - max(TempAvg, 7)))), digits = 2)),
                        `fourDayAvglimit`= as.numeric(signif(chronicNH3limit * 2.5, digits = 2)) )  } %>%
          # Identify if window Ammonia average is above chronic criteria
          mutate(chronicExceedance = `30dayAmmoniaAvg` > chronicNH3limit) %>%
          # attach associated raw data to analysis for later use
          bind_cols(tibble(associatedWindowData = list(chronicWindowData)))
        
        # 4 day average analysis
        fourDayResults <- fourDayAverageAnalysis(chronicWindowData, chronicWindowResultsi)
        
        chronicWindowResults <- bind_rows(chronicWindowResults, 
                                          left_join(chronicWindowResultsi, fourDayResults, by = c('WindowStart', 'fourDayAvglimit'))  ) 
      } else {
        chronicWindowResults <- bind_rows(chronicWindowResults, 
                                          tibble(
                                            WindowStart = min(chronicWindowData$FDT_DATE_TIME), `30dayAmmoniaAvg` = NA,
                                            TempAvg = NA, pHAvg = NA, chronicNH3limit = NA, fourDayAvglimit = NA, chronicExceedance = NA, 
                                            associatedWindowData = list(NULL), fourDayAmmoniaAvg = NA, fourDayExceedance = NA, fourDayWindowData = list(NULL) ) )
      } }
    
    return(left_join(acute, chronicWindowResults, by = c("FDT_DATE_TIME" = "WindowStart") ) ) }
  
  # Trout absent & mussels present scenario
  if(trout == FALSE & mussels == TRUE){
    # Acute Criteria
    acute <- x %>%
      rowwise() %>%
      mutate(ammoniaRound = as.numeric(signif(AMMONIA_mg_L, digits = 2)),
             acuteNH3limit = as.numeric(signif(0.7249 * ((0.0114 / (1 + 10^(7.204 - FDT_FIELD_PH))) + (1.6181 / (1 + 10^(FDT_FIELD_PH - 7.204)))) * min(51.93, (23.12 * 10^(0.036 * (20 - FDT_TEMP_CELCIUS)))), digits = 2)),
             acuteExceedance = ammoniaRound > acuteNH3limit) 
    # Chronic is calculated on 30 day windows, so we need to average temperature and pH within each 30 day window before we can calculate a
    #  chronic criteria. The chronic criteria will be associated with each sample date that starts a 30 day period, but it applies to all 
    #  samples within the 30 day window
    chronicWindowResults <- tibble()
    for( i in 1 : nrow(acute)){
      # Calculate window average measurements for chronic criteria
      chronicWindowData <- filter(acute, between(FDT_DATE_TIME, acute$FDT_DATE_TIME[i], acute$FDT_DATE_TIME[i] + days(30) ) ) %>% 
        ungroup()
      if(nrow(chronicWindowData) > 1){ # need 2 or more data points to run a chronic or 4 day 
        chronicWindowResultsi <- chronicWindowData %>%
          summarise(WindowStart = min(FDT_DATE_TIME),
                    `30dayAmmoniaAvg` = as.numeric(signif(mean(AMMONIA_mg_L, na.rm = T), digits = 2)), # round to even for comparison to chronic criteria
                    TempAvg = mean(FDT_TEMP_CELCIUS, na.rm = T), #don't round to even bc more calculations to follow with data
                    pHAvg = mean(FDT_FIELD_PH, na.rm = T)) %>% #don't round to even bc more calculations to follow with data
          # Chronic Criteria mussels == T & earlyLife == T
          {if(earlyLife == TRUE)
            mutate(., chronicNH3limit = as.numeric(signif(0.8876 * ((0.0278 / (1 + 10^(7.688 - pHAvg))) + (1.1994 / (1 + 10^(pHAvg - 7.688)))) * (2.126 * 10^(0.028 * (20 - max(7, TempAvg)))), digits = 2)),
                   `fourDayAvglimit`= as.numeric(signif(chronicNH3limit * 2.5, digits = 2)))
            else mutate(., chronicNH3limit = as.numeric(NA),
                        `fourDayAvglimit`= as.numeric(NA) ) }  %>%
          # Identify if window Ammonia average is above chronic criteria
          mutate(chronicExceedance = `30dayAmmoniaAvg` > chronicNH3limit) %>%
          # attach associated raw data to analysis for later use
          bind_cols(tibble(associatedWindowData = list(chronicWindowData)))
        
        # 4 day average analysis
        fourDayResults <- fourDayAverageAnalysis(chronicWindowData, chronicWindowResultsi)
        
        chronicWindowResults <- bind_rows(chronicWindowResults, 
                                          left_join(chronicWindowResultsi, fourDayResults, by = c('WindowStart', 'fourDayAvglimit'))  )  
      } else {
        chronicWindowResults <- bind_rows(chronicWindowResults, 
                                          tibble(
                                            WindowStart = min(chronicWindowData$FDT_DATE_TIME), `30dayAmmoniaAvg` = NA,
                                            TempAvg = NA, pHAvg = NA, chronicNH3limit = NA, fourDayAvglimit = NA, chronicExceedance = NA, 
                                            associatedWindowData = list(NULL), fourDayAmmoniaAvg = NA, fourDayExceedance = NA, fourDayWindowData = list(NULL) ) )
      } }
    
    return(left_join(acute, chronicWindowResults, by = c("FDT_DATE_TIME" = "WindowStart")) )  }
  
  
  
  
  # Trout & mussels absent scenario
  if(trout == FALSE & mussels == FALSE){
    # Acute Criteria
    acute <- x %>%
      rowwise() %>%
      mutate(ammoniaRound = as.numeric(signif(AMMONIA_mg_L, digits = 2)),
             acuteNH3limit = as.numeric(signif(0.7249 * ((0.0114 / (1 + 10^(7.204 - FDT_FIELD_PH))) + (1.6181 / (1 + 10^(FDT_FIELD_PH - 7.204)))) * min(51.93, (62.15 * 10^(0.036 * (20 - FDT_TEMP_CELCIUS)))), digits = 2)),
             acuteExceedance = ammoniaRound > acuteNH3limit)  
    # Chronic is calculated on 30 day windows, so we need to average temperature and pH within each 30 day window before we can calculate a
    #  chronic criteria. The chronic criteria will be associated with each sample date that starts a 30 day period, but it applies to all 
    #  samples within the 30 day window
    chronicWindowResults <- tibble()
    for( i in 1 : nrow(acute)){
      # Calculate window average measurements for chronic criteria
      chronicWindowData <- filter(acute, between(FDT_DATE_TIME, acute$FDT_DATE_TIME[i], acute$FDT_DATE_TIME[i] + days(30) ) ) %>% 
        ungroup() 
      if(nrow(chronicWindowData) > 1){ # need 2 or more data points to run a chronic or 4 day 
        chronicWindowResultsi <- chronicWindowData %>%
          summarise(WindowStart = min(FDT_DATE_TIME),
                    `30dayAmmoniaAvg` = as.numeric(signif(mean(AMMONIA_mg_L, na.rm = T), digits = 2)), # round to even for comparison to chronic criteria
                    TempAvg = mean(FDT_TEMP_CELCIUS, na.rm = T), #don't round to even bc more calculations to follow with data
                    pHAvg = mean(FDT_FIELD_PH, na.rm = T)) %>% #don't round to even bc more calculations to follow with data
          # Chronic Criteria mussels == F & earlyLife == T
          {if(earlyLife == TRUE)
            mutate(., chronicNH3limit = as.numeric(signif(0.9405 * ((0.0278 / (1 + 10^(7.688 - pHAvg))) + (1.1994 / (1 + 10^(pHAvg - 7.688)))) * min(6.92, (7.547 * 10^(0.028 * (20 - TempAvg)))), digits = 2)),
                   `fourDayAvglimit`= as.numeric(signif(chronicNH3limit * 2.5, digits = 2)) )
            # Chronic Criteria mussels == F & earlyLife == F
            else mutate(., chronicNH3limit =  as.numeric(signif(0.9405 * ((0.0278 / (1 + 10^(7.688 - pHAvg))) + (1.1994 / (1 + 10^(pHAvg - 7.688)))) * (7.547 * 10^(0.028 * (20 - max(TempAvg, 7)))), digits = 2)),
                        `fourDayAvglimit`= as.numeric(signif(chronicNH3limit * 2.5, digits = 2)) )  }  %>%
          # Identify if window Ammonia average is above chronic criteria
          mutate(chronicExceedance = `30dayAmmoniaAvg` > chronicNH3limit) %>%
          # attach associated raw data to analysis for later use
          bind_cols(tibble(associatedWindowData = list(chronicWindowData)))
        
        # 4 day average analysis
        fourDayResults <- fourDayAverageAnalysis(chronicWindowData, chronicWindowResultsi)
        
        chronicWindowResults <- bind_rows(chronicWindowResults, 
                                          left_join(chronicWindowResultsi, fourDayResults, by = c('WindowStart', 'fourDayAvglimit'))  ) 
      } else {
        chronicWindowResults <- bind_rows(chronicWindowResults, 
                                          tibble(
                                            WindowStart = min(chronicWindowData$FDT_DATE_TIME), `30dayAmmoniaAvg` = NA,
                                            TempAvg = NA, pHAvg = NA, chronicNH3limit = NA, fourDayAvglimit = NA, chronicExceedance = NA, 
                                            associatedWindowData = list(NULL), fourDayAmmoniaAvg = NA, fourDayExceedance = NA, fourDayWindowData = list(NULL) ) )
      } }
    return(left_join(acute, chronicWindowResults, by = c("FDT_DATE_TIME" = "WindowStart")) )  }
}
#freshwaterNH3limit(stationData, trout = TRUE, mussels = TRUE, earlyLife = TRUE)




# This function organizes results from freshwaterNH3limit() into a single assessment decision
freshwaterNH3Assessment <- function(x, # x is station run through freshwaterNH3limit(), which handles citmon/nonagency data appropriately
                                    assessmentType){ # c('acute', 'chronic', 'four-day') one of these options to change criteria tested and window length
  
  
  # Adjust presets based on assessmentType
  if(assessmentType == 'acute'){limitColumn <- quo(acuteExceedance)}
  if(assessmentType == 'chronic'){limitColumn <- quo(chronicExceedance)} ###
  if(assessmentType == 'four-day'){limitColumn <- quo(fourDayExceedance)} ####
  
  # Identify any exceedances
  if(!is.null(x)){
    exceedances <- filter(x, !! limitColumn) # find any exceedances                       
    
    if(nrow(exceedances) > 0){
      
      # Test if > 1 exceedance in any 3 year window, start each window with sampling event
      # Loop through each row of exceedance df to test if any other exceedance in a 3 year window
      exceedancesIn3YrWindow <- tibble()
      for( i in 1 : nrow(exceedances)){
        windowBegin <- exceedances$FDT_DATE_TIME[i]
        windowEnd <- exceedances$FDT_DATE_TIME[i] + years(3)
        
        # Find exceeding data in window defined above
        exceedancesIn3YrWindowData <- filter(exceedances, between(FDT_DATE_TIME, windowBegin, windowEnd) ) %>% 
          ungroup()
        
        
        exceedancesIn3YrWindowi <- tibble(`Window Begin` = windowBegin, `Window End` = windowEnd) %>%
          bind_cols(summarise(exceedancesIn3YrWindowData, nExceedancesInWindow = n())) %>%  # count number of exceedances in 3 year window
          bind_cols(tibble(associatedExceedanceData = list(exceedancesIn3YrWindowData))) # dataset with just exceedances in each exceeding window
        exceedancesIn3YrWindow <- bind_rows(exceedancesIn3YrWindow, exceedancesIn3YrWindowi) 
      }
      
      # Summarize results for user
      # More than one 3 year window with exceedance
      if(nrow(exceedancesIn3YrWindow) > 1){
        return(
          list(
            tibble(AMMONIA_EXC = ifelse(max(exceedancesIn3YrWindow$nExceedancesInWindow) == 1, nrow(exceedancesIn3YrWindow), max(exceedancesIn3YrWindow$nExceedancesInWindow)),
                   AMMONIA_STAT = 'IM',
                   `Assessment Decision` = paste0('Dataset contains more than one 3 year window with at least one ', assessmentType , ' exceedance.')),
            `Exceedance Results` = exceedancesIn3YrWindow)     )
      } else { # only one exceedance in any 3 year window
        return(
          list(
            tibble(AMMONIA_EXC = max(exceedancesIn3YrWindow$nExceedancesInWindow),
                   AMMONIA_STAT = 'Review',
                   `Assessment Decision` = paste0('Dataset contains one 3 year window with at least one ', assessmentType, ' exceedance.')),
            `Exceedance Results` = exceedancesIn3YrWindow)     )
        
      }
    } else { # No exceedances
      return(
        list(
          tibble(AMMONIA_EXC = 0,
                 AMMONIA_STAT = ifelse(nrow(x) > 1, 'S', 'IN'),#'S',
                 `Assessment Decision` = paste0('Dataset contains no ', assessmentType, ' exceedances.')),
          `Exceedance Results` = NA)  )
    }
  } else { # x has no data
    return(
      list(
        tibble(AMMONIA_EXC = NA,
               AMMONIA_STAT = NA,
               `Assessment Decision` = paste0('Dataset contains no Ammonia data.')),
        `Exceedance Results` = NA)  )
  }
  
}

#freshwaterAssessments <- list(acute = freshwaterNH3Assessment(x, 'acute'),
#                              chronic = freshwaterNH3Assessment(x, 'chronic'),
#                              fourDay = freshwaterNH3Assessment(x, 'four-day'))

# Function to consolidate 3 assessments to fit one row
ammoniaDecision <- function(freshwaterAssessments # list of freshwater assessments to consolidate into a single decision
){ 
  consolidatedResults <- map_df(freshwaterAssessments, 1)
  
  review <- filter(consolidatedResults, AMMONIA_STAT %in% c('IM', 'Review'))
  if(nrow(review) > 1){
    
    stationTableOutput <- review %>%
      slice_max(AMMONIA_EXC, n = 1)
    
    # special case if max results in tie, just choose 1
    if(nrow(stationTableOutput) > 1){
      stationTableOutput <- stationTableOutput[1,] }
    
    # Add the other assessment decisions into comment field
    extra <- paste(filter(consolidatedResults, ! `Assessment Decision` %in% stationTableOutput$`Assessment Decision`)$`Assessment Decision`, collapse = ' ')
    stationTableOutput <- mutate(stationTableOutput, `Assessment Decision` = paste(`Assessment Decision`, extra))
  } else { # no exceedances of any type in this scenario
    stationTableOutput <- consolidatedResults[1,]
    # Add the other assessment decisions into comment field
    extra <- paste(filter(consolidatedResults, ! `Assessment Decision` %in% stationTableOutput$`Assessment Decision`)$`Assessment Decision`, collapse = ' ')
    stationTableOutput <- mutate(stationTableOutput, `Assessment Decision` = paste(`Assessment Decision`, extra))
    
  }
  return(stationTableOutput)
}

#ammoniaDecision(list(acute = freshwaterNH3Assessment(x, 'acute'),
#                     chronic = freshwaterNH3Assessment(x, 'chronic'),
#                     fourDay = freshwaterNH3Assessment(x, 'four-day')))




## Identify if metals data exists
metalsData <- function(stationData, metalType){
  if(nrow(stationData) > 0){
    dataOut <- tibble(`_EXC` = NA, `_STAT` = 'Review')
  } else {
    dataOut <- tibble(`_EXC` = NA, `_STAT` = NA)}
  names(dataOut) <- paste0(metalType, names(dataOut))
  return(dataOut)}




# Metals criteria analysis
metalsCriteriaFunction <- function(ID, Hardness, WER){
  # Remember: ln is really log() in R; exp() is natural antilog in R
  # Per 9VAC25-260-140, criteria to 2 sig figs #https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
  
  # If no WER supplied, use 1
  WER <- ifelse(is.na(WER), 1, WER)
  # Establish Hardness Criteria
  criteriaHardness <- ifelse(Hardness < 25, 25, ifelse(Hardness > 400, 400, Hardness))
  
  metalsCriteria <- suppressWarnings(
    tibble(ID= ID, `Antimony PWS` = 5.6, `Antimony All Other Surface Waters` = 640,
           `Arsenic Acute Freshwater` = 340, `Arsenic Chronic Freshwater` = 150, `Arsenic PWS` = 10,
           `Arsenic Acute Saltwater` = 69, `Arsenic Chronic Saltwater` = 36,
           `Barium PWS` = 2000,
           `Cadmium Acute Freshwater` =  signif(WER * exp(0.9789 * (log(criteriaHardness))-3.866) * (1.136672 - (log(criteriaHardness) * 0.041838)), digits = 2),
           `Cadmium Chronic Freshwater` = signif(WER * exp(0.7977 * log(criteriaHardness) - 3.909) * (1.101672 - (log(criteriaHardness) * (0.041838))), digits = 2),
           `Cadmium Acute Saltwater` = signif(33 * WER, digits = 2), `Cadmium Chronic Saltwater` = signif(7.9 * WER, digits = 2), `Cadmium PWS` = 5,
           `ChromiumIII Acute Freshwater` = signif(WER *  (exp(0.8190 * (log(criteriaHardness)) + 3.7256)) * 0.316, digits = 2), 
           `ChromiumIII Chronic Freshwater` = signif(WER *  (exp(0.8190 * (log(criteriaHardness))  +0.6848)) * 0.860, digits = 2), `ChromiumIII PWS` = 100,
           `ChromiumVI Acute Freshwater` = 16, `ChromiumVI Chronic Freshwater` = 11, `ChromiumVI Acute Saltwater` = 1100, `ChromiumVI Chronic Saltwater` = 50, 
           `Copper Acute Freshwater` =  signif(WER * (exp(0.9422 * log(criteriaHardness) - 1.700)) * 0.960, digits = 2),
           `Copper Chronic Freshwater` = signif(WER * (exp(0.8545 * log(criteriaHardness) - 1.702)) * 0.960, digits = 2),
           `Copper Acute Saltwater` =  signif(9.3 * WER, digits = 2), `Copper Chronic Saltwater` =  signif(6.0 * WER, digits = 2), `Copper PWS` = 1300,
           `Lead Acute Freshwater` = signif(WER * (exp(1.273 * log(criteriaHardness) - 1.084)) * (1.46203 - (log(criteriaHardness) * 0.145712)), digits = 2),
           `Lead Chronic Freshwater` = signif(WER * (exp(1.273 * log(criteriaHardness) - 3.259)) * (1.46203 - (log(criteriaHardness) * 0.145712)), digits = 2),
           `Lead Acute Saltwater` = signif(230 * WER, digits = 2), `Lead Chronic Saltwater` = signif(8.8 * WER, digits = 2), `Lead PWS` = 15,
           `Mercury Acute Freshwater` = 1.4, `Mercury Chronic Freshwater` = 0.77, `Mercury Acute Saltwater` = 1.8, `Mercury Chronic Saltwater` = 0.94,
           `Nickel Acute Freshwater` = signif(WER * (exp (0.8460 * log(criteriaHardness) + 1.312)) * 0.998, digits = 2), 
           `Nickel Chronic Freshwater` = signif(WER * (exp(0.8460 * log(criteriaHardness) - 0.8840)) * 0.997, digits = 2),
           `Nickel Acute Saltwater` = signif(74 * WER, digits = 2), `Nickel Chronic Saltwater` = signif(8.2 * WER, digits = 2), `Nickel PWS` = 610,  `Nickel All Other Surface Waters` = 4600,
           `Uranium PWS` = 30,
           `Selenium Acute Freshwater` = 20, `Selenium Chronic Freshwater` = 5.0, 
           `Selenium Acute Saltwater` = signif(290 * WER, digits = 2), `Selenium Chronic Saltwater` = signif(71 * WER, digits = 2),
           `Selenium PWS` = 170, `Selenium All Other Surface Waters` = 4200,
           `Silver Acute Freshwater` = signif(WER * (exp(1.72 * log(criteriaHardness) - 6.52)) * 0.85, digits = 2), `Silver Acute Saltwater` = signif(1.9 * WER, digits = 2),
           `Thallium PWS` = 0.24, `Thallium All Other Surface Waters` = 0.47,
           `Zinc Acute Freshwater` = signif(WER * (exp(0.8473 * log(criteriaHardness) + 0.884)) * 0.978, digits = 2),
           `Zinc Chronic Freshwater` = signif(WER * (exp(0.8473 * log(criteriaHardness) + 0.884)) * 0.986, digits = 2),
           `Zinc Acute Saltwater` = signif(90 * WER, digits = 2), `Zinc Chronic Saltwater` = signif(81 * WER, digits = 2), 
           `Zinc PWS` = 7400, `Zinc All Other Surface Waters` = 26000) %>% 
      pivot_longer(!ID, names_to = 'Criteria', values_to = 'CriteriaValue') %>% 
      mutate(Criteria2 = Criteria) %>%  #duplicate column to split
      separate(Criteria2, c("Metal", "Criteria Type", "Waterbody"), sep = " ") %>% 
      mutate(`Criteria Type` = ifelse(`Criteria Type` == 'All', 'All Other Waters', `Criteria Type`),
             Waterbody = ifelse(Waterbody == 'Other', NA, Waterbody)) %>% 
      dplyr::select(ID, Metal, Criteria, `Criteria Type`, Waterbody, CriteriaValue))
  return(metalsCriteria)
}
#criteria <- metalsCriteriaFunction(stationMetalsData[1,]$Hardness, WER = 1)

# # get one station metals data for assessment
# station <- '2-JKS028.69'
# # pull one station data
# stationData <- filter(conventionals, FDT_STA_ID %in% station) %>% #stationTable$STATION_ID[i]) %>%
#   left_join(stationTable, by = c('FDT_STA_ID' = 'STATION_ID')) %>%
#   pHSpecialStandardsCorrection() %>% # correct pH to special standards where necessary
#   # special lake steps
#   {if(station %in% lakeStations$STATION_ID)#stationTable$STATION_ID[i] %in% lakeStations$STATION_ID)
#     suppressWarnings(suppressMessages(
#       mutate(., lakeStation = TRUE) %>%
#         thermoclineDepth())) # adds thermocline information and SampleDate
#     else mutate(., lakeStation = FALSE) }
# stationMetalsData <- filter(WCmetalsForAnalysis, Station_Id %in% station)

# Single station Metals analysis
metalsAnalysis <- function(stationMetalsData, stationData, WER){
  # If no WER supplied, use 1
  WER <- ifelse(is.na(WER), 1, WER)
  
  # Get WQS from stationData so correct criteria can be applied
  stationMetalsData <- left_join(stationMetalsData, 
                                 dplyr::select(stationData, FDT_STA_ID, CLASS, PWS, ZONE) %>% 
                                   distinct(FDT_STA_ID, .keep_all = TRUE), by = c('Station_Id' = 'FDT_STA_ID')) %>% 
    mutate(`Assess As` = case_when(CLASS == "I" ~ 'Saltwater',
                                   CLASS == "II" & ZONE == 'Estuarine' ~ 'Saltwater',
                                   CLASS == "II" & ZONE == 'Transition' ~ 'More Stringent',
                                   CLASS == "II" & ZONE == 'Tidal Fresh' ~ 'Freshwater',
                                   CLASS %in% c('III', "IV","V","VI","VII") ~ 'Freshwater',
                                   TRUE ~ as.character(NA)),
           ChromiumIII= Chromium, RMK_ChromiumIII = RMK_Chromium, 
           ChromiumVI= Chromium, RMK_ChromiumVI = RMK_Chromium ) %>% # add individual Chromium variables to make joining to assessment criteria easier
    # Roger uses ChromiumIII and VI to flag any potential chromium issues, likely further lab analyses needed if either chromium criteria blown
    dplyr::select(Station_Id:RMK_Cadmium, ChromiumIII:RMK_ChromiumVI, Cadmium:`Assess As`)
  
  # make a place to store raw analysis results
  rawCriteriaResults <- tibble(Station_Id = as.character(NA), WindowDateTimeStart = as.POSIXct(NA), FDT_DEPTH = as.numeric(NA),
                               CLASS = as.factor(NA), PWS = as.factor(NA), ZONE = as.factor(NA), `Assess As` = as.character(NA),
                               Metal = as.character(NA), ValueType = as.character(NA), Value = as.numeric(NA), Criteria = as.character(NA), 
                               `Criteria Type` = as.character(NA), Waterbody = as.character(NA), CriteriaValue = as.numeric(NA),
                               parameterRound = as.numeric(NA), Exceedance = as.numeric(NA))
  acuteCriteriaResults <- rawCriteriaResults 
  chronicCriteriaResults <- acuteCriteriaResults 
  
  # loop through each row of data to correctly calculate criteria and find any chronic scenarios
  for(k in stationMetalsData$FDT_DATE_TIME){
    rawDataWindow <- filter(stationMetalsData, FDT_DATE_TIME == k)
    acuteDataWindow <- filter(stationMetalsData,  between(FDT_DATE_TIME, k, k + hours(1)))
    chronicDataWindow <- filter(stationMetalsData,  between(FDT_DATE_TIME, k, k + days(4)))
    # Run any analyses requiring raw data if data exists
    if(nrow(rawDataWindow) > 0){
      rawData <- rawDataWindow %>% 
        group_by(Station_Id, FDT_DATE_TIME, FDT_DEPTH, CLASS, PWS, ZONE, `Assess As`) %>% 
        dplyr::select(-c(contains('RMK_'))) %>% 
        pivot_longer(cols = Antimony:Hardness, names_to = "Metal", values_to = "Value") %>% 
        mutate(ValueType = 'Raw Result',
               ID = paste(Station_Id, FDT_DATE_TIME, FDT_DEPTH, sep = '_')) %>% # make a uniqueID in case >1 sample for given datetime
        ungroup()
      # Calculate criteria based on raw data
      rawDataCriteria <- metalsCriteriaFunction(filter(rawData, Metal == "Hardness")$ID, filter(rawData, Metal == "Hardness")$Value, WER = 1) %>% 
        filter(`Criteria Type` %in% c('All Other Waters', 'PWS')) %>% # don't need other criteria for acute window
        {if(is.na(unique(rawData$PWS)))
          filter(., `Criteria Type` != 'PWS')
          else .}
      # Join appropriate criteria to rawData for comparison to averaged data
      rawDataCriteriaAnalysis <- left_join(rawData, rawDataCriteria, by = c('ID', 'Metal')) %>% 
        mutate(parameterRound = signif(Value, digits = 2), # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
               Exceedance = ifelse(parameterRound > parameterRound, 1, 0 ),
               WindowDateTimeStart = min(rawDataWindow$FDT_DATE_TIME)) %>%  # use 1/0 to easily summarize multiple results later
        filter(!is.na(Criteria)) %>%  # filter out metals that don't have chronic criteria
        dplyr::select(Station_Id, WindowDateTimeStart, everything()) %>% 
        dplyr::select(-c(FDT_DATE_TIME, ID))
      # Save the results for viewing later
      rawCriteriaResults <- bind_rows(rawCriteriaResults, rawDataCriteriaAnalysis) 
    } else {rawCriteriaResults <- rawCriteriaResults }
    # Run acute analysis if data exists
    if(nrow(acuteDataWindow) > 0){
      acuteData <- acuteDataWindow %>% 
        group_by(Station_Id, FDT_DEPTH, CLASS, PWS, ZONE, `Assess As`) %>% # can't group by datetime or summary can't happen
        dplyr::select(-c(contains('RMK_'))) %>% 
        pivot_longer(cols = Antimony:Hardness, names_to = "Metal", values_to = "CriteriaValue") %>% 
        ungroup() %>% group_by(Station_Id, FDT_DEPTH, CLASS, PWS, ZONE, `Assess As`, Metal) %>% 
        summarise(Value = mean(CriteriaValue, na.rm=T)) %>%  # get hourly average
        mutate(ValueType = 'Hourly Average',
               ID = paste(Station_Id, FDT_DEPTH, sep = '_')) # make a uniqueID in case >1 sample for given datetime
      # Calculate criteria based on hourly averaged data
      acuteDataCriteria <- metalsCriteriaFunction(filter(acuteData, Metal == "Hardness")$ID, filter(acuteData, Metal == "Hardness")$Value, WER = 1) %>% 
        filter(`Criteria Type` == 'Acute') %>% # don't need other criteria for acute window
        # Keep only the criteria needed 
        {if(unique(acuteData$`Assess As`) %in% c('Freshwater', 'Saltwater'))
          filter(., Waterbody %in% c(NA, !!unique(acuteData$`Assess As`)))
          # if in Transition Zone then use the more stringent standard
          else group_by(., Metal) %>% 
            mutate(MoreStringent = min(CriteriaValue)) %>% 
            filter(CriteriaValue == MoreStringent) %>% 
            dplyr::select(-MoreStringent)} 
      # Join appropriate criteria to acuteData for comparison to averaged data
      acuteDataCriteriaAnalysis <- left_join(acuteData, acuteDataCriteria, by = c('ID', 'Metal')) %>% 
        mutate(parameterRound = signif(Value, digits = 2), # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
               Exceedance = ifelse(parameterRound > CriteriaValue, 1, 0 ), # use 1/0 to easily summarize multiple results later
               WindowDateTimeStart = min(acuteDataWindow$FDT_DATE_TIME)) %>% 
        filter(!is.na(Criteria)) %>% # filter out metals that don't have chronic criteria
        dplyr::select(Station_Id, WindowDateTimeStart, everything(), -ID)
      # Save the results for viewing later
      acuteCriteriaResults <- bind_rows(acuteCriteriaResults, acuteDataCriteriaAnalysis) 
    } else {acuteCriteriaResults <- acuteCriteriaResults }
    # Run chronic analysis if data exists
    if(nrow(chronicDataWindow) > 0){
      chronicData <- chronicDataWindow %>% 
        group_by(Station_Id, FDT_DEPTH, CLASS, PWS, ZONE, `Assess As`) %>% # can't group by datetime or summary can't happen
        dplyr::select(-c(contains('RMK_'))) %>% 
        pivot_longer(cols = Antimony:Hardness, names_to = "Metal", values_to = "CriteriaValue") %>% 
        ungroup() %>% group_by(Station_Id, FDT_DEPTH, CLASS, PWS, ZONE, `Assess As`, Metal) %>% 
        summarise(Value = mean(CriteriaValue, na.rm=T)) %>% # get four day average
        mutate(ValueType = 'Four Day Average',
               ID = paste(Station_Id, FDT_DEPTH, sep = '_')) # make a uniqueID in case >1 sample for given datetime
      # Calculate criteria based on hourly averaged data
      chronicDataCriteria <- metalsCriteriaFunction(filter(chronicData, Metal == "Hardness")$ID, filter(chronicData, Metal == "Hardness")$Value, WER = 1) %>% 
        filter(`Criteria Type` == 'Chronic') %>% # don't need other criteria for chronic window analysis
        # Keep only the criteria needed 
        {if(unique(chronicData$`Assess As`) %in% c('Freshwater', 'Saltwater'))
          filter(., Waterbody %in% c(NA, !!unique(chronicData$`Assess As`)))
          # if in Transition Zone then use the more stringent standard
          else group_by(., Metal) %>% 
            mutate(MoreStringent = min(CriteriaValue)) %>% 
            filter(CriteriaValue == MoreStringent) %>% 
            dplyr::select(-MoreStringent)} 
      # Join appropriate criteria to chronicData for comparison to averaged data
      chronicDataCriteriaAnalysis <- left_join(chronicData, chronicDataCriteria, by = c('ID', 'Metal')) %>% 
        mutate(parameterRound = signif(Value, digits = 2), # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
               Exceedance = ifelse(parameterRound > CriteriaValue, 1, 0 ), # use 1/0 to easily summarize multiple results later
               WindowDateTimeStart = min(chronicDataWindow$FDT_DATE_TIME)) %>% 
        filter(!is.na(Criteria)) %>% # filter out metals that don't have chronic criteria
        dplyr::select(Station_Id, WindowDateTimeStart, everything(), -ID)
      # Save the results for viewing later
      chronicCriteriaResults <- bind_rows(chronicCriteriaResults, chronicDataCriteriaAnalysis) 
    } else {chronicCriteriaResults <- chronicCriteriaResults }
  }
  stationCriteriaResults <- bind_rows(rawCriteriaResults, acuteCriteriaResults, chronicCriteriaResults) %>% 
    filter(!is.na(Station_Id)) %>% # drop placeholder rows
    distinct(Station_Id, WindowDateTimeStart, FDT_DEPTH, Criteria, .keep_all = T) %>% # remove duplicates in case > 1 depth per datetime
    arrange(Station_Id, WindowDateTimeStart, FDT_DEPTH, Metal)
  return(stationCriteriaResults)
}

# Metals Assessment function that makes sense of output from metalsAnalysis()
metalsAssessmentFunction <- function(metalsAnalysisResults){
  metalsAnalysisResults %>% 
    group_by(Station_Id, Criteria) %>% 
    summarise(Exceedances = sum(Exceedance, na.rm = T)) %>% 
    arrange(Criteria) %>% # arrange on just Criteria to make column order make more sense 
    pivot_wider(names_from = Criteria, values_from = Exceedances)
}


# Single station chloride assessment
chlorideFreshwaterAnalysis <- function(stationData){
  # doesn't apply in class II transition zone
  stationData <- filter(stationData, CLASS %in% c('III', "IV","V","VI","VII") |
                          CLASS ==  "II" & ZONE == 'Tidal Fresh') %>% 
    filter(!(LEVEL_CHLORIDE %in% c('Level II', 'Level I'))) %>% # get lower levels out
    filter(!is.na(CHLORIDE_mg_L)) #get rid of NA's
  
  if(nrow(stationData) > 0){
    
    # make a place to store analysis results
    acuteCriteriaResults <- tibble(FDT_STA_ID = as.character(NA), WindowDateTimeStart = as.POSIXct(NA), FDT_DEPTH = as.numeric(NA),
                                   Value = as.numeric(NA), ValueType = as.character(NA), 
                                   `Criteria Type` = as.character(NA), CriteriaValue = as.numeric(NA), 
                                   parameterRound = as.numeric(NA), Exceedance = as.numeric(NA))
    chronicCriteriaResults <- acuteCriteriaResults
    
    # loop through each row of data to correctly calculate criteria and find any chronic scenarios
    for(k in stationData$FDT_DATE_TIME){  #k = stationData$FDT_DATE_TIME[1]
      acuteDataWindow <- filter(stationData,  between(FDT_DATE_TIME, k, k + hours(1)))
      chronicDataWindow <- filter(stationData,  between(FDT_DATE_TIME, k, k + days(4)))
      
      # Run acute analysis if data exists
      if(nrow(acuteDataWindow) > 0){
        acuteDataCriteriaAnalysis <- dplyr::select(acuteDataWindow, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, CHLORIDE_mg_L) %>% 
          group_by(FDT_STA_ID, FDT_DEPTH) %>% # can't group by datetime or summary can't happen
          summarise(Value = mean(CHLORIDE_mg_L, na.rm=T)) %>%  # get hourly average
          mutate(ValueType = 'Hourly Average',
                 ID = paste( FDT_STA_ID, FDT_DEPTH, sep = '_'), # make a uniqueID in case >1 sample for given datetime
                 `Criteria Type` = 'Acute', 
                 CriteriaValue = 860) %>%  # 860,000ug/L criteria to mg/L
          mutate(parameterRound = signif(Value, digits = 2), # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
                 Exceedance = ifelse(parameterRound > CriteriaValue, 1, 0 ), # use 1/0 to easily summarize multiple results later
                 WindowDateTimeStart = min(acuteDataWindow$FDT_DATE_TIME)) %>% 
          dplyr::select(FDT_STA_ID, WindowDateTimeStart, everything(), -ID)
        # Save the results for viewing later
        acuteCriteriaResults <- bind_rows(acuteCriteriaResults, acuteDataCriteriaAnalysis) 
      } else {acuteCriteriaResults <- acuteCriteriaResults }
      
      # Run chronic analysis if data exists
      if(nrow(chronicDataWindow) > 0){
        chronicDataCriteriaAnalysis <- dplyr::select(chronicDataWindow, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, CHLORIDE_mg_L) %>% 
          group_by(FDT_STA_ID, FDT_DEPTH) %>% # can't group by datetime or summary can't happen
          summarise(Value = mean(CHLORIDE_mg_L, na.rm=T)) %>%  # get hourly average
          mutate(ValueType = 'Four Day Average',
                 ID = paste( FDT_STA_ID, FDT_DEPTH, sep = '_'), # make a uniqueID in case >1 sample for given datetime
                 `Criteria Type` = 'Chronic', 
                 CriteriaValue = 230) %>%  # 230,000ug/L criteria to mg/L
          mutate(parameterRound = signif(Value, digits = 2), # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
                 Exceedance = ifelse(parameterRound > CriteriaValue, 1, 0 ), # use 1/0 to easily summarize multiple results later
                 WindowDateTimeStart = min(chronicDataWindow$FDT_DATE_TIME)) %>% 
          dplyr::select(FDT_STA_ID, WindowDateTimeStart, everything(), -ID)
        # Save the results for viewing later
        chronicCriteriaResults <- bind_rows(chronicCriteriaResults, chronicDataCriteriaAnalysis) 
      } else {chronicCriteriaResults <- chronicCriteriaResults }
    }
    #summarise results
    stationCriteriaResults <- bind_rows( acuteCriteriaResults, chronicCriteriaResults) %>% 
      filter(!is.na(FDT_STA_ID)) %>% # drop placeholder rows
      distinct(FDT_STA_ID, WindowDateTimeStart, FDT_DEPTH, `Criteria Type`, .keep_all = T) %>% # remove duplicates in case > 1 depth per datetime
      arrange(FDT_STA_ID, WindowDateTimeStart, FDT_DEPTH, `Criteria Type`)
    return(stationCriteriaResults)
  } else {return(tibble(FDT_STA_ID = as.character(NA), WindowDateTimeStart = as.POSIXct(NA), FDT_DEPTH = as.numeric(NA),
                        Value = as.numeric(NA), ValueType = as.character(NA), 
                        `Criteria Type` = as.character(NA), CriteriaValue = as.numeric(NA), 
                        parameterRound = as.numeric(NA), Exceedance = as.numeric(NA)) ) }
}
#chlorideFreshwaterAnalysis(stationData)




chlorideFreshwaterSummary <- function(chlorideFreshwater){
  chlorideFreshwaterExceed <- filter(chlorideFreshwater, Exceedance ==1)
  if(nrow(chlorideFreshwaterExceed) >0){
    return(tibble(CHL_EXC = nrow(chlorideFreshwaterExceed), CHL_STAT = 'Review'))
  } else {return(tibble(CHL_EXC = NA, CHL_STAT= NA))}
}
#chlorideFreshwaterSummary(suppressMessages(chlorideFreshwaterAnalysis(stationData)))

