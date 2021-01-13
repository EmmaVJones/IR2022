
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
    mutate(Lake_Name = case_when(WATER_NAME %in% c('Dan River','Buffalo Creek','Bluestone Creek') ~ 'Kerr Reservoir',
                                 WATER_NAME %in% c('Smith Lake (Aquia Reservoir)') ~ 'Aquia Reservoir (Smith Lake)',
                                 WATER_NAME %in% c('Claytor Lake (New River)', 'Claytor Lake (Peak Creek)') ~ 'Claytor Lake',
                                 WATER_NAME %in% c('Fairystone Lake (Goblin Town Creek)') ~ 'Fairystone Lake', 
                                 WATER_NAME %in% c('Harwoods Mill Reservoir (PWS)') ~ 'Harwoods Mill Reservoir',
                                 WATER_NAME %in% c('Lake Anna', 'Lake Anna/Contrary Creek', 'Lake Anna/Freshwater Creek', 
                                                   'Lake Anna/Gold Mine Creek', 'Lake Anna/Pamunkey Creek', 
                                                   'Lake Anna/Plentiful Creek', 'Terrys Run/Lake Anna') ~ 'Lake Anna',
                                 WATER_NAME %in% c('Lake Cohoon (PWS)') ~ 'Lake Cohoon',          
                                 WATER_NAME %in% c('Lake Kilby (PWS)') ~ 'Lake Kilby',
                                 WATER_NAME %in% c('Lake Meade (PWS)') ~ 'Lake Meade',
                                 WATER_NAME %in% c('Lake Moomaw (Jackson River)') ~ 'Lake Moomaw',
                                 WATER_NAME %in% c('Lake Prince - Reservoir (PWS)') ~ 'Lake Prince - Reservoir',
                                 WATER_NAME %in% c('Lake Smith (PWS)') ~ 'Lake Smith',
                                 WATER_NAME %in% c('Lake Whitehurst (PWS)') ~ 'Lake Whitehurst',
                                 WATER_NAME %in% c('Leesville Lake', 'Leesville Lake (Pigg R.)', 
                                                   'Leesville Lake Middle (Roanoke R.)') ~ 'Leesville Lake',
                                 WATER_NAME %in% c('Lee Hall Reservoir- Upper, Middle','Lee Hall Reservoir-Lower') ~ 'Lee Hall Reservoir',
                                 WATER_NAME %in% c('Little Creek Reservoir - (PWS)') ~ 'Little Creek Reservoir (VBC)',
                                 WATER_NAME %in% c('Little Creek Reservoir (PWS)') ~ 'Little Creek Reservoir (JCC)',
                                 WATER_NAME %in% c('Lone Star Lake F (PWS)') ~ 'Lone Star Lake F (Crystal Lake)', 
                                 WATER_NAME %in% c('Lone Star Lake G (PWS)') ~ 'Lone Star Lake G (Crane Lake)', 
                                 WATER_NAME %in% c('Lone Star Lake I (PWS)') ~ 'Lone Star Lake I (Butler Lake)', 
                                 WATER_NAME %in% c('Martinsville (Beaver Creek) Reservoir') ~ 'Martinsville Reservoir (Beaver Creek Reservoir)',
                                 WATER_NAME %in% c('Philpott Reservoir (Goblin Town Creek)', 
                                                   'Philpott Reservoir (Smith River)') ~ "Philpott Reservoir",
                                 WATER_NAME %in% c('Roanoke River') ~ 'Lake Gaston',                         
                                 WATER_NAME %in% c('S F Rivanna River Reservoir') ~ 'Rivanna Reservoir (South Fork Rivanna Reservoir)',
                                 str_detect(WATER_NAME, 'Smith Mtn. Lake') ~ 'Smith Mountain Lake', 
                                 WATER_NAME %in% c('Speights Run - Lake (PWS)') ~ 'Speights Run Lake',
                                 WATER_NAME %in% c('Waller Mill Reservoir (PWS)') ~ 'Waller Mill Reservoir',
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
      mutate(exceedanceRate = as.numeric(round((EXC/SAMP)*100,digits=0))) # round to nearest whole number per Memo to Standardize Rounding for Assessment Guidance
    
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
    z <- data.frame(EXC = NA, SAMP= nrow(parameterDataset), exceedanceRate= NA, STAT=NA)
    names(z) <- paste(parameter,names(z), sep='_')
    return(z)
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
  dplyr::select(x,FDT_DATE_TIME,FDT_TEMP_CELCIUS, LEVEL_FDT_TEMP_CELCIUS, `Max Temperature (C)`)%>% # Just get relevant columns, 
    filter(!(LEVEL_FDT_TEMP_CELCIUS %in% c('Level II', 'Level I'))) %>% # get lower levels out
    filter(!is.na(FDT_TEMP_CELCIUS))%>% #get rid of NA's
    rename(parameter = !!names(.[2]), limit = !!names(.[4])) %>% # rename columns to make functions easier to apply
    # Round to Even Rule
    mutate(parameterRound = signif(parameter, digits = 2), # two significant figures based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section50/
           exceeds = ifelse(parameterRound > limit, T, F)) # Identify where above max Temperature, 
}
#tempExceedances(x) %>%
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
  } }

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
  # remove any data that shouldn't be considered
  x <- filter(x, !(LEVEL_FDT_TEMP_CELCIUS %in% c('Level II', 'Level I')) |
                !(LEVEL_FDT_FIELD_PH %in% c('Level II', 'Level I'))) %>% # get lower levels out
    # lake stations should only be surface sample
    {if(unique(x$lakeStation) == TRUE)
      filter(., FDT_DEPTH <= 0.3)
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
                 AMMONIA_STAT = 'S',
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





