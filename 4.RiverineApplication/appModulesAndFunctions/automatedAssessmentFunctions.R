
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
    if(results$EXC < 1 & results$SAMP <= 10){outcome <- 'S'}
    
    
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




#Max Temperature Exceedance Function
tempExceedances <- function(x){
  dplyr::select(x,FDT_DATE_TIME,FDT_TEMP_CELCIUS, FDT_TEMP_CELCIUS_RMK, `Max Temperature (C)`)%>% # Just get relevant columns, 
    filter(!(FDT_TEMP_CELCIUS_RMK %in% c('Level II', 'Level I'))) %>% # get lower levels out
    filter(!is.na(FDT_TEMP_CELCIUS))%>% #get rid of NA's
    rename(parameter = !!names(.[2]), limit = !!names(.[4])) %>% # rename columns to make functions easier to apply
    # Round to Even Rule
    mutate(parameterRound = round(parameter, digits = 0), # round to whole number based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section50/
           exceeds = ifelse(parameterRound > limit, T, F)) # Identify where above max Temperature, 
}
#tempExceedances(x) %>%
#  quickStats('TEMP')


# Minimum DO Exceedance function
DOExceedances_Min <- function(x){
  dplyr::select(x,FDT_DATE_TIME,DO,DO_RMK,`Dissolved Oxygen Min (mg/L)`)%>% # Just get relevant columns, 
    filter(!(DO_RMK %in% c('Level II', 'Level I'))) %>% # get lower levels out
    filter(!is.na(DO)) %>% 
    rename(parameter = !!names(.[2]), limit = !!names(.[4])) %>% # rename columns to make functions easier to apply
    # Round to Even Rule
    mutate(parameterRound = round(parameter, digits = 1), # round to 1 digit based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section50/
           exceeds = ifelse(parameterRound < limit, T, F))# Identify where below min DO 
}
#DOExceedances_Min(x) %>% quickStats('DO')


# Daily Average exceedance function
DO_Assessment_DailyAvg <- function(x){ 
  dplyr::select(x,FDT_STA_ID,FDT_DATE_TIME, FDT_DATE_TIME,FDT_DEPTH,DO,DO_RMK,`Dissolved Oxygen Min (mg/L)`,`Dissolved Oxygen Daily Avg (mg/L)`)%>% # Just get relevant columns, 
    filter(!(DO_RMK %in% c('Level II', 'Level I'))) %>% # get lower levels out
    filter(!is.na(DO)) %>% #get rid of NA's
    mutate(date = as.Date(FDT_DATE_TIME, format="%m/%d/%Y"), 
           limit = `Dissolved Oxygen Daily Avg (mg/L)`) %>% 
    group_by(date) %>%
    mutate(n_Samples_Daily = n()) %>% # how many samples per day?
    filter(n_Samples_Daily > 1) %>%
    # Daily average with average rounded to even
    mutate(DO_DailyAverage = round(mean(DO), digits = 1),  # round to 1 digit based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section50/
           exceeds = ifelse(DO_DailyAverage < `Dissolved Oxygen Daily Avg (mg/L)`,T,F)) %>% 
    ungroup() %>% 
    dplyr::select(-c(FDT_DATE_TIME, `Dissolved Oxygen Min (mg/L)`))
}
#DO_Assessment_DailyAvg(x) %>% quickStats('DO_Daily_Avg')

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
  pH <- dplyr::select(x,FDT_DATE_TIME,FDT_DEPTH,FDT_FIELD_PH,FDT_FIELD_PH_RMK,`pH Min`,`pH Max`)%>% # Just get relevant columns, 
    filter(!(FDT_FIELD_PH_RMK %in% c('Level II', 'Level I'))) %>% # get lower levels out
    filter(!is.na(FDT_FIELD_PH)) #get rid of NA's
    
  # only run analysis if WQS exist for station
    if(any(is.na(pH$`pH Min`)) | any(is.na(pH$`pH Max`))){
      pH <- mutate(pH, interval = 1, exceeds = FALSE, limit = `pH Min`) # placeholder to run quickStats() without any WQS
    } else {
      pH <- pH %>%
        rowwise() %>% 
        # Round to Even Rule
        mutate(parameterRound = round(FDT_FIELD_PH, digits = 1)) %>% # round to 1 digit based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section50/
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
      mutate(`Parameter Rounded to WQS Format` = round(!! fieldName_, digits = 0),  # round to WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
             limit =  PWSlimit) %>%
      rename(parameter = !!names(.[4])) %>% # rename columns to make functions easier to apply
      mutate(exceeds = ifelse(parameter > limit, T, F)) # Identify where above WQS limit
    return(quickStats(parameterData, outputName)) #%>% dplyr::select(-ends_with('STAT')))    
  } }

#assessPWS(x, NITRATE, RMK_NITRATE, 10, 'PWS_Nitrate')
#assessPWS(x, CHLORIDE, RMK_CHLORIDE, 250, 'PWS_Chloride')
#assessPWS(x, SULFATE_TOTAL, RMK_SULFATE_TOTAL, 250, 'PWS_Total_Sulfate')


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
#countNutrients(x, PHOSPHORUS, RMK_PHOSPHORUS, 0.2) %>% quickStats('NUT_TP')
#countNutrients(x, CHLOROPHYLL, RMK_CHLOROPHYLL, NA)  %>% quickStats('NUT_CHLA')




# Metals exceedances

metalsExceedances <- function(x, metalType){
  # if any data given to function
  if(nrow(x) > 0){ EXC <- length(which(x == 'NSP' | x == 'OE')) 
  }else { EXC <- NA  }
  
  x <- tibble(EXC = EXC, STAT = ifelse(EXC > 0, 'Review', 'S'))
  names(x) <- paste(metalType,names(x), sep='_')
  return(x)
}
