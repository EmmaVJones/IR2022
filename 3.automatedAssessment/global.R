
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
                    `Max Temperature (C)` = c(NA, NA, NA, 32, 31, 21, 20, NA))

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
    results <- data.frame(VIO = nrow(filter(parameterDataset, exceeds == TRUE)),
                          SAMP = nrow(parameterDataset)) %>%
      # Implement Round to Even on Exceedance Frequency
      mutate(exceedanceRate = as.numeric(round((VIO/SAMP)*100,digits=0))) # round to nearest whole number per Memo to Standardize Rounding for Assessment Guidance
    
    if(results$VIO >= 1){outcome <- 'Review'} # for Mary
    if(results$VIO >= 1 & results$exceedanceRate < 10.5){outcome <- 'Review'}
    if(results$exceedanceRate > 10.5 & results$VIO >= 2 & results$SAMP > 10){outcome <- '10.5% Exceedance'}
    if(results$VIO < 1 &results$exceedanceRate < 10.5 & results$SAMP > 10){outcome <- 'S'}
    if(results$VIO >= 1 & results$SAMP <= 10){outcome <- 'Review'}
    if(results$VIO < 1 & results$SAMP <= 10){outcome <- 'S'}
    
    
    results <- mutate(results, STAT = outcome)
    names(results) <- c(paste(parameter,names(results)[1], sep = '_'),
                        paste(parameter,names(results)[2], sep = '_'),
                        paste(parameter,names(results)[3], sep = '_'),
                        paste(parameter,names(results)[4], sep = '_'))
    #rename based on parameter entered
    return(results)
  } else {
    z <- data.frame(VIO = NA, SAMP= nrow(parameterDataset), exceedanceRate= NA, STAT=NA)
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
  temp <- dplyr::select(x,FDT_DATE_TIME,FDT_TEMP_CELCIUS, FDT_TEMP_CELCIUS_RMK, `Max Temperature (C)`)%>% # Just get relevant columns, 
    filter(!(FDT_TEMP_CELCIUS_RMK %in% c('Level II', 'Level I'))) %>% # get lower levels out
    filter(!is.na(FDT_TEMP_CELCIUS))%>% #get rid of NA's
    rename(parameter = !!names(.[2]), limit = !!names(.[4])) %>% # rename columns to make functions easier to apply
    # Round to Even Rule
    mutate(parameterRound = round(parameter, digits = 0), # round to whole number based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section50/
           exceeds = ifelse(parameterRound > limit, T, F)) # Identify where above max Temperature, 

  quickStats(temp, 'TEMP')
}
#tempExceedances(x)


# Minimum DO Exceedance function
DOExceedances_Min <- function(x){
  DO <- dplyr::select(x,FDT_DATE_TIME,DO,DO_RMK,`Dissolved Oxygen Min (mg/L)`)%>% # Just get relevant columns, 
    filter(!(DO_RMK %in% c('Level II', 'Level I'))) %>% # get lower levels out
    filter(!is.na(DO)) %>% 
    rename(parameter = !!names(.[2]), limit = !!names(.[4])) %>% # rename columns to make functions easier to apply
    # Round to Even Rule
    mutate(parameterRound = round(parameter, digits = 1), # round to 1 digit based on WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section50/
           exceeds = ifelse(parameterRound < limit, T, F))# Identify where below min DO 
  
  quickStats(DO, 'DO')
}
#DOExceedances_Min(x)

# pH range Exceedance Function
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
  
  quickStats(pH, 'PH')
}
#pHExceedances(x)