
## Old bacteria methods just hanging on

bacteria_ExceedancesSTV_OLD <- function(x, STVlimit){                                    
  x %>% rename(parameter = !!names(.[2])) %>% # rename columns to make functions easier to apply
    mutate(limit = STVlimit, exceeds = ifelse(parameter > limit, T, F)) # Single Sample Maximum 
}

bacteria_ExceedancesGeomeanOLD <- function(x, bacteriaType, geomeanLimit){
  if(nrow(x) > 0){
    suppressWarnings(mutate(x, SampleDate = format(FDT_DATE_TIME,"%m/%d/%y"), # Separate sampling events by day
                            previousSample=lag(SampleDate,1),
                            previousSampleBacteria=lag(!! bacteriaType,1)) %>% # Line up previous sample with current sample line
                       rowwise() %>% 
                       mutate(sameSampleMonth= as.numeric(strsplit(SampleDate,'/')[[1]][1])  -  as.numeric(strsplit(previousSample,'/')[[1]][1])) %>% # See if sample months are the same, e.g. more than one sample per calendar month
                       filter(sameSampleMonth == 0 | is.na(sameSampleMonth)) %>% # keep only rows with multiple samples per calendar month  or no previous sample (NA) to then test for geomean
                       # USING CALENDAR MONTH BC THAT'S HOW WRITTEN IN GUIDANCE, rolling 4 wk windows would have been more appropriate
                       mutate(sampleMonthYear = paste(month(as.Date(SampleDate,"%m/%d/%y")),year(as.Date(SampleDate,"%m/%d/%y")),sep='/')) %>% # grab sample month and year to group_by() for next analysis
                       group_by(sampleMonthYear) %>%
                       mutate(geoMeanCalendarMonth =  EnvStats::geoMean(as.numeric(get(bacteriaType)), na.rm = TRUE), # Calculate geomean
                              limit = geomeanLimit, samplesPerMonth = n()))
  }
}
# How bacteria is assessed
bacteria_Assessment_OLD <- function(x, bacteriaType, geomeanLimit, STVlimit){
  if(nrow(x)>1){
    bacteria <- dplyr::select(x,FDT_DATE_TIME, !! bacteriaType)%>% # Just get relevant columns, 
      filter(!is.na(!!bacteriaType)) #get rid of NA's
    # Geomean Analysis (if enough n)
    if(nrow(bacteria)>0){
      bacteriaGeomean <- bacteria_ExceedancesGeomeanOLD(bacteria, bacteriaType, geomeanLimit) %>%     
        distinct(sampleMonthYear, .keep_all = T) %>%
        filter(samplesPerMonth > 4, geoMeanCalendarMonth > limit) %>% # minimum sampling rule for geomean to apply
        mutate(exceeds = TRUE) %>%
        dplyr::select(sampleMonthYear, geoMeanCalendarMonth, limit, exceeds, samplesPerMonth)
      geomeanResults <- quickStats(bacteriaGeomean, bacteriaType) %>%
        mutate(`Assessment Method` = 'Old Monthly Geomean')
      geomeanResults[,4] <- ifelse(is.na(geomeanResults[,4]),NA, dplyr::recode(geomeanResults[,4], 'Review' = paste('Review if ', bacteriaType,'_VIO > 1',sep='')))
      
      # Single Sample Maximum Analysis
      bacteriaSSM <- bacteria_ExceedancesSTV_OLD(bacteria, STVlimit) 
      SSMresults <- quickStats(bacteriaSSM, bacteriaType) %>% mutate(`Assessment Method` = 'Old Single Sample Maximum')
      return( rbind(geomeanResults, SSMresults) )
    }
  }
  
}



# Quick stats function from assessment process, just efficiently applies assessment logic across many parameters

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
    z <- data.frame(EXC = NA, SAMP= nrow(parameterDataset), exceedanceRate= NA, STAT= paste(parameter, 'WQS info missing from analysis'))
    names(z) <- paste(parameter,names(z), sep='_')
    return(z)
  }
}


