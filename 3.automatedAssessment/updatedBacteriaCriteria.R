# Updated Bacteria Criteria based on 9VAC25-260-170 (https://lis.virginia.gov/cgi-bin/legp604.exe?000+reg+9VAC25-260-170&000+reg+9VAC25-260-170)

# Function is built to calculate both E.coli and Enterococci based on 90 day assessment windows.
# here is some test data 
#stationData <- filter(conventionals, FDT_STA_ID %in% '1AACO014.57')%>%
#    left_join(stationTable, by = c('FDT_STA_ID' = 'STATION_ID'))

#stationData <- filter(conventionals, FDT_STA_ID %in% '2-JKS023.61') %>%
#  left_join(stationTable, by = c('FDT_STA_ID' = 'STATION_ID'))

# add some high frequency data
#stationData <- bind_rows(stationData,
#                         data.frame(FDT_STA_ID = c('2-JKS023.61', '2-JKS023.61', '2-JKS023.61', '2-JKS023.61', '2-JKS023.61', '2-JKS023.61', 
#                                                  '2-JKS023.61', '2-JKS023.61', '2-JKS023.61', '2-JKS023.61'),
#                           FDT_DATE_TIME= as.POSIXct(c('2019-02-12 10:00:00', '2019-02-13 10:00:00', '2019-02-14 10:00:00', '2019-02-15 10:00:00', '2019-02-16 10:00:00',
#                                           '2019-02-17 10:00:00', '2019-02-18 10:00:00', '2019-02-19 10:00:00', '2019-02-20 10:00:00', '2019-02-21 10:00:00')),
#                          E.COLI = c(22, 33, 44, 55, 66, 77, 88, 99, 100, 800),
#                          ECOLI_RMK = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)))


#x <- stationData
#bacteriaField <- 'E.COLI' #
##  'ENTEROCOCCI'
#bacteriaRemark <- 'ECOLI_RMK' #  
##  'RMK_31649'
#sampleRequirement <- 10
#STV <- 410 #
##130
#geomeanCriteria <- 126 #
##  35
#rm(i); rm(x);rm(bacteriaField);rm(bacteriaRemark); rm(sampleRequirement); rm(STV); rm(geomeanCriteria); rm(stationTableName); rm(z);rm(exceedGeomean); rm(out); rm(exceedSTVrate);rm(x2); rm(time1);rm(timePlus89); rm(nSamples); rm(exceedSTVn);

# Function to assess on 90 day windows across input dataset
# really just a building block, one probably wouldn't run this function independently
bacteriaExceedances_NEW <- function(x, # input dataframe with bacteria data
                                    bacteriaField, # name of bacteria field data
                                    bacteriaRemark, # name of bacteria comment field
                                    sampleRequirement, # minimum n samples in 90 day window needed to apply geomean
                                    STV, # unique for ecoli/enter
                                    geomeanCriteria # unique for ecoli/enter
                                    ){
  # Output tibble to organize results, need list object to save associate data
  out <- tibble(`StationID` = as.character(NA),
                `Date Window Starts` = as.Date(NA), 
                `Date Window Ends` = as.Date(NA), 
                `Samples in 90 Day Window` = as.numeric(NA), 
                `STV Exceedances In Window` = as.numeric(NA), 
                `STV Exceedance Rate` = as.numeric(NA), 
                `STV Assessment` = as.character(NA),
                `Geomean In Window` = as.numeric(NA),
                `Geomean Assessment` = as.character(NA),
                associatedData = list())
  
  # Data reorg to enable both types of bacteria assessment from a single function
  x2 <- dplyr::select(x, FDT_STA_ID, FDT_DATE_TIME, !! bacteriaField, !! bacteriaRemark) %>%
    rename(Value = bacteriaField) %>%
    filter(!is.na(Value))
  
  if(nrow(x2) > 0){
    # Loop through each row of input df to test 90 day windows against assessment criteria
    for( i in 1 : nrow(x2)){
      time1 <- as.Date(x2$FDT_DATE_TIME[i])
      timePlus89 <- time1 + days(89) 
      
      # Organize prerequisites to decision process
      z <- filter(x2, FDT_DATE_TIME >= time1 & FDT_DATE_TIME <= timePlus89) %>% 
        mutate(nSamples = n(), # count number of samples in 90 day window
               STVhit = ifelse(Value > STV, TRUE, FALSE), # test values in window against STV
               E.COLI_geomean = ifelse(nSamples > 1, EnvStats::geoMean(Value, na.rm = TRUE), NA), # calculate geomean of samples if nSamples>1
               geomeanCriteriaHit = ifelse(E.COLI_geomean > geomeanCriteria, TRUE, FALSE)) # test geomean against geomean Criteria
      
      # First level of testing: any STV hits in dataset? Want this information for all scenarios
      nSTVhitsInWindow <- nrow(filter(z, STVhit == TRUE))
      STVexceedanceRate <- ifelse(z$nSamples >= 10, round(( nSTVhitsInWindow / unique(z$nSamples)) * 100, digits = 0), # STV exceedance rate calculation with round to even math
                                  NA) # no STV exceedance rate if < 10 samples
      if(nSTVhitsInWindow == 0){
        `STV Assessment` <- 'No STV violations within 90 day window' } 
      if(nSTVhitsInWindow == 1){
        `STV Assessment` <- paste(nSTVhitsInWindow, ' STV violation(s) with ', format(STVexceedanceRate, digits = 3), 
                                  '% exceedance rate in 90 day window | Insufficient Information (Prioritize for follow up monitoring)',sep='')}
      if(nSTVhitsInWindow >= 2){
        `STV Assessment` <- paste(nSTVhitsInWindow, ' STV violation(s) with ', format(STVexceedanceRate, digits = 3), 
                                  '% exceedance rate in 90 day window | Impaired: ', nSTVhitsInWindow,' hits in the same 90-day period',sep='') }
      
      
      # Second level of testing: only if minimum geomean sampling requirements met in 90 day period
      if(unique(z$nSamples) >= sampleRequirement){
        # Geomean Hit
        if(unique(z$geomeanCriteriaHit) == TRUE){
          `Geomean Assessment` <- paste('Geomean: ', format(unique(z$E.COLI_geomean), digits = 3), 
                                        ' | Impaired: geomean exceeds criteria in the 90-day period', sep='')  
        } else{
          `Geomean Assessment` <-  paste('Geomean: ', format(unique(z$E.COLI_geomean), digits = 3), 
                                         ' | Geomean criteria met, hold assessment decision for further testing', sep= '')} 
      } else { # minimum geomean sampling requirements NOT met in 90 day period
        `Geomean Assessment` <- 'Insufficient Information: geomean sampling criteria not met'  }
      
      out[i,] <-  tibble(`StationID` = unique(x2$FDT_STA_ID),
                         `Date Window Starts` = time1, `Date Window Ends` = timePlus89, 
                         `Samples in 90 Day Window` = unique(z$nSamples), 
                         `STV Exceedances In Window` = nSTVhitsInWindow, 
                         `STV Exceedance Rate` = STVexceedanceRate,
                         `STV Assessment` = `STV Assessment`,
                         `Geomean In Window` = ifelse(unique(z$nSamples) >= sampleRequirement, unique(z$E.COLI_geomean), NA), # avoid excitement, only give geomean result if 10+ samples
                         `Geomean Assessment` = `Geomean Assessment`,
                         associatedData = list(z)) 
    } #end for loop
  } else {
    return(tibble(`StationID` = unique(x$FDT_STA_ID),
                  `Date Window Starts` = as.Date(NA), 
                  `Date Window Ends` = as.Date(NA), 
                  `Samples in 90 Day Window` = as.numeric(NA), 
                  `STV Exceedances In Window` = as.numeric(NA), 
                  `STV Exceedance Rate` = as.numeric(NA), 
                  `STV Assessment` = as.character(NA),
                  `Geomean In Window` = as.numeric(NA),
                  `Geomean Assessment` = as.character(NA),
                  associatedData = list(NA)))
  }
  
  
  return(out) 
}

#y <- bacteriaExceedances_NEW(stationData, 'E.COLI', 'ECOLI_RMK', 10, 410, 126)



#Function to see if any 90 day windows have 2+ STV exceedances
STVexceedance <- function(df, STV){
  morethan1STVexceedanceInAnyWindow <- filter(df, `STV Exceedances In Window` >= 2)
  if(nrow(morethan1STVexceedanceInAnyWindow) > 0){
    return('| 2 or more STV exceedances in a 90 day window |')
  }
}



# Function to summarize bacteria assessment results into decisions
# This function returns all potential issues with priory on geomean results IF there
# are enough samples to run geomean
# Round to even rules are applied
bacteriaAssessmentDecision <- function(x, # input dataframe with bacteria data
                                       bacteriaField, # name of bacteria field data
                                       bacteriaRemark, # name of bacteria comment field
                                       sampleRequirement, # minimum n samples in 90 day window needed to apply geomean
                                       STV, # unique for ecoli/enter
                                       geomeanCriteria # unique for ecoli/enter
){
    # Rename output columns based on station table template
    stationTableName <- ifelse(bacteriaField == 'E.COLI', "ECOLI", "ENTER")
    
    nSamples <- select(x,  Value = {{ bacteriaField }} ) %>% 
      filter(!is.na(Value)) # total n samples taken in assessment window
    
    
    if(nrow(nSamples) > 0){ # only proceed through decisions if there is data to be analyzed

    # Run assessment function
    z <- suppressWarnings(bacteriaExceedances_NEW(x, bacteriaField, bacteriaRemark, sampleRequirement, STV, geomeanCriteria)   )
    exceedSTVn <- select(x,  Value = {{ bacteriaField }} ) %>% 
      filter(Value > STV) # total STV exceedances in dataset
    exceedSTVrate <- filter(z, `STV Exceedance Rate` > 10)
    exceedGeomean <- filter(z, `Geomean In Window` > geomeanCriteria)
    
    # Decision logic time, work through geomean first and if there is no appropriate geomean data (no windows with 10+ samples)
    #   then go to STV assessment
    
    # Were at least 10 samples taken within any 90-day period of the assessment window?
    if( any(!is.na(z$`Geomean In Window`)) ){ # Were at least 10 samples taken within any 90-day period of the assessment window? - Yes
      # Do the geometric means calculated for the 90-day periods represented by 10+ samples meet the GM criterion?
      if( nrow(exceedGeomean) == 0){ # Do the geometric means calculated for the 90-day periods represented by 10+ samples meet the GM criterion? - Yes
        # Do any of the 90-day periods of the assessment window represented in the dataset exceed the 10% STV Exceedance Rate?
        if( nrow(exceedSTVn) > 0){ # Do any of the 90-day periods of the assessment window represented in the dataset exceed the 10% STV Exceedance Rate? - Yes
          
          # Yes, in a 90-day period represented by 10+ samples
          if(nrow(filter(exceedSTVrate, `Samples in 90 Day Window` >= 10 & `STV Exceedance Rate` > 10)) > 0){ # STV exceedances in a 90-day period represented by >= 10 samples
            return(tibble(StationID = unique(z$StationID),
                          `_EXC` = nrow(exceedSTVn), # right now this is set to # total STV exceedances, not the # STV exceedances in a 90-day period with 10+ samples
                          `_SAMP` = nrow(nSamples), 
                          `_GM_EXC` = nrow(exceedGeomean),
                          `_GM_SAMP` = nrow(filter(z, !is.na(`Geomean In Window`))),
                          `_STAT` = "IM",
                          `_STAT_VERBOSE` = "Impaired- STV exceedances in a 90-day period represented by >= 10 samples after verifying geomean passes where applicable",
                          associatedDecisionData = list(z) ) %>%
                     rename_with( ~ gsub("_", paste0(stationTableName,"_"), .x, fixed = TRUE)) ) # fix names to match station table format
            
          } else {  # STV exceedances in a 90-day period represented by < 10 samples
            
            # 2 or more hits in the same 90-day period?
            if(any(z$`STV Exceedances In Window` >= 2) ){
              return(tibble(StationID = unique(z$StationID),
                            `_EXC` = nrow(exceedSTVn), # right now this is set to # total STV exceedances, not the # STV exceedances in a 90-day period with 10+ samples
                            `_SAMP` = nrow(nSamples), 
                            `_GM_EXC` = nrow(exceedGeomean),
                            `_GM_SAMP` = nrow(filter(z, !is.na(`Geomean In Window`))),
                            `_STAT` = "IM",
                            `_STAT_VERBOSE` = "Impaired- 2 or more STV hits in the same 90-day period after verifying geomean passes where applicable",
                            associatedDecisionData = list(z) ) %>%
                       rename_with( ~ gsub("_", paste0(stationTableName,"_"), .x, fixed = TRUE)) ) # fix names to match station table format
              } else { # 1 hit in one or multiple 90-day periods after verifying geomean passes where applicable
                return(tibble(StationID = unique(z$StationID),
                              `_EXC` = nrow(exceedSTVn), # right now this is set to # total STV exceedances, not the # STV exceedances in a 90-day period with 10+ samples
                              `_SAMP` = nrow(nSamples), 
                              `_GM_EXC` = nrow(exceedGeomean),
                              `_GM_SAMP` = nrow(filter(z, !is.na(`Geomean In Window`))),
                              `_STAT` = "OE",
                              `_STAT_VERBOSE` = "Fully Supporting with an observed effect- 1 STV hit in one or multiple 90-day periods after verifying geomean passes where applicable",
                              associatedDecisionData = list(z) ) %>%
                         rename_with( ~ gsub("_", paste0(stationTableName,"_"), .x, fixed = TRUE)) ) # fix names to match station table format
                }
          }
          
        } else {  # Do any of the 90-day periods of the assessment window represented in the dataset exceed the 10% STV Exceedance Rate? - No
          return(tibble(StationID = unique(z$StationID),
                        `_EXC` = nrow(exceedSTVn), # right now this is set to # total STV exceedances, not the # STV exceedances in a 90-day period with 10+ samples
                        `_SAMP` = nrow(nSamples), 
                        `_GM_EXC` = nrow(exceedGeomean),
                        `_GM_SAMP` = nrow(filter(z, !is.na(`Geomean In Window`))),
                        `_STAT` = "S",
                        `_STAT_VERBOSE` = "Fully Supporting - No STV exceedances or geomean exceedances",
                        associatedDecisionData = list(z) ) %>%
                   rename_with( ~ gsub("_", paste0(stationTableName,"_"), .x, fixed = TRUE)) ) # fix names to match station table format
          }
        
      } else { # Do the geometric means calculated for the 90-day periods represented by 10+ samples meet the GM criterion? - No
        return(tibble(StationID = unique(z$StationID),
                      `_EXC` = nrow(exceedSTVn), # right now this is set to # total STV exceedances, not the # STV exceedances in a 90-day period with 10+ samples
                      `_SAMP` = nrow(nSamples), 
                      `_GM_EXC` = nrow(exceedGeomean),
                      `_GM_SAMP` = nrow(filter(z, !is.na(`Geomean In Window`))),
                      `_STAT` = "IM",
                      `_STAT_VERBOSE` = "Impaired- geomean exceedance(s)",
                      associatedDecisionData = list(z) ) %>%
                 rename_with( ~ gsub("_", paste0(stationTableName,"_"), .x, fixed = TRUE)) ) # fix names to match station table format
        }
      
    } else { # Were at least 10 samples taken within any 90-day period of the assessment window? - No
      # Were there any hits of the STV during the dataset?
      if( nrow(exceedSTVn) == 0){ # Were there any hits of the STV during the dataset? - No
        return(tibble(StationID = unique(z$StationID),
                      # not quite right yet
                      `_EXC` = nrow(exceedSTVn), # right now this is set to # total STV exceedances, not the # STV exceedances in a 90-day period with 10+ samples
                      `_SAMP` = nrow(nSamples), 
                      `_GM_EXC` = nrow(exceedGeomean),
                      `_GM_SAMP` = nrow(filter(z, !is.na(`Geomean In Window`))),
                      `_STAT` = "IN", # is this the right code???
                      `_STAT_VERBOSE` = "Insufficient Information (Prioritize for follow up monitoring)- 0 STV hits but insufficient data to analyze geomean",
                      associatedDecisionData = list(z) ) %>%
                 rename_with( ~ gsub("_", paste0(stationTableName,"_"), .x, fixed = TRUE)) ) # fix names to match station table format
        } else { # Were there any hits of the STV during the dataset? - Yes
          # 2 or more hits in the same 90-day period
          if(any(z$`STV Exceedances In Window` >= 2) ){
            return(tibble(StationID = unique(z$StationID),
                          # not quite right yet
                          `_EXC` = nrow(exceedSTVn), # right now this is set to # total STV exceedances, not the number of STV exceedances in a 90-day period with 10+ samples
                          `_SAMP` = nrow(nSamples), 
                          `_GM_EXC` = nrow(exceedGeomean),
                          `_GM_SAMP` = nrow(filter(z, !is.na(`Geomean In Window`))),
                          `_STAT` = "IM", # is this the right code???
                          `_STAT_VERBOSE` = "Impaired- 2 or more STV hits in the same 90-day period",
                          associatedDecisionData = list(z) ) %>%
                     rename_with( ~ gsub("_", paste0(stationTableName,"_"), .x, fixed = TRUE)) ) # fix names to match station table format
          } else { 
            # 1 hit in one or multiple 90-day periods
            return(tibble(StationID = unique(z$StationID),
                          `_EXC` = nrow(exceedSTVn), # right now this is set to # total STV exceedances, not the # STV exceedances in a 90-day period with 10+ samples
                          `_SAMP` = nrow(nSamples), 
                          `_GM_EXC` = nrow(exceedGeomean),
                          `_GM_SAMP` = nrow(filter(z, !is.na(`Geomean In Window`))),
                          `_STAT` = "IN", # is this the right code???
                          `_STAT_VERBOSE` = "Insufficient Information (Prioritize for follow up monitoring)- 1 STV hit in one or multiple 90-day periods but insufficient data to analyze geomean",
                          associatedDecisionData = list(z) ) %>%
                     rename_with( ~ gsub("_", paste0(stationTableName,"_"), .x, fixed = TRUE)) ) # fix names to match station table format
            }
        }
      }
    # No bacteria data to analyze
    } else {
      return(tibble(StationID = unique(x$FDT_STA_ID),
                    `_EXC` = 0, # right now this is set to # total STV exceedances, not the # STV exceedances in a 90-day period with 10+ samples
                    `_SAMP` = 0, 
                    `_GM_EXC` = 0,
                    `_GM_SAMP` = 0,
                    `_STAT` = NA, # is this the right code???
                    `_STAT_VERBOSE` = NA, 
                    associatedDecisionData = list(NA)) %>%
               rename_with( ~ gsub("_", paste0(stationTableName,"_"), .x, fixed = TRUE)) )
    }
  }

# To get just info for station table  
#xxx <- bacteriaAssessmentDecision(stationData, 'E.COLI', 'ECOLI_RMK', 10, 410, 126) %>%
#  dplyr::select(StationID:ECOLI_STAT)
#xxx <- bacteriaAssessmentDecision(stationData, 'ENTEROCOCCI', 'RMK_31649', 10, 130, 35) %>%
#  dplyr::select(StationID:ENTER_STAT)
