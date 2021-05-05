# build metals assessment
# run HowToRunAutomatedAssessment2022.Rmd through line 203

# Metals criteria analysis
metalsCriteriaFunction <- function(Hardness, WER){
  # Remember: ln is really log() in R; exp() is natural antilog in R
  # Per 9VAC25-260-140, criteria to 2 sig figs #https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
  
  # If no WER supplied, use 1
  WER <- ifelse(is.na(WER), 1, WER)
  # Establish Hardness Criteria
  criteriaHardness <- ifelse(Hardness < 25, 25, ifelse(Hardness > 400, 400, Hardness))
  
  metalsCriteria <- suppressWarnings(
    tibble(empty= 'Column', `Antimony PWS` = 5.6, `Antimony All Other Surface Waters` = 640,
           `Arsenic Acute Freshwater` = 340, `Arsenic Chronic Freshwater` = 150, `Arsenic PWS` = 10,
           `Arsenic Acute Saltwater` = 69, `Arsenic Chronic Saltwater` = 36,
           `Barium PWS` = 2000,
           `Cadmium Acute Freshwater` =  signif(WER * exp(0.9789 * (log(criteriaHardness))-3.866) * (1.136672 - (log(criteriaHardness) * 0.041838)), digits = 2),
           `Cadmium Chronic Freshwater` = signif(WER * exp(0.7977 * log(criteriaHardness) - 3.909) * (1.101672 - (log(criteriaHardness) * (0.041838))), digits = 2),
           `Cadmium Acute Saltwater` = signif(33 * WER, digits = 2), `Cadmium Chronic Saltwater` = signif(7.9 * WER, digits = 2), `Cadmium PWS` = 5,
           `ChromiumIII Acute Freshwater` = signif(WER *  (exp(0.8190 * (log(criteriaHardness)) + 3.7256)) * 0.316, digits = 2), 
           `ChromiumIII Chronic Freshwater` = signif(WER *  (exp(0.8190 * (log(criteriaHardness))+0.6848)) * 0.860, digits = 2), `ChromiumIII PWS` = 100,
           `ChromiumVI Acute Freshwater` = 16, `ChromiumVI Chromium Freshwater` = 11, `ChromiumVI Acute Saltwater` = 1100, `ChromiumVI Chronic Saltwater` = 50, 
           # Copper assessment Dropped per Tish email 5/3/21
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
      pivot_longer(!empty, names_to = 'Criteria', values_to = 'Measure') %>% 
      mutate(Criteria2 = Criteria) %>%  #duplicate column to split
      separate(Criteria2, c("Metal", "Criteria Type", "Waterbody"), sep = " ") %>% 
      mutate(`Criteria Type` = ifelse(`Criteria Type` == 'All', 'All Other Waters', `Criteria Type`),
             Waterbody = ifelse(Waterbody == 'Other', NA, Waterbody)) %>% 
      dplyr::select(Metal, Criteria, `Criteria Type`, Waterbody, Measure))
  return(metalsCriteria)
}


# get one station metals data for assessment
station <- '2-JKS028.69'
# pull one station data
stationData <- filter(conventionals, FDT_STA_ID %in% station) %>% #stationTable$STATION_ID[i]) %>%
  left_join(stationTable, by = c('FDT_STA_ID' = 'STATION_ID')) %>%
  pHSpecialStandardsCorrection() %>% # correct pH to special standards where necessary
  # special lake steps
  {if(station %in% lakeStations$STATION_ID)#stationTable$STATION_ID[i] %in% lakeStations$STATION_ID)
    suppressWarnings(suppressMessages(
      mutate(., lakeStation = TRUE) %>%
        thermoclineDepth())) # adds thermocline information and SampleDate
    else mutate(., lakeStation = FALSE) }
stationMetalsData <- filter(WCmetals, Station_Id %in% station)



metalsAssessment <- function(stationMetalsData, stationData, WER){
  # If no WER supplied, use 1
  WER <- ifelse(is.na(WER), 1, WER)
  
  # Get WQS from stationData so correct criteria can be applied
  stationMetalsData <- left_join(stationMetalsData, 
                                 dplyr::select(stationData, FDT_STA_ID, CLASS, PWS, ZONE), by = 'FDT_STA_ID') %>% 
    mutate(`Assess As` = case_when(CLASS == "I" ~ 'Saltwater',
                                   CLASS == "II" & ZONE == 'Estuarine' ~ 'Saltwater',
                                   CLASS == "II" & ZONE == 'Transition' ~ 'More Stringent',
                                   CLASS == "II" & ZONE == 'Tidal Fresh' ~ 'Freshwater',
                                   CLASS %in% c('III', "IV","V","VI","VII") ~ 'Freshwater',
                                   TRUE ~ as.character(NA)))
  
  
  # make a place to store raw chronic data if needed
  fourDayResults <- tibble(`fourDayMetalAvg` = as.numeric(NA),
                           WindowStart = as.POSIXct(NA),
                           `fourDayAvglimit` = as.numeric(NA),
                           fourDayExceedance = as.logical(NA),
                           fourDayWindowData = list())
  
  # loop through each row of data to correctly calculate criteria and find any chronic scenarios
  i <- stationMetalsData$FDT_DATE_TIME[3] # 1 is good test for averaging over acute and chronic windows
  for(i in stationMetalsData$FDT_DATE_TIME){
    acuteDataWindow <- filter(stationMetalsData,  between(FDT_DATE_TIME, i, i + hours(1)))
    chronicDataWindow <- filter(stationMetalsData,  between(FDT_DATE_TIME, i, i + days(4)))
    # Run acute analysis if data exists
    if(nrow(acuteDataWindow) > 0){
      
    }
    
    # Run chronic analysis if data exists
    if(nrow(chronicDataWindow) > 1){
      
    }
  }
  
  # calculate criteria
  criteria <- metalsCriteriaFunction(Hardness, WER)
}



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