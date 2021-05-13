# build metals assessment
# run HowToRunAutomatedAssessment2022.Rmd through line 203
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


# Bring in pinned metals data and reorganize to make further analysis easier
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

glimpse(WCmetalsForAnalysis)
  

# Single Station Metals Analysis Workflow

#2-BGC008.10 no hardness after lab codes removed so good tester
# 6BCLN279.43 no data 6/24/2015 after lab codes removed and tons of data

# get one station metals data for assessment
station <- '6BCLN279.43'#'2-BGC008.10'#'8-CON005.38'#'2-JKS028.69'
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
stationMetalsData <- filter(WCmetalsForAnalysis, Station_Id %in% station)

singleStationMetalsResults <- metalsAnalysis(stationMetalsData, stationData, WER=1)





# mulitstation Metals Analysis Workflow, probably won't use this but just in case

# make a place to store raw analysis results
metalsAnalysisResults <- tibble(Station_Id = as.character(NA), WindowDateTimeStart = as.POSIXct(NA), FDT_DEPTH = as.numeric(NA),
                   CLASS = as.factor(NA), PWS = as.factor(NA), ZONE = as.factor(NA), `Assess As` = as.character(NA),
                   Metal = as.character(NA), ValueType = as.character(NA), Value = as.numeric(NA), Criteria = as.character(NA), 
                   `Criteria Type` = as.character(NA), Waterbody = as.character(NA), CriteriaValue = as.numeric(NA),
                   parameterRound = as.numeric(NA), Exceedance = as.numeric(NA))

for(i in 1:nrow(stationTable)){
  stationData <- filter(conventionals, FDT_STA_ID %in% stationTable$STATION_ID[i]) %>%
    left_join(stationTable, by = c('FDT_STA_ID' = 'STATION_ID')) %>%
    pHSpecialStandardsCorrection() %>% # correct pH to special standards where necessary
    # special lake steps
    {if(stationTable$STATION_ID[i] %in% lakeStations$STATION_ID)
      suppressWarnings(suppressMessages(
        mutate(., lakeStation = TRUE) %>%
          thermoclineDepth())) # adds thermocline information and SampleDate
      else mutate(., lakeStation = FALSE) }
  stationMetalsData <- filter(WCmetalsForAnalysis, Station_Id %in% stationTable$STATION_ID[i])
  # only run analysis if necessary
  if(nrow(stationMetalsData) > 0){
    print(paste('Assessing metals for site:', i ,'in', nrow(stationTable)))
    metalsAnalysisResults <- bind_rows(metalsAnalysisResults, metalsAnalysis(stationMetalsData, stationData, WER=1))
  } 
}

metalsAssessment <- metalsAssessmentFunction(metalsAnalysisResults)
  
metJKS28 <- metalsAnalysis(stationMetalsData, stationData, WER=1)
View(metalsAssessmentFunction(metJKS28))





# Build Assessment Visualization
library(plotly)


staticLimit <- c("Antimony PWS", "Antimony All Other Surface Waters", "Arsenic Acute Freshwater", "Arsenic Chronic Freshwater", "Arsenic PWS",
                 "Arsenic Acute Saltwater", "Arsenic Chronic Saltwater", "Barium PWS","Cadmium PWS","ChromiumIII PWS",
                 "ChromiumVI Acute Freshwater", "ChromiumVI Chronic Freshwater", "ChromiumVI Acute Saltwater", "ChromiumVI Chronic Saltwater", 
                 "Lead PWS", "Mercury Acute Freshwater", "Mercury Chronic Freshwater", "Mercury Acute Saltwater", "Mercury Chronic Saltwater",
                 "Nickel PWS",  "Nickel All Other Surface Waters", "Uranium PWS","Selenium Acute Freshwater", "Selenium Chronic Freshwater", 
                 "Selenium PWS", "Selenium All Other Surface Waters","Thallium PWS", "Thallium All Other Surface Waters","Zinc PWS", 
                 "Zinc All Other Surface Waters")

criteriaSelection <- 'Cadmium Acute Freshwater'#'Antimony All Other Surface Waters'#'Zinc Chronic Freshwater'#'Cadmium Acute Freshwater'#'Antimony All Other Surface Waters'

dat <- filter(singleStationMetalsResults, Criteria ==  criteriaSelection) %>% 
  filter(Value != 'NaN') # drop any unmeasured values
dat$SampleDate <- as.POSIXct(dat$WindowDateTimeStart, format="%m/%d/%y")
plot_ly(data=dat) %>%
  {if(criteriaSelection %in% staticLimit)
    add_markers(., x= ~SampleDate, y= ~Value,mode = 'scatter', name=~Metal, marker = list(color= '#535559'),
              hoverinfo="text",text=~paste(sep="<br>",
                                           paste("StationID: ",Station_Id),
                                           paste("Date: ",SampleDate),
                                           paste("Depth: ",FDT_DEPTH, "m"),
                                           paste(Metal,":",Value, "ug/L"),
                                           paste('Static Criteria:', CriteriaValue, "ug/L"))) %>%
      add_lines(data=dat, x=~SampleDate,y=~CriteriaValue, mode='line', line = list(color = '#484a4c',dash = 'dot'),
                hoverinfo = "text", text= ~paste(criteriaSelection, "Criteria:",  CriteriaValue, "ug/L"), name="Static Criteria") 
    else add_markers(., data=dat, x= ~SampleDate, y= ~Value, mode = 'scatter', name=~Metal, marker = list(color= ~Exceedance), colors = c('#535559', 'red'), #color= ~Exceedance, #colors = c('#535559', 'red'),#marker = list(color= '#535559'),
                     symbol =  ~Exceedance, symbols = c(16,15), 
                     hoverinfo="text",text=~paste(sep="<br>",
                                                  paste("StationID: ",Station_Id),
                                                  paste("Date: ",SampleDate),
                                                  paste("Depth: ",FDT_DEPTH, "m"),
                                                  paste(Metal,":",Value, "ug/L"),
                                                  paste('Hardness Based Criteria:', CriteriaValue, "ug/L")))       } %>%
  layout(showlegend=FALSE,
         yaxis=list(title=paste(stringr::word(criteriaSelection, 1), "ug/L")),#"E. coli (CFU / 100 mL)"),
         xaxis=list(title="Sample Date",tickfont = list(size = 10))) 
