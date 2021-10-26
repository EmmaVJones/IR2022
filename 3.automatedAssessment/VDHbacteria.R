#this script reorganizes the VDH bacteria data and assessess for regions

VDH <- read_excel('data/final2022data/Citizen Monitoring Data/22IR VDH Beach Data_2019_2020.xlsx',
                sheet = 'VDH Beach Enterococci') %>% 
  mutate(#FDT_DATE_TIME = paste())
    LEVEL_ENTEROCOCCI = 'Level III')


# make sure all data is accepted first
unique(VDH$ResultStatusIdentifier )
# simplify data for analysis, will join full dataset back later
VDHsimple <- bind_rows(dplyr::select(VDH, FDT_STA_ID = MonitoringLocationIdentifier,
                           FDT_DATE_TIME = ActivityStartDate,
                           ENTEROCOCCI = ResultMeasureValue, 
                           LEVEL_ENTEROCOCCI),
                       # Kristie organized this version for run in Sept 2021
                       read_csv('data/final2022data/Citizen Monitoring Data/2020IR_VDHdatafor 22IR.csv') %>%  
                         mutate(FDT_DATE_TIME = as.Date(`Date time`, format = '%m/%d/%Y')) %>% 
                         dplyr::select(FDT_STA_ID = `Station ID`, FDT_DATE_TIME, 
                                       ENTEROCOCCI = `Enterococcus per 100ml`, 
                                       LEVEL_ENTEROCOCCI = `Enterococcus Method`) ) %>% 
  mutate(ENTEROCOCCI = na_if(ENTEROCOCCI, 0)) # force 0's to 1 so geomean function can work properly


stationTableResults <- stationsTemplate %>% 
  dplyr::select(STATION_ID, ENTER_EXC:ENTER_STAT)


for(i in 1:length(unique(VDHsimple$FDT_STA_ID))){
  print(paste('Assessing station', i, 'of', length(unique(VDHsimple$FDT_STA_ID)), sep=' '))
  
  stationData <- filter(VDHsimple, FDT_STA_ID %in% unique(VDHsimple$FDT_STA_ID)[i])
  
  if(nrow(stationData) > 0){
    results <- bacteriaAssessmentDecision(stationData, 'ENTEROCOCCI', 'LEVEL_ENTEROCOCCI', 10, 130, 35) %>% 
      dplyr::select(-associatedDecisionData)%>% 
      rename('STATION_ID' = 'StationID',
             'ENTER_BACTERIADECISION' = 'BACTERIADECISION', 'ENTER_BACTERIASTATS' = 'BACTERIASTATS')
    stationTableResults <- bind_rows(stationTableResults,results)
  }
  
  
}

stationTableResults <- left_join(VDH %>% 
                                    distinct(MonitoringLocationIdentifier, .keep_all = T) %>% 
                                    dplyr::select(OrganizationIdentifier:ActivityMediaName, ProjectIdentifier:ResultSampleFractionText,
                                                  ResultMeasureUnitCode:ProviderName),
                                  stationTableResults, by = c('MonitoringLocationIdentifier' = 'STATION_ID'))

write_csv(stationTableResults,'dataForAssessors/VDHbacteriaResults.csv', na = "") # dont write out a character NA in csv

write_csv(stationTableResults,'dataForAssessors/VDHbacteriaResults2015-2020.csv', na = "") # dont write out a character NA in csv




# station review


x <- filter(VDHsimple, FDT_STA_ID  == '21VABCH-VA582379')
x <- filter(VDHsimple, FDT_STA_ID  == '21VABCH-VA532597')
bacteriaAssessmentDecision(x, 'ENTEROCOCCI', 'LEVEL_ENTEROCOCCI', 10, 130, 35)
bacteriaField <- 'ENTEROCOCCI'
bacteriaRemark <- 'LEVEL_ENTEROCOCCI'
sampleRequirement <- 10
STV <- 130
geomeanCriteria <- 35

z1 <- z
z2 <- z

write.csv(bind_rows(z1, z2) %>% 
  dplyr::select(-associatedData), 'dataForAssessors/VDHsnippet.csv', row.names = F)
