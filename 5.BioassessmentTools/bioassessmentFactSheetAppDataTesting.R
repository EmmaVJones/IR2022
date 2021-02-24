source('global.R')

# Get data from bio
IR2020 <- read_excel('data/BioassessmentRegionalResultsIR2020.xlsx') %>%
  filter(!is.na(FinalAssessmentRating))

#IR2022 <- read_excel('data/BioassessmentRegionalResultsIR2022test.xlsx')
userUpload <- IR2022[2:5,]

# check against pinned data, overwrite if necessary
# this is the original pin in case it needs to be reset during testing
#pin(IR2022[1:3,], name = 'IR2022bioassessmentDecisions_test', description = 'Test dataset for developing IR2022 bioassessment fact sheet tool', board = 'rsconnect') # original pin

pinCheck('IR2022bioassessmentDecisions_test', userUpload) # can change to real deal pin in time

# pull new pin
pinnedDecisions <- pin_get('IR2022bioassessmentDecisions_test', board = 'rsconnect')

# Allow users to choose which stations to run report on (start with what was just uploaded, then whatever else is available from pin)
userStationChoice <- bind_rows(filter(pinnedDecisions, StationID %in% userUpload$StationID),
                               filter(pinnedDecisions, ! StationID %in% userUpload$StationID)) %>%
  dplyr::select(StationID) %>% pull() 

# organize benthic and habitat data for chosen stations to send to report
assessmentDecision_UserSelection <- filter(pinnedDecisions, StationID %in% userStationChoice)

SCI_UserSelection <- filter(VSCIresults, StationID %in% filter(assessmentDecision_UserSelection, AssessmentMethod == 'VSCI')$StationID) %>%
  bind_rows(
    filter(VCPMI63results, StationID %in% filter(assessmentDecision_UserSelection, AssessmentMethod == 'VCPMI + 63')$StationID)  ) %>%
  bind_rows(
    filter(VCPMI65results, StationID %in% filter(assessmentDecision_UserSelection, AssessmentMethod == 'VCPMI - 65')$StationID)  ) %>%
  # add back in description information
  left_join(filter(benSamps, StationID %in% userStationChoice) %>%
              dplyr::select(StationID, Sta_Desc, BenSampID,US_L3CODE, US_L3NAME, HUC_12, VAHU6, Basin, Basin_Code),
            by = c('StationID', 'BenSampID')) %>%
  dplyr::select(StationID, Sta_Desc, BenSampID, `Collection Date`, RepNum, everything())

habitatUserSelection <- habitatConsolidation( userStationChoice, habSamps, habValues)

# reorganize to only send individual station data to report
#forReport <- list()
for(i in unique(assessmentDecision_UserSelection$StationID)){
  # dat <- list(assessmentDecision = as_tibble(filter(assessmentDecision_UserSelection, StationID %in% i)),
  #                   SCI = filter(SCI_UserSelection, StationID %in% i),
  #                   habitat = filter(habitatUserSelection, StationID %in% i))
  # forReport[i] <- list(dat)
  #myStation <- stationID[i]  # my species - to be reused as 1) parameter & 2) file name
  render("bioassessmentFactSheet.Rmd", # the template
         params = list(assessmentDecision = as_tibble(filter(assessmentDecision_UserSelection, StationID %in% i)),
                       SCI = filter(SCI_UserSelection, StationID %in% i),
                       habitat = filter(habitatUserSelection, StationID %in% i)),
         output_file = paste(i, '.html', sep = ''), # name of the output file - species name and pdf extension
         quiet = T,
         encoding = 'UTF-8')
}



View(forReport)

forReport$`2BBUF006.39`


forReport$assessmentDecision

forReport$StationID



# run report

