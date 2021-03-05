source('global.R')

# Get data from bio
IR2020 <- read_excel('data/BioassessmentRegionalResultsIR2020.xlsx') %>%
  filter(!is.na(FinalAssessmentRating))

###########IR2022 <- read_excel('data/BioassessmentRegionalResultsIR2022test.xlsx')
#IR2022 <- read_excel('data/BioassessmentRegionalResultsIR2022test.xlsx')

# check against pinned data, overwrite if necessary
# this is the original pin in case it needs to be reset during testing
#pin(IR2022[1:3,], name = 'IR2022bioassessmentDecisions_test', description = 'Test dataset for developing IR2022 bioassessment fact sheet tool', board = 'rsconnect') # original pin

# original list of what's available on the server
#OGpinnedDecisions <- pin_get('IR2022bioassessmentDecisions_test', board = 'rsconnect')

# what user uploads
userUpload <- IR2022[2:5,]
userUploadFail <- mutate(userUpload, StationID = case_when(StationID == "2-CAT026.29" ~ "2-CAT026.00", 
                                                           TRUE ~ as.character(StationID)))

# make sure all uploaded stations are valid
userUploadValid <- stationValidation(userUploadFail)

# display any stations that cannot undergo more work
invalidInputData <- filter(userUpload, ! StationID %in% userUploadValid$StationID)

DT::datatable(mutate(userUpload, invalidData = case_when(StationID %in% invalidInputData$StationID ~ 1,
                                                         TRUE ~ 0)),
              escape=F, rownames = F, options=list(scrollX = TRUE, scrollY = "800px",pageLength=nrow(userUpload))) %>%
  formatStyle('StationID', 'invalidData', backgroundColor = styleEqual(c(0, 1), c(NA, 'yellow'))  )

pinCheck('IR2022bioassessmentDecisions_test', userUploadValid) # can change to real deal pin in time

# pull new pin
#pinnedDecisions <- pin_get('IR2022bioassessmentDecisions_test', board = 'rsconnect') # new list of what's available to make a report
if(!exists('userUpload') ){#is.null(inputFile())){
  pinnedDecisions <- pin_get('IR2022bioassessmentDecisions_test', board = 'rsconnect')
} else {
  pinCheck('IR2022bioassessmentDecisions_test', userUploadValid) # can change to real deal pin in time
  # pull new pin
  pinnedDecisions <- pin_get('IR2022bioassessmentDecisions_test', board = 'rsconnect')
}

# Allow users to choose which stations to run report on (start with what was just uploaded, then whatever else is available from pin)
userStationChoice <- if(exists('userUploadValid')){
  bind_rows(filter(pinnedDecisions, StationID %in% userUploadValid$StationID),
            filter(pinnedDecisions, ! StationID %in% userUploadValid$StationID)) %>%
    dplyr::select(StationID) %>% pull()
} else{unique(pinnedDecisions$StationID)   }

# organize benthic and habitat data for chosen stations to send to report
#if(exists('pinnedDecisions')){
  assessmentDecision_UserSelection <- filter(pinnedDecisions, StationID %in% userStationChoice)
#  } else {assessmentDecision_UserSelection <- filter(OGpinnedDecisions, StationID %in% userStationChoice)}

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

#forReport <- list()
# dat <- list(assessmentDecision = as_tibble(filter(assessmentDecision_UserSelection, StationID %in% i)),
#                   SCI = filter(SCI_UserSelection, StationID %in% i),
#                   habitat = filter(habitatUserSelection, StationID %in% i))
# forReport[i] <- list(dat)


# run report
for(i in unique(assessmentDecision_UserSelection$StationID)){
  render("bioassessmentFactSheet.Rmd", # the template
         # reorganize data from app to send to report
         params = list(assessmentDecision = as_tibble(filter(assessmentDecision_UserSelection, StationID %in% i)),
                       SCI = filter(SCI_UserSelection, StationID %in% i),
                       habitat = filter(habitatUserSelection, StationID %in% i)),
         output_dir = "reports", # drop this for shiny app
         output_file = paste(i, '.html', sep = ''), # name of the output file
         quiet = T,
         encoding = 'UTF-8')
}

i =  "5AASM017.27"

# for report testing
dat <- list(assessmentDecision = as_tibble(filter(assessmentDecision_UserSelection, StationID %in% i)),
            SCI = filter(SCI_UserSelection, StationID %in% i),
            habitat = filter(habitatUserSelection, StationID %in% i))

assessmentDecision1 <- dat$assessmentDecision
SCI1 <- dat$SCI
habitat1 <- dat$habitat


## Plot SCI results
SCIresultsPlot(SCI1, assessmentDecision1)

# SCI Results table
rawBugData(SCI1)

## SCI metrics table
SCImetricsTable(SCI1)
  
## SCI statistics
SCIstatistics(SCI1)
    

## Habitat plot
habitatPlot(habitat1)

# habitat table
habitatDTcoloredTable(habitat1)











### Citizen Fact Sheet

GPuserStation1 <- "2-JKS023.61"#unique(benSamps$StationID)
GPuserSCIMethod1 <- c('VSCI', 'VCPMI + 63', 'VCPMI - 65')[1]

GPbenSamps1 <- filter(benSampsAll, StationID %in% GPuserStation1)

GPuserWindow1 <- c(as.Date('2010-11-03'), as.Date('2017-11-03'))#min(GPbenSamps1$`Collection Date`), max(GPbenSamps1$`Collection Date`))

GPbenSampsFilter1 <- filter(GPbenSamps1, between(as.Date(`Collection Date`), GPuserWindow1[1], GPuserWindow1[2]))

# pull SCI information based on user SCI choice
if(GPuserSCIMethod1 == 'VSCI'){
  GPSCI <- filter(VSCIresultsAll, BenSampID %in% GPbenSampsFilter1$BenSampID) }
if(GPuserSCIMethod1 == 'VCPMI + 63'){
  GPSCI <- filter(VCPMI63resultsAll, BenSampID %in% GPbenSampsFilter1$BenSampID) }
if(GPuserSCIMethod1 == 'VCPMI - 65'){
  GPSCI <- filter(VCPMI65resultsALL, BenSampID %in% GPbenSampsFilter1$BenSampID) }
# add back in description information
GPSCI <- bind_rows(SCItemplate,
                   filter(GPSCI, `Target Count` == 110) %>%
                   left_join(filter(benSamps, BenSampID %in% GPbenSampsFilter1$BenSampID) %>%
                               dplyr::select(StationID, Sta_Desc, BenSampID,US_L3CODE, US_L3NAME, HUC_12, VAHU6, Basin, Basin_Code),
                             by = c('StationID', 'BenSampID')) %>%
                     dplyr::select(StationID, Sta_Desc, BenSampID, `Collection Date`, RepNum, everything()) ) %>%
  drop_na(StationID)
              

habitatUserSelection1 <- habitatConsolidation( GPuserStation1, habSampsAll, habValuesAll)

# make report
render("GPbioassessmentFactSheet.Rmd", # the template
       # reorganize data from app to send to report
       params = list(SCI = GPSCI,
                     habitat = habitatUserSelection1),
       output_dir = "GPreports", # drop this for shiny app
       output_file = paste(unique(GPSCI$StationID), '.docx', sep = ''), # name of the output file
       quiet = T,
       encoding = 'UTF-8')

