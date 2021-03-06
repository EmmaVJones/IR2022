# ui
tabPanel('Data Upload',
         h3('Tool Overview'),
         p("The Lacustrine Assessment Tool is designed to expedite analysis, assessment
                                            decisions, and quality assurance/quality control (QA/QC) procedures for Virginia's
                                            2022 Integrated Report (IR). The data window analyzed covers 
                                            January 1, 2015 to December 31, 2020. Users can expect significant time savings
                                            on repetitive procedures including: raw data organization from disparate databases, 
                                            geospatial organization of stations by assessment unit, standard/criteria calculations, 
                                            and data visualization."),
         p('This application represents the third iteration of an automated assessment tool. The datasets
                                            and parameters chosen for analysis readily lend themselves to automated processing. Future versions
                                            of the tool may include additional datasets and analyses.'),
         p('For feedback, troubleshooting, and questions regarding analyses or missing data, please contact 
                                            Emma Jones (emma.jones@deq.virginia.gov)'),
         br(),
         h3('Tool Inputs'),br(),
         h4('Prepopulated Tool Inputs'),
         p("Some data sources are compiled for users and programmed into the app, requiring no user manipulation.
                                            These datasets include: "),
         tags$ul(
           tags$li('Conventionals- CEDS data pulled and analyzed by Roger Stewart (roger.stewart@deq.virginia.gov) for each 
                                            Integrated Report data window.'), 
           tags$li("Statewide Assessment (spatial) layer- The spatial dataset that identifies each regional office's watersheds.")),
         br(),
         h4('User Defined Tool Inputs'),
         p('In order to allow for more flexible assessment unit updates as required throughout the assessment process,
                                             users must upload certain datasets that follow a specified template. These include their regional
                                             Stations Table (generated by the automated assessment scripts) and Regional Assessment Unit shapefiles.'),
         h5('Stations Table- Generated by the Automated Assessment Tool'),
         helpText('This dataset is derived before any Lacustrine Assessment Tool analysis 
                                                   procedures can commence using information provided by assessors from the ',
                  span(strong('Regional Assessment Metadata Validation Tool.')), 
                  'After completing the necessary WQS and AU attribution steps overviewed in the ',
                  span(strong('Regional Assessment Metadata Validation Tool')),'once, users are provided
                                                   an initial automated assessment (Stations Table) to upload to this application and/or
                                                   the ',span(strong('CEDS WQA Stations Table Bulk Upload')), ' tool. Should any assessment 
                                                   units change throughout the assessment process, users have the ability to update their local
                                                   Stations Table and Regional Assessment Units spatial layers with the required changes and 
                                                   upload those new datasets to this application. With that updated information, this tool will
                                                   reflect the changes to the user. This process may be repeated as many times as is necessary 
                                                   throughout the assessment process. After stations and AUs are reviewed, users may upload
                                                   their Stations Table to the ', span(strong('CEDS WQA Stations Table Bulk Upload')), ' tool.'),
         helpText(strong('A note on Inversioning spatial data: '),"If assessment unit changes are necessary throughout
                                                   the course of the assessment process, the user should update their local AU spatial dataset, sync 
                                                   these changes to the statewide spatial datasest, and export a local copy of this updated spatial
                                                   dataset for use in the assessment applications. Users should also alter the assessment unit name
                                                   attributed to the station(s) in the Station Table to ensure the assessment applications can appropriately
                                                   reorganize data according to the user's changes."),
         fileInput('stationsTable','Upload your Regional Stations Table.', accept = c(".csv")),
         helpText('If this is your first time using the tool, please download a copy of the latest automated station
                                                   table output for upload to the tool. You may manipulate this .csv to only reflect your specific region.'),
         fluidRow(
           downloadButton('downloadTemplate',"Download statewide example dataset to upload to the tool."),
           uiOutput('templateLastUpdated_')),
         br(),
         h5('Regional Assessment Units'),
         helpText(span('This shapefile is the current working copy of the regional assessment units.',
                       strong('It will be uploaded to the app on startup for you to expedite application rendering
                                                        time.'), ' Any changes to the regional dataset (e.g. split an assessment 
                                                        unit) should be synced with the statewide version and a copy of the ', strong('new spatial 
                                                        dataset should be sent to Emma Jones (emma.jones@deq.virginia.gov) to update on the server.')))
         #h6(strong('To view or change the location of the Regional Assessment Units shapefile sourced
         #          by the application, see the AUshapefileLocation.R script.'))
         #fileInput('regionalAUshapefile','Choose your Regional Assessment Unit shapefile.',
         #          accept = c(".shp",#)),# only need .shp for st_read 
         #                     ".dbf",".prj",".sbn",".sbx","shp.xml",".shx"), multiple = T),
),









# server


# real
stationTable <- reactive({
  req(input$stationsTable)
  inFile <- input$stationsTable
  stationTable <- read_csv(inFile$datapath,
                           col_types = cols(COMMENTS = col_character(),
                                            LACUSTRINE = col_character())) %>% # force to character bc parsing can incorrectly guess logical based on top 1000 rows
    #fix periods in column names from excel
    as_tibble() %>%
    filter_at(vars(starts_with('TYPE')), any_vars(. == 'L')) %>% # keep only lake stations
    # add WQS information to stations
    left_join(WQSlookup, by = c('STATION_ID'='StationID')) %>%
    mutate(CLASS_BASIN = paste(CLASS,substr(BASIN, 1,1), sep="_")) %>%
    mutate(CLASS_BASIN = ifelse(CLASS_BASIN == 'II_7', "II_7", as.character(CLASS))) %>%
    # Fix for Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard)
    left_join(WQSvalues, by = 'CLASS_BASIN') %>%
    dplyr::select(-c(CLASS.y,CLASS_BASIN)) %>%
    rename('CLASS' = 'CLASS.x') %>%
    left_join(dplyr::select(WQMstationFull, WQM_STA_ID, EPA_ECO_US_L3CODE, EPA_ECO_US_L3NAME) %>%
                distinct(WQM_STA_ID, .keep_all = TRUE), by = c('STATION_ID' = 'WQM_STA_ID')) %>% # last cycle had code to fix Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard) but not sure if necessary
    lakeNameStandardization() %>% # standardize lake names
    
    # extra special step
    mutate(Lake_Name = case_when(STATION_ID %in% c('2-TRH000.40') ~ 'Thrashers Creek Reservoir',
                                 TRUE ~ as.character(Lake_Name))) %>%
    
    
    left_join(lakeNutStandards, by = c('Lake_Name')) %>%
    # lake drummond special standards
    mutate(`Chlorophyll a (ug/L)` = case_when(Lake_Name %in% c('Lake Drummond') ~ 35,
                                              TRUE ~ as.numeric(`Chlorophyll a (ug/L)`)),
           `Total Phosphorus (ug/L)` = case_when(Lake_Name %in% c('Lake Drummond') ~ 40,
                                                 TRUE ~ as.numeric(`Total Phosphorus (ug/L)`))) %>%
    mutate(lakeStation = TRUE)
  return(stationTable)  })

#######################################
# for testing
stationTable <- reactive({
  read_csv('userDataToUpload/processedStationData/stationTableResults.csv',
           col_types = cols(COMMENTS = col_character())) %>%# force to character bc parsing can incorrectly guess logical based on top 1000 rows
    filter_at(vars(starts_with('TYPE')), any_vars(. == 'L')) %>% # keep only lake stations
    # add WQS information to stations
    left_join(WQSlookup, by = c('STATION_ID'='StationID')) %>%
    mutate(CLASS_BASIN = paste(CLASS,substr(BASIN, 1,1), sep="_")) %>%
    mutate(CLASS_BASIN = ifelse(CLASS_BASIN == 'II_7', "II_7", as.character(CLASS))) %>%
    # Fix for Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard)
    left_join(WQSvalues, by = 'CLASS_BASIN') %>%
    dplyr::select(-c(CLASS.y,CLASS_BASIN)) %>%
    rename('CLASS' = 'CLASS.x') %>%
    left_join(dplyr::select(WQMstationFull, WQM_STA_ID, EPA_ECO_US_L3CODE, EPA_ECO_US_L3NAME) %>%
                distinct(WQM_STA_ID, .keep_all = TRUE), by = c('STATION_ID' = 'WQM_STA_ID')) %>% # last cycle had code to fix Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard) but not sure if necessary
    lakeNameStandardization() %>% # standardize lake names
    left_join(lakeNutStandards, by = c('Lake_Name'))
}) #for testing
#######################################



# real
regionalAUs <- reactive({ 
  req(input$pullAUs)
  withProgress(message = 'Reading in Large Spatial File',
               st_zm(st_as_sf(pin_get('AUreservoir_EVJ', board = 'rsconnect')) ) %>%
                 lakeNameStandardization()) })  