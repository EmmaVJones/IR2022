shinyUI(fluidPage(theme="yeti.css",
                  navbarPage("VDEQ Benthic Assessment Fact Sheet Tool",
                             tabPanel("About",
                                      h4('Background'),
                                      p(paste0('This application generates standardized Benthic Fact Sheets for stations within the IR', assessmentCycle,
                                               ' data window. This tool was developed to be used after a regional biologist assesses stations using the '),
                                               span(tagList(a("Bioassessment Dashboard.",
                                                              href="http://deq-rstudio-prod.cov.virginia.gov/rsconnect/IR2022BioassessmentDashboard/", 
                                                              target = 'blank'))), ' Assessment decisions saved on the server feed the Riverine Assessment
                                               application.'),
                                      h4('Usage'),
                                      p('Users may choose from stations that have assessment decisions saved on the R server or
                                        from data uploaded to the application. Once a station is chosen, the `Generate Report` button
                                        compiles benthic and habitat data within the IR window with biologist assessment decisions and writes a report,
                                        downloaded to the users Downloads folder.'),
                                      h4('Contact Information'),
                                      p('Please direct all questions about this application to Emma Jones (emma.jones@deq.virginia.gov) and Jason Hill 
                                        (jason.hill@deq.virginia.gov).')),
                             tabPanel('Report',
                                      sidebarPanel(
                                        helpText("To generate a bioassessment fact sheet, you must choose a station from the list of stations below with
                                                 assessment decisions saved on the R server or upload a spreadsheet of stations with assessment
                                                 decisions."),
                                        uiOutput('userStations_'),
                                        h4('Generate Report for selected station'),
                                        uiOutput('downloadReport_'),
                                        hr(),
                                        h4('(Optional) Data Upload'),
                                        helpText('By uploading data to this application, you are sending assessment decisions on the
                                                 R server. This dataset is shared with regional assessment staff for the assessment process.
                                                 If you upload a station that already exists in the shared dataset, the most recent version will
                                                 overwrite the previous version.'),
                                        fileInput('userData','Upload Assessment Decisions (.xlsx)',accept='.xlsx',width='100%'),
                                        helpText("All data uploaded to the application must follow a specific template. The template is available for
                                                 download by ",
                                                 span(tagList(a("emailing Emma Jones.",
                                                                href="https://mail.google.com/mail/?view=cm&fs=1&to=emma.jones@deq.virginia.gov", target = 'blank'))))
                                        ),
                                      mainPanel(
                                        #verbatimTextOutput('test'),
                                        h3('User uploaded data'),
                                        helpText("If any StationID's are highlighted, then those stations did not pass validation rules and cannot be used for report
                                                 generation nor will they be saved on the R server. StationID's must have benthic data within the IR window to be 
                                                 used in this tool. Make sure the StationID is spelled correctly before ",
                                                 span(tagList(a("contacting Emma Jones",
                                                                href="https://mail.google.com/mail/?view=cm&fs=1&to=emma.jones@deq.virginia.gov", target = 'blank')), 
                                                      " with potential issues.")),
                                        dataTableOutput('inputTable')
                                        )
                                      ))))
