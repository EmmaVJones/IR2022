shinyUI(fluidPage(theme="yeti.css",
                  navbarPage("VDEQ Benthic Assessment Fact Sheet Tool",
                             tabPanel("About",
                                      h4('Background'),
                                      p(paste0('This application generates two types of standardized Benthic Fact Sheets for DEQ stations. The Assessment Fact Sheet tab
                                               generates fact sheets for data collected within the IR', assessmentCycle,
                                               ' data window. This tool was developed to be used after a regional biologist assesses stations using the '),
                                               span(tagList(a("Bioassessment Dashboard.",
                                                              href="http://deq-rstudio-prod.cov.virginia.gov/rsconnect/IR2022BioassessmentDashboard/",
                                                              target = 'blank'))), ' Assessment decisions saved on the server feed the Riverine Assessment
                                               application. The General Purpose Biological Fact Sheet tab generates a generic fact sheet with benthic macroinvertebrate
                                        and habitat information. The format of this report follows the same structure of the Assessment Fact Sheet report, but it allows
                                        users to select specific sample windows from all available benthic data and leaves most of the comment sections blank for the 
                                        user to fill out in the Word document. This version does not save any information to the R server.'),
                                      h4('Assesment Fact Sheet Usage'),
                                      p('Users may choose from stations that have assessment decisions saved on the R server or
                                        from data uploaded to the application. Once a station is chosen, the `Generate Report` button
                                        compiles benthic and habitat data within the IR window with biologist assessment decisions and writes a report,
                                        downloaded to the users Downloads folder.'),
                                      h4('General Purpose Biological Fact Sheet Usage'),
                                      p('Users may choose from any station that has benthic macroinvertebrate information. Once a station is chosen, the `Generate Report` button
                                        compiles benthic and habitat data within the collection window chosen by the user and writes a Microsoft Word report with some basic plots
                                        and tables, downloaded to the users Downloads folder. This version is sparse and is meant to be completed by the biologist for the intended 
                                        audience. Due to the nature of MS Word documents, the interactive color coded (and extremely wide) html tables do not translate well to that 
                                        medium. Users may choose from a long version of SCI and habitat metrics (included as a default for their chosen station and collection window)
                                        or save images of the html tables presented in the application for manual inclusion in their report.'),
                                      h4('Contact Information'),
                                      p('Please direct all questions about this application to Emma Jones (emma.jones@deq.virginia.gov) and Jason Hill
                                        (jason.hill@deq.virginia.gov).')),
                             tabPanel('Assessment Fact Sheet',
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
                                        )),
                             tabPanel('General Puropose Biological Fact Sheet',
                                      sidebarPanel(
                                        helpText("To generate a fact sheet for ", span(strong('PURPOSES OTHER THAN THE ASSESSMENT')),
                                                 " you must choose a station and collection window from the list of stations below."),
                                        selectInput('GPuserStation', "Choose a station to generate report",
                                                    choices = unique(benSamps$StationID)),
                                        selectInput('GPuserSCIMethod', "Choose a SCI method to generate report",
                                                    choices = c('VSCI', 'VCPMI + 63', 'VCPMI - 65')),
                                        uiOutput('GPuserWindow_'),
                                        h4('Generate Report for selected station'),
                                        downloadButton('GPdownloadReport', 'Generate Report')
                                      ),
                                      mainPanel(
                                        helpText('The way we visualize SCI and habitat metrics (aka super wide datasets with one row per sample event) does not lend
                                                 itself to conversion to Microsoft Word (the table is too wide and is cut off from the page). To overcome these
                                                 issues the metric information is presented in two ways. The report tacks a "long" version of the metric results to
                                                 the end of the report and this tab allows user to use the "snip" tool to save a .png of the marked up tables for manual
                                                 inclusion in the Microsoft Word document. It is up to the biologist to decide which version of the information best
                                                 suits their reporting needs.'),
                                        #verbatimTextOutput('testtest'),
                                        DT::dataTableOutput('GPSCImetrics'), br(),
                                        dataTableOutput('GPhabitatMetrics'))
                                      ))))
