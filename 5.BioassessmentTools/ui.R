shinyUI(fluidPage(theme="yeti.css",
                  navbarPage("VDEQ Benthic Assessment Fact Sheet Tool",
                             
                             tabPanel('Report',
                                      sidebarPanel(
                                        fileInput('userData','Upload Site (.xlsx)',accept='.xlsx',width='100%'),
                                        helpText('By uploading data to this application, you are sending saving assessment decisions on the
                                                 R server. This dataset is shared with regional assessment staff for the assessment process.
                                                 If you upload a station that already exists in the shared dataset, the most recent version will
                                                 overwrite the previous version.'),
                                        uiOutput('userStations_'),
                                        uiOutput('downloadReport_')),
                                      mainPanel(
                                        h3('User uploaded data'),
                                        dataTableOutput('inputTable')
                                        #verbatimTextOutput('test'),
                                        )
                                      ),
                             tabPanel("About"))))