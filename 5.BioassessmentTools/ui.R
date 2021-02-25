shinyUI(fluidPage(theme="yeti.css",
                  navbarPage("VDEQ Benthic Assessment Fact Sheet Tool",
                             
                             tabPanel('Report',
                                      sidebarPanel(
                                        fileInput('userData','Upload Site (.xlsx)',accept='.xlsx',width='100%'),
                                        uiOutput('userStations_')),
                                      mainPanel(
                                        dataTableOutput('inputTable'),
                                        verbatimTextOutput('test'),
                                        downloadButton('downloadReport', 'Generate Report'))
                                      ),
                             tabPanel("About"))))