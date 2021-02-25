shinyUI(fluidPage(theme="yeti.css",
                  navbarPage("VDEQ Benthic Assessment Fact Sheet Tool",
                             
                             tabPanel('Report',
                                      sidebarPanel(
                                        fileInput('userData','Upload Site (.xlsx)',accept='.xlsx',width='100%'),
                                        uiOutput('userStations_'),
                                        uiOutput('downloadReport_')),
                                      mainPanel(
                                        h5('User uploaded data'),
                                        dataTableOutput('inputTable')
                                        #verbatimTextOutput('test'),
                                        )
                                      ),
                             tabPanel("About"))))