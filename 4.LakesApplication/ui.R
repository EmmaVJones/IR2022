shinyUI(fluidPage(theme="yeti.css",
                  shinyjs::useShinyjs(),
                  div(
                    id = "loading_page",
                    h1("Loading...")
                  ),
                  hidden(
                    div(
                      id = "main_content",
                      # suppress error messages as data loads, hacky
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                      ),
                      navbarPage(paste("VDEQ ",assessmentCycle," IR Riverine Assessment Tool", sep=''),
                                 # add back in
                                 
                                 
                                 
                                 
                                 
                                 
                                 
                                 tabPanel('Watershed Selection',
                                          sidebarPanel(
                                            p('Run once per user session.'),
                                            helpText("Select Region to Assess and click ",span(strong("Retrieve Assessment Units From Server")), "button
                                                     to bring in spatial data for further analyses."),
                                            uiOutput('DEQregionSelectionUI'),
                                            br(),
                                            actionButton('pullAUs',"Retrieve Assessment Units From Server",class='btn-block'),
                                            hr(),
                                            p('Run as often as necessary per user session.'),
                                            uiOutput('lakeSelection_'),
                                            #dynamicSelectInput("lakeSelection", "Select Lake", multiple = FALSE),
                                            helpText('To begin assessing the selected lake, click the ',
                                                     span(strong('Assessment Unit Review')), ' tab at the top of the navigation bar.'),
                                            hr(),
                                            p('Optional.'),
                                            helpText('After you use the VAHU6 drop down to select your desired watershed, you may
                                                     preview the stations and AUs contained within the selected watershed by clicking the ',
                                                     span(strong('Spatially Preview Stations and Assessment Units')),' below.'),
                                            actionButton('reviewAUs',"Spatially Preview Stations and Assessment Units",class='btn-block'),
                                            br(),
                                            helpText('For a quick `30,000 foot view of the watershed`, click the button below to open a map
                                                     overviewing station statuses.'),
                                            actionButton('statusOverview','Watershed Status Overview', class='btn-block')  ),
                                          mainPanel(
                                            verbatimTextOutput('test'),
                                            
                                            leafletOutput('VAmap'),
                                            br(),
                                            h5(strong('Assessment Units in Selected VAHU6')),
                                            DT::dataTableOutput('AUSummary'),
                                            br(),
                                            h5(strong('Stations in Selected VAHU6 that were sampled in current window')),
                                            helpText("The stations highlighted in gray can be analyzed by the application. The stations
                                                     highlighted in yellow were sampled in the current window, but cannot be analyzed by the
                                                     application because they are not in the input stations table."),
                                            DT::dataTableOutput('stationSummary'),
                                            br(),
                                            h5(strong("Stations in Selected VAHU6 that have no data in the current window but were carried 
                                                      over from last cycle due to an IM designation in one of the 2020IR status fields or
                                                      the 2020 stations table reports the station was carried over from a previous cycle.")),
                                            helpText('These stations can be viewed in the application and stations table, but none of the 
                                                     parameter modules will display data as no data is available in the current window.'),
                                            DT::dataTableOutput('carryoverStationSummary'),
                                            br(), br(), br() # a bit of breathing room
                                          )
                                 )#,
                      )))))