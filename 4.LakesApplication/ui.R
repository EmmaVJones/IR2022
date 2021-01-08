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
                                            hr()),
                                          mainPanel(
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
                                            #br(),
                                            #h5(strong("Stations in Selected VAHU6 that have no data in the current window but were carried 
                                            #          over from last cycle due to an IM designation in one of the 2020IR status fields or
                                            #          the 2020 stations table reports the station was carried over from a previous cycle.")),
                                            #helpText('These stations can be viewed in the application and stations table, but none of the 
                                            #         parameter modules will display data as no data is available in the current window.'),
                                            #DT::dataTableOutput('carryoverStationSummary'),
                                            #verbatimTextOutput('test'),
                                            br(), br(), br() # a bit of breathing room
                                          )),
                                 tabPanel('Assessment Unit Review',
                                          #fluidRow(column(9, 
                                          DT::dataTableOutput('selectedLake'),#)),#,
                                          #column(3,br(),actionButton('pullVAHU6data','Select Watershed for analysis'),
                                          #      helpText('If the button above is disabled, there are no AUs in the selected VAHU6 watershed.'))),
                                          hr(),
                                          uiOutput('AUselection_'),
                                          h5(strong('AU information from last cycle')),
                                          DT::dataTableOutput('selectedAU'),br(),
                                          uiOutput('stationSelection_'),
                                          fluidRow(column(4, DT::dataTableOutput('stationInfo')),
                                                   column(4, leafletOutput('stationMap', height = 300, width = 300),
                                                          helpText("The AUs displayed on the map above represent all AUs associated with the selected
                                                                  station (listed in a station's ID305B_1:ID305B_10 fields) for context. ")),
                                                   column(4, 
                                                          tabsetPanel(
                                                            tabPanel('2020 Station Table',
                                                                     DT::dataTableOutput('stationHistoricalInfo1')),
                                                            tabPanel('2018 Station Table',
                                                                     DT::dataTableOutput('stationHistoricalInfo2'))))),
                                          hr()#,
                                 )
                      )))))