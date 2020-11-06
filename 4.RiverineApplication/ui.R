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
                                            dynamicSelectInput("basinSelection", "Select Basin/Subbasin", multiple = FALSE),
                                            dynamicSelectInput("HUC6Selection", "Select VAHU6", multiple = FALSE),
                                            helpText('To begin assessing the selected VAHU6, click the ',
                                                     span(strong('Assessment Unit Review')), ' tab at the top of the navigation bar.'),
                                            hr(),
                                            p('Optional.'),
                                            helpText('After you use the VAHU6 drop down to select your desired watershed, you may
                                                     preview the AUs contained within the selected watershed by clicking the ',
                                                     span(strong('Spatially Preview Assessment Units')),' below.'),
                                            actionButton('reviewAUs',"Spatially Preview Assessment Units",class='btn-block')
                                            ),
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
                                            
                                            br(), br(), br() # a bit of breathing room
                                          )
                                 ),
                                 tabPanel('Assessment Unit Review',
                                          fluidRow(column(9, DT::dataTableOutput('selectedVAHU6'))),#,
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
                                                   column(4, DT::dataTableOutput('stationHistoricalInfo'))),
                                          hr(),
                                          h3('Station Results for Review'),
                                          helpText('This table outputs the site specific results for direct export to the Station Table. It also serves to highlight
                                                  where exceedances are present and should be reviewed in the individual parameter visualization tabs below.'),
                                          h4('Stations Table Results'),
                                          helpText('Parameters are highlighted
                                                  in different colors to indicate further review may be necessary. Parameters highlighted in yellow have at least one 
                                                  violation of a standard. When BENTHIC_STAT is highlighed, it indicates there is benthic data present for that site
                                                  and the assessor should review that information with the Regional Biologist. Parameters highlighted in red exceed the 10.5% exceedance rate. Both scenarios warrant further
                                                  investigation and may requre comments in the Station Table and ADB.'),
                                          DT::dataTableOutput('stationTableDataSummary'), br(), 
                                          h4('PWS violations'),
                                          helpText("Any PWS violations should noted in a station's COMMENT field of the Stations Table. The table below organizes 
                                                  PWS information to expedite the comment process."),
                                          DT::dataTableOutput('PWStable'),
                                          br(),hr(),br(),
                                          h3('Assessment Unit Raw Data Review and Visualization'),
                                          tabsetPanel(
                                            tabPanel('Conventionals Data',
                                                     tabsetPanel(
                                                       tabPanel('Raw Data',br(),
                                                                DT::dataTableOutput('AURawData'),
                                                                h4('Data Summary'),
                                                                h5('Records Retrieved in Assessment Unit:'),
                                                                fluidRow(column(1),column(10,textOutput('stationDataTableRecords'))),
                                                                h5('Field and Lab Data in Assessment Window:'),
                                                                fluidRow(column(1),column(10,tableOutput('uniqueStationDataTableRecords'))),
                                                                h5('Assessment Window:'),
                                                                fluidRow(column(1),column(10,textOutput('stationDataTableAssessmentWindow'))), br(),br()),
                                                       tabPanel('Temperature',
                                                                helpText('Review each site using the single site visualization section. The results from this analysis are reflected
                                                                        in the TEMP_VIO, TEMP_SAMP, and TEMP_STAT columns in the station table.'),
                                                                temperaturePlotlySingleStationUI('temperature')),
                                                       tabPanel('pH'),
                                                       tabPanel("DO"),
                                                       tabPanel("Specific Conductance"),
                                                       tabPanel("Salinity"),
                                                       tabPanel("Total Nitrogen"),
                                                       tabPanel("Ammonia"),
                                                       tabPanel("Total Phosphorus"),
                                                       tabPanel("Fecal Coliform"),
                                                       tabPanel("E. Coli"),
                                                       tabPanel("Enterococci"),
                                                       tabPanel("Chlorophyll a"),
                                                       tabPanel("Suspended Sediments"),
                                                       tabPanel("Nitrate"),
                                                       tabPanel("Chloride"),
                                                       tabPanel("Sulfate")
                                                     )),
                                            tabPanel('EDAS Data'),
                                            tabPanel('Metals Data')#,
                                            #tabPanel('Toxics Data', 
                                            #          tabsetPanel(
                                            #            tabPanel('Fish Tissue Data'),
                                            #            tabPanel('Water Column Toxics'),
                                            #            tabPanel('Sediment Toxics')))
                                            
                                          ))
                      )))))
                                            