#source('global.R')

shinyUI(fluidPage(tags$head(
  tags$style(
    HTML(".shiny-notification {position:fixed; top: calc(60%); left: calc(10%); }"))),
  theme= "yeti.css",
  navbarPage("WQM Planning Tool", #id = 'someID',  # key for passing URL to specific Tab
             
             # tabPanel('How To',
             #          h2(strong('This project is still in beta testing phase.')),
             #          h3('Please report any data or application issues to Emma Jones emma.jones@deq.virginia.gov.'),
             #          
             #          # just using statewideResults$BRRO$pullDate as default for all regions bc the BRRO logic was built first. all pull dates identical
             #          
             #          
             #          h4('This tool allows users to quickly review region-specific water quality monitoring data. More frequent review of monitoring data ensures DEQ can improve
             #            QA/QC procedures, planning efforts, and watershed understanding for various projects, among other benefits.'),
             #          p('The data presented reflects monitoring data queried from the start of the current year to ',format(statewideResults$BRRO$pullDate, "%m-%d-%Y"), '. This dataset and analysis
             #            is refreshed on the 15th of every month. All results are run through automated analysis scripts to identify any parameter exceedances, where appropriate
             #            WQS metadata exist for a station. Please contact Emma Jones and your regional assessor if you find a station that does not have the appropriate WQS information
             #            attributed.'),
             #          p('The parameters analyzed include: Temperature, Dissolved Oxygen, pH, E.coli STV, E.coli Geomean, Enterococci STV, Enterococci Geomean, Ammonia,
             #            Total Phosphorus, and Chlorophyll a.'),
             #          p("The Regional Map Tab allows users to explore parameter exceedances spatially across a selected monitoring region."),
             #          p('The Exceedance Summary Tab provides preliminary understanding of sampled stations in the top table. The lower table reflects all stations that 
             #            have an exceedance in any parameter analyzed. '),
             #          p('The Monitoring Summary Tab allows users to explore regional monitoring efforts by collector ID with a heatmap of stations visited by collector ID.')),
             tabPanel("Regional Review",
                      h4(span('Preliminary analysis information presented in this tool reflects data available in ODS from two windows. The `Year to Date`
                              tab (below, left) overviews data collected by the selected region from ', 
                              strong(format(statewideResults$BRRO$`Date Range YTD`[1], "%m-%d-%Y")),' to ',
                              strong(format(statewideResults$BRRO$`Date Range YTD`[2], "%m-%d-%Y")), '. The `Two Year Summary to Date`
                              tab (below, right) overviews data collected by the selected region from ', 
                              strong(format(statewideResults$BRRO$`Date Range`[1], "%m-%d-%Y")),' to ',
                              strong(format(statewideResults$BRRO$`Date Range`[2], "%m-%d-%Y")), 'All information
                                                 is based on automated assessment scripts and do not indicate final assessment decisions made by regional assessment staff.')),
                      fluidRow(column(4, helpText("Select a region of interest from the drop down to the right and click the `Analyze Region` button to visualize monitoring 
                                                  results as of the query date noted above.")  ),
                               column(4, selectInput('regionChoice', 'Choose a region to review monitoring data collected from the start of the year',
                                                     choices = names(statewideResults))),
                               column(4, actionButton('runData', 'Analyze Region'))),
                      tabsetPanel(
                        tabPanel("Year to Date",
                                 br(),
                                 tabsetPanel(
                                   #regionalReviewUI('regionalReviewYTD')),
                                   tabPanel('Regional Map',
                                            # doing this outside the module bc issue with conditionalpanel and module logic
                                            fluidRow(column(4, helpText("This map allows users to quickly preview station exceedance results as of the last query date. By default, the map presents a station overview where 
                                                             the station is colored based on the most harmful preliminary results category (e.g. if a station has 8 parameters with no exceedances, 
                                                             2 parameters with one exceedance each, and 1 parameter with two or more exceedances, the station will be colored red to reflect the station contains at least one
                                                             parameter with two or more exceedances). 
                                                             The number of parameter exceedances in the most harmful category is reported for each station in the popup window (accessed by clicking
                                                             the station in the map). If a specific parameter is chosen, the map reflects the number of exceedances for that parameter for each station.")),
                                                     column(4, selectizeInput('parameterChoiceYTD', 'Choose a parameter to visualize regional exceedances.',
                                                                              choices = c('Overall Status',
                                                                                          parameterEXCcrosswalk$Parameter[!parameterEXCcrosswalk$Parameter %in% c('Water Column Metals', 'Water Column Toxics', 'Sediment Metals', 
                                                                                                                                                                  'Sediment Toxics', 'Fish Tissue Metals', 'Fish Tissue Toxics',
                                                                                                                                                                  'Benthics')]))),
                                                     column(4,
                                                            conditionalPanel(condition = "input.parameterChoiceYTD == 'Total Phosphorus'",
                                                                             helpText('For non-lake stations, a Total Phosphorus threshold of 0.2 mg/L was used as an exceedance flag to indicate the potential
                                                                    for nutrient problems.')),
                                                            conditionalPanel(condition = "input.parameterChoiceYTD == 'Chlorophyll a'",
                                                                             helpText('Chlorophyll a analyses are only performed on lake stations.')))),
                                            regionalReviewMapUI('regionalReviewMapYTD')),
                                   tabPanel('Station Status Summary',
                                            regionalReviewStationStatusUI('regionalReviewStationStatusYTD')),
                                   tabPanel('Monitoring Summary',
                                            regionalReviewMonitoringSummaryUI('regionalReviewMonitoringSummaryYTD')))),
                                 
                        tabPanel("Two Year Summary to Date",
                                 br(),
                                 tabsetPanel(
                                   tabPanel('Regional Map',
                                            # doing this outside the module bc issue with conditionalpanel and module logic
                                            fluidRow(column(4, helpText("This map allows users to quickly preview station exceedance results as of the last query date. By default, the map presents a station overview where 
                                                             the station is colored based on the most harmful preliminary results category (e.g. if a station has 8 parameters with no exceedances, 
                                                             2 parameters with one exceedance each, and 1 parameter with two or more exceedances, the station will be colored red to reflect the station contains at least one
                                                             parameter with two or more exceedances). 
                                                             The number of parameter exceedances in the most harmful category is reported for each station in the popup window (accessed by clicking
                                                             the station in the map). If a specific parameter is chosen, the map reflects the number of exceedances for that parameter for each station.")),
                                                     column(4, selectizeInput('parameterChoiceTwoYear', 'Choose a parameter to visualize regional exceedances.',
                                                                              choices = c('Overall Status',
                                                                                          parameterEXCcrosswalk$Parameter[!parameterEXCcrosswalk$Parameter %in% c('Water Column Metals', 'Water Column Toxics', 'Sediment Metals', 
                                                                                                                                                                  'Sediment Toxics', 'Fish Tissue Metals', 'Fish Tissue Toxics',
                                                                                                                                                                  'Benthics')]))),
                                                     column(4,
                                                            conditionalPanel(condition = "input.parameterChoiceTwoYear == 'Total Phosphorus'",
                                                                             helpText('For non-lake stations, a Total Phosphorus threshold of 0.2 mg/L was used as an exceedance flag to indicate the potential
                                                                    for nutrient problems.')),
                                                            conditionalPanel(condition = "input.parameterChoiceTwoYear == 'Chlorophyll a'",
                                                                             helpText('Chlorophyll a analyses are only performed on lake stations.')))),
                                            regionalReviewMapUI('regionalReviewMapTwoYear')),
                                   tabPanel('Station Status Summary',
                                            regionalReviewStationStatusUI('regionalReviewStationStatusTwoYear')),
                                   tabPanel('Monitoring Summary',
                                            regionalReviewMonitoringSummaryUI('regionalReviewMonitoringSummaryTwoYear'))))
                                 
                      )
                        
             )
  )))
                   