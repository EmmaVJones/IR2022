shinyUI(fluidPage(tags$head(
  tags$style(
    HTML(".shiny-notification {position:fixed; top: calc(60%); left: calc(10%); }"))),
  theme= "yeti.css",
  navbarPage("WQM Planning Tool", #id = 'someID',  # key for passing URL to specific Tab
             
              # tabPanel('How To',
              #          h2(strong('This project is still in beta testing phase.')),
              #          h5('Please report any data or application issues to Emma Jones emma.jones@deq.virginia.gov.'),
              #          p('This tool allows users to quickly assess Water Quality Monitoring Station(s) against appropriate 
              #            standards and criteria for a given date range. The more frequent assessment of monitoring data ensures DEQ can improve
              #            QA/QC procedures, planning efforts, and watershed understanding for various projects, among other benefits.') ),
             tabPanel("Rapid Assessment",
                      tabsetPanel(
                        tabPanel("Station Data",
                                 sidebarPanel(
                                   helpText("Query pulls data directly from CEDS (data is refreshed nightly). All spatial data 
                                                           used to assist spatial querying methods are stored on R server (spatial data is refreshed weekly)."),
                                   radioButtons('queryType', "How would you like to query stations?",
                                                choices = c('Spatial Filters', 'Wildcard Selection', 
                                                            'Manually Specify Stations (requires a few seconds for the station text box to appear)')),
                                   # Spatial filters
                                   conditionalPanel(condition = "input.queryType == 'Spatial Filters'",
                                                    uiOutput('spatialFilters_assessmentRegion'),
                                                    uiOutput('spatialFilters_subbasin'),
                                                    uiOutput('spatialFilters_VAHU6')),
                                   # Wildcard Selection
                                   conditionalPanel(condition = "input.queryType == 'Wildcard Selection'",
                                                    uiOutput('wildcardSelection')),
                                   # Manually Specify Stations Selection
                                   conditionalPanel(condition = "input.queryType == 'Manually Specify Stations (requires a few seconds for the station text box to appear)'",
                                                    uiOutput('manualSelectionUI')),
                                   hr(), # keep these at the top level to allow reuse of same filter parameters
                                   helpText("Additional filter(s) applied on 'Pull Stations' request. These filters are not interactively cross validated."),
                                   uiOutput('spatialFilters_Ecoregion'),
                                   uiOutput('spatialFilters_EcoregionLevel4'),
                                   uiOutput('spatialFilters_County'),
                                   uiOutput('dateRange_multistationUI'),
                                   fluidRow(column(6, uiOutput('programCode_FilterUI')),
                                            column(6,actionButton('showProgramCodeTable', 'Sample Program Codes'))),
                                   wellPanel(
                                     helpText('Interactive Cross Validation Active In this Box'),
                                     uiOutput('labMediaCodeUI'),
                                     fluidRow(column(6, uiOutput('sampleGroupCode_FilterUI')),
                                              column(6,actionButton('showSampleGroupCodeTable', 'Sample Group Codes')))),
                                   helpText('Remember, use % as your wildcard, not *'),
                                   textInput('wildcardRunIDText', 'Filter Stations by Run ID Wildcard Selection', value = NULL, placeholder = 'HF%'), 
                                   uiOutput('analyte_FilterUI'),
                                   
                                   # add in appropriate pull data button based on query type
                                   conditionalPanel(condition = "input.queryType == 'Spatial Filters'",
                                                    actionButton('begin_multistation_spatial', 'Pull Stations',class='btn-block')),
                                   conditionalPanel(condition = "input.queryType == 'Wildcard Selection'",
                                                    actionButton('begin_multistation_wildcard', 'Pull Stations',class='btn-block')),
                                   conditionalPanel(condition = "input.queryType == 'Manually Specify Stations (requires a few seconds for the station text box to appear)'",
                                                    actionButton('begin_multistation_manual', 'Pull Stations',class='btn-block')) ),
                                 mainPanel(
                                   leafletOutput('multistationMap'),
                                   helpText('Stations identified in the spatial filter are displayed below unless user further refines
                                                          selected stations with polygon and rectangle drawing tools in map.'),
                                   br(),
                                   h4('Station Information'),
                                   DT::dataTableOutput('multistationInfoTable')) ),
                        tabPanel("Water Quality Data",
                                 sidebarPanel(
                                   uiOutput('multistationDateRangeFilter_'),
                                   uiOutput('multistationLabCodesDropped_'),
                                   uiOutput('multistationDepthFilter_'),
                                   checkboxGroupInput('multistationRepFilter', "Filter Reps",
                                                      choices = c('R','S1', 'S2'), selected = 'R'), width = 3),
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel('Station Data',
                                              h4('Same same as WQM Data query tool') )))),
                      tabPanel("Rapid Evaluation",
                               assessmentUI('assessmentModule') ),
                               
                               # sidebarPanel(
                               #   p(span('This area of the application allows users to assess water quality monitoring data on the fly based on
                               #            previous station and date window selections. CEDS does not contain all data necessary to fully assess
                               #            a station and potential date ranges and stations selected may not meet necessary assessment assumptions, so this tool
                               #            is considered a ', strong('PRELIMINARY ASSESSMENT'), 'and is ', strong('NOT VALID AS AN OFFICIAL ASSESSMENT DECISION'),
                               #            '; however, understanding how station(s) compare to relevant standards/criteria is useful for monitoring planning
                               #            and QA purposes.')),
                               #     helpText('To perform an assessment on the selected stations and date range, first verify the station information on the 
                               #              `Assessment Station Metadata` tab. Please complete the necessary WQS and lake designations (if applicable) information
                               #              to appropriately assess the chosen data. When the metadata is complete, press the `Assess` button below and navigate
                               #              to the `Results` tab to view station summary.'),
                               #     actionButton('rapidAssessmentRun', 'Assess', class='btn-block')),
                               # mainPanel(
                               #   tabsetPanel(
                               #     tabPanel('Assessment Station Metadata',
                               #              verbatimTextOutput('test')),
                               #     tabPanel('Results')
                               #   )
                               # )
                               # )
                               # )
                      #),
             tabPanel("Monitoring Report") 
  )))
))
