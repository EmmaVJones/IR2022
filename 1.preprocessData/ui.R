
shinyUI(fluidPage(theme= "yeti.css",
                  navbarPage("Regional Assessment Metadata Validation",
                             #navbarMenu("Assessment Unit QA",
                                        #tabPanel("Watershed Selection",
                                        #         sidebarPanel(
                                        #           dynamicSelectInput("DEQregionSelection", "Select DEQ Assessment Region", multiple = FALSE),
                                        #           dynamicSelectInput("basinSelection", "Select Major Basin", multiple = FALSE),
                                        #           selectInput('assessmentType','Assessment Type', choices = c('Riverine','Lacustrine','Estuarine')),
                                        #           #dynamicSelectInput("HUC6Selection", "Select VAHU6", multiple = FALSE),
                                        #           br(),
                                        #           actionButton('begin', HTML("Begin Review With Selection <br/>(Clears Cached Results)"),class='btn-block')),
                                        #         mainPanel(
                                        #           leafletOutput('VAmap'),
                                        #           br(),
                                        #           h5(strong('Preprocessing Data Recap for Selected Region/Basin/Type Combination')),
                                        #           fluidRow(column(3, textOutput('singleSnapSummary1')),
                                        #                    column(3, textOutput('snapTooManySummary1')),
                                        #                    column(3, textOutput('noSnapSummary1')),
                                        #                    column(3, textOutput('regionalSitesSummary1'))),
                                        #           br(),
                                        #           h5(strong('Assessment Units in Selected Major Basin')),
                                        #           DT::dataTableOutput('AUSummary'), br(), br(), br() # a little breathing room
                                        #         )),
                                        #tabPanel("Manual Review",
                                        #         wellPanel(
                                        #           fluidRow(column(3, textOutput('singleSnapSummary2'),
                                        #                           actionButton('plotSingleSnapSummary', HTML('Plot stations that snapped <br/>to 1 AU Segment'))),
                                        #                    column(3, textOutput('snapTooManySummary2'),
                                        #                           actionButton('plotSnapTooManySummary', HTML('Plot stations that snapped <br/>to > 1 AU Segment'))),
                                        #                    column(3, textOutput('noSnapSummary2'),
                                        #                           actionButton('plotNoSnapSummary', HTML('Plot stations that snapped <br/>to 0 AU segments'))),
                                        #                    column(3, textOutput('regionalSitesSummary2'),
                                        #                           actionButton('plotRegionalSitesSummary', HTML('Plot all stations in <br/>the selected Region/Basin'))))),
                                        #         leafletOutput('AUmap'),
                                        #         fluidRow(
                                        #           actionButton('clear_all', 'Clear Selection', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('backspace')),
                                        #           actionButton('accept', 'Accept', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('check-circle')),
                                        #           actionButton('changeAU', 'Manual AU Adjustment', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('exchange')),
                                        #           actionButton('checkMeOut', 'Check Me Out', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('exchange')),
                                        #           downloadButton('downloadAU', label = "Export reviews",style='color: #fff; background-color: #228b22; border-color: #134e13')        ),
                                        #         br(),
                                        #         tabsetPanel(
                                        #           tabPanel(strong('Original User Uploaded and Existing Stations Data'),
                                        #                    br(),
                                        #                    h5(strong('Selected Station Information')),
                                        #                    DT::dataTableOutput('selectedSiteTable'),
                                        #                    h5(strong('Associated AU Information')),
                                        #                    DT::dataTableOutput('associatedAUTable'),
                                        #                    br(), br(), br(),
                                        #                    verbatimTextOutput('test')),
                                        #           tabPanel(strong('Updated Stations Data'),
                                        #                    br(),
                                        #                    fluidRow(
                                        #                      h5(strong('Adjusted Station Data')),
                                        #                      div(DT::dataTableOutput("adjustedStationsTable"), style = "font-size:80%")),
                                        #                    br(), br(), br()
                                        #           )))),
                             navbarMenu("Water Quality Standards QA",
                                        tabPanel("Watershed Selection",
                                                 sidebarPanel(
                                                   fluidRow(column(5, selectInput('WQSwaterbodyType','Waterbody Type', choices = unique(WQSlayerConversion$waterbodyType))),
                                                            column(7, actionButton('WQSstart',HTML("Begin Review With <br/>Waterbody Selection <br/>(Clears Cached Results)"),
                                                                                   class='btn-block'))),
                                                   hr(),
                                                   conditionalPanel(condition = ("input.WQSstart"), # dynamicSelectInput did not work with progress bar for spatial file
                                                                    uiOutput('WQSDEQregionSelection_'),
                                                                    uiOutput('WQSsubbasinSelection_'),
                                                                    br(),
                                                                    uiOutput('WQSbegin_'))),
                                                 
                                                 mainPanel(
                                                   #verbatimTextOutput('test'),
                                                   leafletOutput('WQSVAmap'),
                                                   h5(strong('Preprocessing Data Recap for Selected Region/Subbasin/Type Combination')),
                                                   fluidRow(column(3, textOutput('singleSnapSummary1WQS')),
                                                            column(3, textOutput('snapTooManySummary1WQS')),
                                                            column(3, textOutput('noSnapSummary1WQS')),
                                                            column(3, textOutput('regionalSitesSummary1WQS'))),
                                                   br())),
                                        tabPanel("Manual Review")
                                                 
                                        
                                        ),
                             tabPanel("About",
                                      p('This app was created to help assessors attach correct AU and WQS information to 
                                              stations for each assessment cycle.'),
                                      p('Emma has already run the joining and snapping scripts such that the assessor
                                              is just identifying the correct AU/WQS where more than one segment snapped to a site 
                                              or no segments snapped to a site in addition to reviewing all metadata prior
                                              to assessing stations.'))
                  )))
