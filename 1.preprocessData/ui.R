#source('global.R')

shinyUI(fluidPage(theme= "yeti.css",
                  navbarPage("Regional Assessment Metadata Validation",
                             tabPanel("Watershed Selection",
                                      sidebarPanel(
                                        dynamicSelectInput("DEQregionSelection", "Select DEQ Assessment Region", multiple = FALSE),
                                        dynamicSelectInput("basinSelection", "Select Major Basin", multiple = FALSE),
                                        selectInput('assessmentType','Assessment Type', choices = c('Riverine','Lacustrine','Estuarine')),
                                        #dynamicSelectInput("HUC6Selection", "Select VAHU6", multiple = FALSE),
                                        br(),
                                        actionButton('begin',"Begin",class='btn-block')),
                                      mainPanel(
                                        leafletOutput('VAmap'),
                                        br(),
                                        h5(strong('Assessment Units in Selected Major Basin')),
                                        #                                              ,
                                        DT::dataTableOutput('AUSummary')
                                      )),
                             tabPanel("Assessment Unit Review",
                                      wellPanel(
                                        fluidRow(column(4, textOutput('regionalSitesSummary'),
                                                        actionButton('plotRegionalSitesSummary', 'Plot stations in the selected Region/Basin')),
                                                 column(4, textOutput('snapTooManySummary'),
                                                        actionButton('plotSnapTooManySummary', 'Plot stations that snapped to > 1 AU Segment')),
                                                 column(4, textOutput('noSnapSummary'),
                                                        actionButton('plotNoSnapSummary', 'Plot stations that snapped to 0 AU segments in preprocessing')))),
                                      leafletOutput('AUmap'),
                                      fluidRow(
                                        actionButton('clear_all', 'Clear all', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('backspace')),
                                        actionButton('accept', 'Accept', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('check-circle')),
                                        actionButton('changeAU', 'Manual AU Adjustment', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('exchange')) ),
                                      br(),
                                      tabsetPanel(
                                        tabPanel(strong('Original User Uploaded and Existing Stations Data'),
                                                 br(),
                                                 h5(strong('Selected Station Information')),
                                                 DT::dataTableOutput('selectedSiteTable'),
                                                 h5(strong('Associated AU Information')),
                                                 DT::dataTableOutput('associatedAUTable'),
                                                 verbatimTextOutput('test')),
                                        tabPanel(strong('Updated Stations Data'),
                                                 br(),
                                                 fluidRow(
                                                   h5(strong('Adjusted Station Data')),
                                                   div(DT::dataTableOutput("adjustedStationsTable"), style = "font-size:80%"))
                                        ))),
                             tabPanel("WQS Review"),
                             tabPanel("About",
                                      p('This app was created to help assessors attach correct AU and WQS information to 
                                              stations for each assessment cycle.'),
                                      p('Emma has already run the joining and snapping scripts such that the assessor
                                              is just identifying the correct AU/WQS where more than one segment snapped to a site 
                                              or no segments snapped to a site in addition to reviewing all metadata prior
                                              to assessing stations.'))
                  )))