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
                                 tabPanel('Watershed Selection',
                                          sidebarPanel(
                                            dynamicSelectInput("DEQregionSelection", "Select DEQ Assessment Region", multiple = FALSE),
                                            dynamicSelectInput("basinSelection", "Select Basin/Subbasin", multiple = FALSE),
                                            dynamicSelectInput("HUC6Selection", "Select VAHU6", multiple = FALSE),
                                            br()#,
                                            #actionButton('reviewAUs',"Preview Assesment Units",class='btn-block')
                                            ),
                                          mainPanel(
                                            #leafletOutput('VAmap'),
                                            br(),
                                            h5(strong('Assessment Units in Selected VAHU6')),
                                            DT::dataTableOutput('AUSummary')#,
                                          )
                                 )
                      )))))
                                            