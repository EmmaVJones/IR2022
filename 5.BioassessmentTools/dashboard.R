# source('global.R')
# 
# assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
# ecoregion <- st_read('data/GIS/vaECOREGIONlevel3__proj84.shp') 

ui <- dashboardPage(
  
  dashboardHeader(title = "IR 2022 Bioassessment Dashboard", titleWidth = '400px'),
  dashboardSidebar(
    sidebarMenu(
      helpText('Pro Tip: You can use each of the drop', br(),
               ' down boxes independently; however, ', br(),
               'if you start with the Collector Filter,', br(),
               ' the cross validation features narrow',br(), 
               ' down options in subsequent selection ', br(),
               ' fields.'),
      uiOutput('filters'))),
  dashboardBody(verbatimTextOutput('table1'),
                verbatimTextOutput('table2'),
                verbatimTextOutput('table3'),
                verbatimTextOutput('table4'))
)

server <- function(input, output, session) {
  
  output$filters <- renderUI({
    list(
      selectInput("collectorFilter", "Collector Filter", choices = sort(unique(benSamps$`Collected By`)), multiple = TRUE),
      selectInput("basinFilter", "Basin Filter", choices = sort(unique(benSamps$Basin_Code)), selected = NULL, multiple = TRUE),
      selectInput("stationFilter", "StationID Filter", choices = sort(unique(benSamps$StationID)), selected = NULL, multiple = TRUE),
      selectInput("repFilter", "Rep Filter", choices = sort(unique(benSamps$RepNum)), selected = NULL, multiple = TRUE)    )})
  
  
  observe({ updateSelectInput(session, "basinFilter", "Basin Filter",
                              #choices = sort(unique(benSampsFilter()$Basin_Code))) })
                              choices = if(!is.null(input$collectorFilter)){
                                filter(benSamps, `Collected By` %in% input$collectorFilter) %>%
                                  distinct(Basin_Code) %>% arrange(Basin_Code) %>% pull()
                              } else {distinct(benSamps, Basin_Code) %>% arrange(Basin_Code) %>% pull()} ) })
  
  observe({ updateSelectInput(session, "stationFilter", "StationID Filter",
                              choices = if(!is.null(input$collectorFilter)){
                                filter(benSamps, `Collected By` %in% input$collectorFilter) %>%
                                  filter( Basin_Code %in% input$basinFilter) %>%
                                  distinct(StationID) %>% arrange(StationID) %>% pull()
                              } else {distinct(benSamps, StationID) %>% arrange(StationID) %>% pull()} ) })
  
  observe({ updateSelectInput(session, "repFilter", "Rep Filter",
                              choices = if(!is.null(input$collectorFilter)){
                                filter(benSamps, `Collected By` %in% input$collectorFilter) %>%
                                  filter( Basin_Code %in% input$basinFilter) %>%
                                  filter(RepNum %in% input$repFilter) %>%
                                  distinct(RepNum) %>% arrange(RepNum) %>% pull()
                              } else {distinct(benSamps, RepNum) %>% arrange(RepNum) %>% pull()} ) })
  
  benSampsFilter <- reactive({
    benSamps %>%
      {if(!is.null(input$collectorFilter))
        filter(., `Collected By` %in% input$collectorFilter)
        else . } %>%
      {if(!is.null(input$basinFilter))
        filter(., Basin_Code %in% input$basinFilter)
        else . } %>%
      {if(!is.null(input$stationFilter))
        filter(., StationID %in% input$stationFilter)
        else . } %>%
      {if(!is.null(input$repFilter))
        filter(., RepNum %in% input$repFilter)
        else .} })
  
  output$table2 <- renderPrint({ #input$collectorFilter })
    benSampsFilter() })
}

shinyApp(ui, server)

