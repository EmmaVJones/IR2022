

# source('global.R')
# 
# assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
# ecoregion <- st_read('data/GIS/vaECOREGIONlevel3__proj84.shp') 

ui <- dashboardPage(
  
  dashboardHeader(title = "IR 2022 Bioassessment Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      selectInput("collectorFilter", "Collector Filter", choices = sort(unique(benSamps$`Collected By`)), multiple = TRUE),
      selectInput("basinFilter", "Basin Filter", choices = sort(unique(benSamps$Basin_Code)), selected = NULL, multiple = TRUE),
      selectInput("stationFilter", "StationID Filter", choices = sort(unique(benSamps$StationID)), selected = NULL, multiple = TRUE),
      selectInput("repFilter", "Rep Filter", choices = sort(unique(benSamps$RepNum)), selected = NULL, multiple = TRUE))  ),
  dashboardBody(verbatimTextOutput('table1'),
                verbatimTextOutput('table2'),
                verbatimTextOutput('table3'),
                verbatimTextOutput('table4'))
)

server <- function(input, output, session) {
  
  observe({ updateSelectInput(session, "basinFilter", "Basin Filter",
                              #choices = sort(unique(benSampsFilter()$Basin_Code))) })
                              choices = if(!is.null(input$collectorFilter)){
                                filter(benSamps, `Collected By` %in% input$collectorFilter) %>%
                                  distinct(Basin_Code) %>% arrange(Basin_Code) %>% pull()
                              } else {distinct(benSamps, Basin_Code) %>% arrange(Basin_Code) %>% pull()} ) })
  
  observe({ updateSelectInput(session, "stationFilter", "StationID Filter",
                              choices = if(!is.null(input$collectorFilter)){
                                filter(benSamps, `Collected By` %in% input$collectorFilter) %>%
                                  distinct(Basin_Code) %>% arrange(Basin_Code) %>% pull()
                              } else {distinct(benSamps, Basin_Code) %>% arrange(Basin_Code) %>% pull()} ) })
  
  benSampsFilter <- reactive({
    benSamps %>%
      {if(!is.null(input$collectorFilter))
        filter(., `Collected By` %in% input$collectorFilter)
        else . } %>%
      {if(!is.null(input$basinFilter))
        filter(., Basin_Code %in% basinFilter)
        else . } %>%
      {if(!is.null(input$stationFilter))
        filter(., StationID %in% stationFilter)
        else . } %>%
      {if(!is.null(input$repFilter))
        filter(., RepNum %in% repFilter)
        else .} })
  
  
  # output$collector <- renderUI({req(benSampsFilter())
  #   z <- if(nrow(benSampsFilter())>0)
  #   selectInput("dynamicCollectorFilter", "Collector Filter", 
  #               choices = sort(unique(benSampsFilter()$`Collected By`)), multiple = TRUE)  })
  # 
  
  output$table1 <- renderPrint({ #input$collectorFilter })
    benSamps %>%
      {if(!is.null(input$collectorFilter))
        filter(., `Collected By` %in% input$collectorFilter)
        else . }  })
  output$table2 <- renderPrint({ #input$collectorFilter })
    benSamps %>%
      {if(!is.null(input$collectorFilter))
        filter(., `Collected By` %in% input$collectorFilter)
        else . } %>%
      {if(!is.null(input$basinFilter))
        filter(., Basin_Code %in% basinFilter)
        else . }  })
  output$table3 <- renderPrint({ #input$collectorFilter })
    benSamps %>%
      {if(!is.null(input$collectorFilter))
        filter(., `Collected By` %in% input$collectorFilter)
        else . } %>%
      {if(!is.null(input$basinFilter))
        filter(., Basin_Code %in% basinFilter)
        else . } %>%
      {if(!is.null(input$stationFilter))
        filter(., StationID %in% stationFilter)
        else . }  })
  output$table4 <- renderPrint({ #input$collectorFilter })
    benSamps %>%
      {if(!is.null(input$collectorFilter))
        filter(., `Collected By` %in% input$collectorFilter)
        else . } %>%
      {if(!is.null(input$basinFilter))
        filter(., Basin_Code %in% basinFilter)
        else . } %>%
      {if(!is.null(input$stationFilter))
        filter(., StationID %in% stationFilter)
        else . } %>%
      {if(!is.null(input$repFilter))
        filter(., RepNum %in% repFilter)
        else .}  })
}

shinyApp(ui, server)
