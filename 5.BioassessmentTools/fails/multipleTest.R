
ui <- dashboardPage(
  
  dashboardHeader(title = "IR 2022 Bioassessment Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      dynamicSelectInput("collector", "Select collector", multiple = TRUE),
      dynamicSelectInput("basin", "Select basin", multiple = TRUE))),
      #selectInput("collectorFilter", "Collector Filter", choices = sort(unique(benSamps$`Collected By`)), multiple = TRUE),
      #selectInput("basinFilter", "Basin Filter", choices = sort(unique(benSamps$Basin_Code)), selected = NULL))),
  
  dashboardBody(tableOutput('table1'))
    
  )
    
server <- function(input, output, session) {
  # Query AU's By Selectize arguments
  #the_data <- reactive({lakeStations})
  the_data <- reactive({benSamps})
  #region_filter <- shiny::callModule(dynamicSelect, 'regionSelection', the_data, "OFFICE_NM")
  collector_filter <- shiny::callModule(dynamicSelect, "collector", the_data, "Collected By" )
  basin_filter <- shiny::callModule(dynamicSelect, "basin", collector_filter, "Basin_Code" )
  
  
  output$table1 <- renderTable({basin_filter()})
  
}

shinyApp(ui, server)