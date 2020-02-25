library(shiny)
library(DT)


sites_Accepted <- filter(sitesUnique, originalStationID %in% '1.1') 

shinyApp(
  ui = fluidPage(
    DTOutput('x1'),
    DTOutput('y1')
  ),
  server = function(input, output, session) {
    x = reactiveValues(sites_Accepted = sites_Accepted)
    
    #x = reactive({sites_Accepted})
    #y <- reactive({
    #  input$x1_cell_edit
    #  x
    #})
    
    output$x1 = renderDT(x[['sites_Accepted']], selection = 'none', rownames = F, editable = T)
      
    proxy = dataTableProxy('x1')
    
    print(str(x))
    
    observeEvent(input$x1_cell_edit, {
      info = input$x1_cell_edit
      str(info)
      i = info$row
      j = info$col + 1  # column index offset by 1
      v = info$value
      x[['sites_Accepted']][i, j] <<- DT::coerceValue(v, x[['sites_Accepted']][i, j])
      replaceData(proxy, x[['sites_Accepted']], resetPaging = FALSE, rownames = FALSE)
    })
    
    #output$y1 = renderDT(y(), selection = 'none', rownames = F, editable = T)
  }
)

