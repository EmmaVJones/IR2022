# https://stackoverflow.com/questions/52593539/edit-datatable-in-shiny-with-dropdown-selection-for-factor-variables/62811612#62811612
#If you want to use the unique values of the column, set the option createdCell to JS(createdCell()), or simply don't set this option.

#https://github.com/rstudio/DT/pull/480
# 
# library(shiny)
# library(DT)
# 
# callback <- c(
#   "var id = $(table.table().node()).closest('.datatables').attr('id');",
#   "$.contextMenu({",
#   "  selector: '#' + id + ' td.factor input[type=text]',",
#   "  trigger: 'hover',",
#   "  build: function($trigger, e){",
#   "    var levels = $trigger.parent().data('levels');",
#   "    if(levels === undefined){",
#   "      var colindex = table.cell($trigger.parent()[0]).index().column;",
#   "      levels = table.column(colindex).data().unique();",
#   "    }",
#   "    var options = levels.reduce(function(result, item, index, array){",
#   "      result[index] = item;",
#   "      return result;",
#   "    }, {});",
#   "    return {",
#   "      autoHide: true,",
#   "      items: {",
#   "        dropdown: {",
#   "          name: 'Edit',",
#   "          type: 'select',",
#   "          options: options,",
#   "          selected: 0",
#   "        }",
#   "      },",
#   "      events: {",
#   "        show: function(opts){",
#   "          opts.$trigger.off('blur');",
#   "        },",
#   "        hide: function(opts){",
#   "          var $this = this;",
#   "          var data = $.contextMenu.getInputValues(opts, $this.data());",
#   "          var $input = opts.$trigger;",
#   "          $input.val(options[data.dropdown]);",
#   "          $input.trigger('change');",
#   "        }",
#   "      }",
#   "    };",
#   "  }",
#   "});"
# )
# 
# createdCell <- function(levels){
#   if(missing(levels)){
#     return("function(td, cellData, rowData, rowIndex, colIndex){}")
#   }
#   quotedLevels <- toString(sprintf("\"%s\"", levels))
#   c(
#     "function(td, cellData, rowData, rowIndex, colIndex){",
#     sprintf("  $(td).attr('data-levels', '[%s]');", quotedLevels),
#     "}"
#   )
# }
# 
# ui <- fluidPage(
#   tags$head(
#     tags$link(
#       rel = "stylesheet",
#       href = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.css"
#     ),
#     tags$script(
#       src = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.js"
#     )
#   ),
#   DTOutput("dtable")
# )
# 
# server <- function(input, output){
#   output[["dtable"]] <- renderDT({
#     datatable(
#       iris, editable = "cell", callback = JS(callback),
#       options = list(
#         columnDefs = list(
#           list(
#             targets = 5,
#             className = "factor",
#             createdCell = JS(createdCell(c(levels(iris$Species), "another level")))
#           )
#         )
#       )
#     )
#   }, server = FALSE)
# }
# 
# shinyApp(ui, server)
# 
# 
# 
# 






# https://community.rstudio.com/t/saving-editable-dt-table-values-to-reactivevalue-in-shiny/48825/3

### Libraries
library(shiny)
library(dplyr)
library(DT)

### Data
# input_data <- data.frame(Brand = c("Brand1", "Brand2","Brand3"),
#                          ratio = c (.5, .5, .5),
#                          cost = c(2000, 3000, 4000),
#                          stringsAsFactors = FALSE) %>% 
#   mutate(updated_price = cost * ratio)

input_data <- stationTableUI 


### Module
modFunction <- function(input, output, session, data,reset) {
  
  v <- reactiveValues(data = data)
  
  proxy = dataTableProxy("mod_table")
  
  observeEvent(input$mod_table_cell_edit, {
    print(names(v$data))
    info = input$mod_table_cell_edit
    str(info)
    i = info$row
    j = info$col
    k = info$value
    str(info)
    
    isolate(        v$data[i, j] <<- DT::coerceValue(k, v$data[i, j]) )
    replaceData(proxy, v$data, resetPaging = FALSE)  # replaces data displayed by the updated table
  })
  
  ### Reset Table
  observeEvent(reset(), {
    v$data <- data # your default data
  })
  
  print(isolate(colnames(v$data)))
  output$mod_table <- DT::renderDataTable({
    DT::datatable(v$data, selection = 'none', editable = TRUE,
                  options = list(dom = 't', scrollX = TRUE, scrollY = '200px'))
    
  })
  
  return(v)
}

modFunctionUI <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("mod_table"))
  
}

### Shiny App
shinyApp(
  ui = basicPage(
    mainPanel(
      
      actionButton("reset", "Reset"),
      tags$hr(),
      modFunctionUI("editable")
    )
  ),
  server = function(input, output) {
    demodata<-input_data
    edited <- callModule(modFunction,"editable", demodata,
                         reset = reactive(input$reset))
    data_df_final <- reactiveValues()
    observe(
      {data_df_final$data <- edited$data}
    )
    observe(print(data_df_final$data))
  }
)

