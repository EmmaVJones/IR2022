# https://gist.github.com/MarkEdmondson1234/7e56ee7ac5caa74224327489b0849e61

library(shiny)

#' Safe subset
#'
#' @param df Dataframe
#' @param column One name of column to subset within
#' @param subset Vector of entries in column to subset to
#'
#' If column not in df, returns back the df
safeSubset <- function(df, column, subset){
  
  testthat::expect_is(df, "data.frame")
  testthat::expect_is(column, "character")
  testthat::expect_equal(length(column), 1)
  
  if(!is.null(subset)){
    testthat::expect_is(subset, "character")
  } else {
    message("Subset is NULL, returning original")
    out <- df
  }
  
  message(" # subsetting # original rows: ",nrow(df) ," column:", column, " by ", paste(subset, collapse = ", "))
  
  col <- df[[column]]
  
  if(!is.null(col)){
    out <- df[col %in% subset,]
    message("Subset rows: ", nrow(out))
  } else {
    message("Column not found:", column)
    out <- df
  }
  
  out
  
}


#' Dynamical Update of a selectInput
#'
#' Shiny Module: useage details at \link{dynamicSelect}
#'
#' @param id shiny id
#'
#' @return dynamicSelectInput
#' @export
dynamicSelectInput <- function(id, label, multiple = FALSE){
  
  ns <- shiny::NS(id)
  
  shiny::selectInput(ns("dynamic_select"), label,
                     choices = NULL, multiple = multiple, width = "100%")
  
}

#' Dynamical Update of a selectInput
#'
#' Shiny Module
#'
#' Use via \code{callModule(dynamicSelect, "name_select", the_data, "cyl")}
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param the_data data.frame containing column of choices
#' @param column The column to select from
#' @param default_select The choices to select on load
#'
#' @seealso \link{dynamicSelectInput}
#'
#' @return the_data filtered to the choice
#' @export
dynamicSelect <- function(input, output, session, the_data, column, default_select = NULL){
  
  ns <- session$ns
  
  ## update input$dynamic_select
  observe({
    shiny::validate(
      shiny::need(the_data(),"Fetching data")
    )
    dt <- the_data()
    
    testthat::expect_is(dt, "data.frame")
    testthat::expect_is(column, "character")
    
    choice <- sort(unique(dt[[column]]))
    
    updateSelectInput(session, "dynamic_select",
                      choices = choice,
                      selected = default_select)
    
  })
  
  new_data <- reactive({
    shiny::validate(
      shiny::need(input$dynamic_select,"Select data"),
      shiny::need(the_data(), "Waiting for data")
    )
    
    sd <- the_data()
    selected <- input$dynamic_select
    
    ## will return sd even if column is NULL
    safeSubset(sd, column, selected)
    
  })
  
  return(new_data)
  
}
