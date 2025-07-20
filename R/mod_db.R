#' db UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinipsum
mod_db_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns('random_plot'))
  )
}

#' db Server Functions
#'
#' @noRd
mod_db_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # observeEvent(trigger(), {
      output$random_plot <- renderPlot({
        random_ggplot()
      # })
    })
  })
}

## To be copied in the UI
# mod_db_ui("db_1")

## To be copied in the server
# mod_db_server("db_1")
