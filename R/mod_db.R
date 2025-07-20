#' db UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import RMySQL
#' @import bslib
#' @import reactable
#' @import dplyr
#' @import dbplyr
#' @import shinyWidgets
mod_db_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_sidebar(
      sidebar = sidebar(
        tags$h3("Filters"),
        selectizeInput(
          ns('select_variable'),
          "Metric",
          choices = NULL,
          multiple = FALSE
        ),
        selectizeInput(
          ns('select_fips'),
          "FIPS Code",
          choices = NULL,
          multiple = FALSE
        ),
        actionBttn(
          ns("query"),
          "Query",
          color = 'success',
          style = 'pill'
        )
      ),
      uiOutput(ns('table_ui'))
    )
  )
}

#' db Server Functions
#'
#' @noRd
mod_db_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    con <- db_connect()

    # Get choices for filters
    fips_available <- dbGetQuery(
      con,
      'SELECT DISTINCT fips FROM metrics ORDER BY fips'
    )
    updateSelectizeInput(
      inputId = "select_fips",
      choices = unique(fips_available$fips),
      server = TRUE
    )

    # Filter metrics table from database lazily
    filtered_df <- reactive({
      dat_db <- tbl(con, 'metrics')
      if (!is.null(input$select_fips) && length(input$select_fips) > 0) {
        dat_db <- dat_db %>%
          filter(fips == input$select_fips)
      }
      collect(dat_db)
    })

    # Make table appear when user hits query
    output$table_ui <- renderUI({
      req(input$query)
      reactable::reactableOutput(ns('table'))
    })

    # Use filtered df from db to make table
    output$table <- renderReactable({
      df <- filtered_df()
      if ('id' %in% names(df)) df$id <- NULL
      names(df) <- c('FIPS', 'Year', 'Variable Name', 'Value')

      get_reactable(
        df,
        defaultColDef = colDef(
          headerStyle = list(
            fontWeight = "bold",
            background = '#f7f7f8',
            fontSize = '16px'
          )
        ),
        columns = list('Variable Name' = colDef(minWidth = 200))
      )
    })
  })
}

## To be copied in the UI
# mod_db_ui("db_1")

## To be copied in the server
# mod_db_server("db_1")
