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
    mod_database_ui(ns('database')),
    page_sidebar(
      sidebar = sidebar(
        tags$h3("Filters"),
        selectizeInput(
          ns('select_dimension'),
          "Dimension",
          choices = c('Economics', 'Environment', 'Human', 'Production', 'Social'),
          multiple = TRUE
        ),
        selectizeInput(
          ns('select_index'),
          "Index",
          choices = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          ns('select_indicator'),
          "Indicator",
          choices = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          ns('select_metric'),
          "Metric",
          choices = NULL,
          multiple = TRUE
        ),
        selectizeInput(
          ns('select_fips'),
          "FIPS Code",
          choices = NULL,
          multiple = TRUE
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

    # Use database module
    database_functions <- mod_database_server('database')

    # Update filter choices using database module functions
    observe({
      updateSelectizeInput(
        inputId = "select_fips",
        choices = database_functions$get_fips_choices(),
        server = TRUE,
        selected = NULL
      )
    })
    observe({
      updateSelectizeInput(
        inputId = "select_dimension",
        choices = c('Economics', 'Environment', 'Health', 'Production', 'Social'),
        server = TRUE,
        selected = NULL
      )
    })
    observe({
      updateSelectizeInput(
        inputId = "select_index",
        choices = database_functions$get_index_choices(input$select_dimension),
        server = TRUE,
        selected = NULL
      )
    })
    observe({
      updateSelectizeInput(
        inputId = "select_indicator",
        choices = database_functions$get_indicator_choices(input$select_index),
        server = TRUE,
        selected = NULL
      )
    })
    observe({
      updateSelectizeInput(
        inputId = "select_metric",
        choices = database_functions$get_metric_choices(input$select_indicator),
        server = TRUE,
        selected = NULL
      )
    })

    # Database function for getting filtered data
    filtered_df <- reactive({
      database_functions$get_filtered_data(
        dimension_filter = input$select_dimension,
        index_filter = input$select_index,
        indicator_filter = input$select_indicator,
        metric_filter = input$select_metric,
        fips_filter = input$select_fips
      )
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
      names(df) <- snakecase::to_title_case(names(df))
      names(df) <- sub('Fips', 'FIPS Code', names(df))

      get_reactable(
        df,
        defaultColDef = colDef(
          headerStyle = list(
            fontWeight = "bold",
            background = '#f7f7f8',
            fontSize = '16px'
          )
        )
        # columns = list('Variable Name' = colDef(minWidth = 200))
      )
    })
  })
}

## To be copied in the UI
# mod_db_ui("db_1")

## To be copied in the server
# mod_db_server("db_1")
