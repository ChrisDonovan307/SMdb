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
        tags$p(
          # 'Use the filters below to query the database.',
          'Note that larger queries can take a couple of seconds to retrieve.'
        ),
        selectizeInput(
          ns('select_dimension'),
          'Dimension',
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
        selectInput(
          ns('select_geography'),
          'Geography',
          choices = c('All', 'Counties', 'States'),
          multiple = FALSE
        ),
        # selectizeInput(
        #   ns('select_fips'),
        #   "FIPS Code",
        #   choices = NULL,
        #   multiple = TRUE
        # ),
        actionBttn(
          ns("query"),
          "Query",
          color = 'success',
          style = 'fill',
          icon = icon(
            name = 'magnifying-glass',
            lib = 'font-awesome'
          )
        ),
        downloadBttn(
          ns('download'),
          'Download',
          color = 'success',
          style = 'fill',
          icon = icon(
            name = 'download',
            lib = 'font-awesome'
          )
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
    database_functions <- mod_database_server(
      'database',
      dimension_input = reactive(input$select_dimension),
      index_input = reactive(input$select_index),
      indicator_input = reactive(input$select_indicator),
      query_trigger = reactive(input$query)
    )

    # Update filter choices using database module functions
    # observe({
    #   updateSelectizeInput(
    #     inputId = "select_fips",
    #     choices = database_functions$get_fips_choices(),
    #     server = TRUE,
    #     selected = NULL
    #   )
    # })
    observe({
      updateSelectizeInput(
        inputId = "select_dimension",
        choices = c('Economics', 'Environment', 'Health', 'Production', 'Social'),
        server = TRUE,
        selected = NULL
      )
    })
    observe({
      index_choices <- database_functions$get_index_choices()
      updateSelectizeInput(
        inputId = "select_index",
        choices = index_choices,
        server = TRUE,
        selected = NULL
      )
    })
    observe({
      indicator_choices <- database_functions$get_indicator_choices()
      updateSelectizeInput(
        inputId = "select_indicator",
        choices = indicator_choices,
        server = TRUE,
        selected = NULL
      )
    })
    observe({
      metric_choices <- database_functions$get_metric_choices()
      updateSelectizeInput(
        inputId = "select_metric",
        choices = metric_choices,
        server = TRUE,
        selected = NULL
      )
    })

    # Database function for getting filtered data
    # filtered_df <- reactive({
    #   database_functions$get_filtered_data(
    #     dimension_filter = input$select_dimension,
    #     index_filter = input$select_index,
    #     indicator_filter = input$select_indicator,
    #     metric_filter = input$select_metric,
    #     # fips_filter = input$select_fips,
    #     geography_filter = input$select_geography
    #   )
    # })
    filtered_df <- database_functions$get_filtered_data

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

    # Download CSV of table ----
    output$download <- downloadHandler(
      filename = function() {
        paste(
          Sys.Date(),
          "_metrics.csv",
          sep = ""
        )
      },
      content = function(file) {
        out <- filtered_df() %>%
          select(
            fips,
            year,
            variable_name,
            value
          )
        write.csv(out, file, row.names = FALSE)
      },
      contentType = 'text/csv'
    )

  })
}

## To be copied in the UI
# mod_db_ui("db_1")

## To be copied in the server
# mod_db_server("db_1")
