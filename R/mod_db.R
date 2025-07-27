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
#' @import shinycssloaders
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
          ns('select_state'),
          'States',
          choices = c(
            'Northeast',
            'Connecticut',
            'Maine',
            'Massachusetts',
            'New Hampshire',
            'New Jersey',
            'New York',
            'Pennsylvania',
            'Rhode Island',
            'Vermont'
          ),
          multiple = TRUE,
          selected = 'Northeast'
        ),
        selectInput(
          ns('select_geography'),
          'Geography',
          choices = c('All', 'Counties', 'States'),
          multiple = FALSE
        ),
        awesomeCheckbox(
          ns('select_meta'),
          label = "Include metadata",
          status = 'primary',
          value = FALSE
        ),
        actionBttn(
          ns("query"),
          "Query",
          color = 'primary',
          style = 'fill',
          icon = icon(
            name = 'magnifying-glass',
            lib = 'font-awesome'
          )
        ),
        downloadBttn(
          ns('download'),
          'Download',
          color = 'primary',
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

    # observe({
    #   browser()
    #   print(input$select_meta)
    # })

    # mod_database functions ----
    database_functions <- mod_database_server(
      'database',
      dimension_input = reactive(input$select_dimension),
      index_input = reactive(input$select_index),
      indicator_input = reactive(input$select_indicator),
      metric_input = reactive(input$select_metric),
      meta_input = reactive(input$select_meta),
      geography_input = reactive(input$select_geography),
      state_input = reactive(input$select_state),
      query_trigger = reactive(input$query)
    )

    # Filter dropdowns ----
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


    # Filtered data ----
    filtered_df <- database_functions$get_filtered_data


    # Table output ----
    # Make table appear when user hits query
    output$table_ui <- renderUI({
      req(input$query)
      reactable::reactableOutput(ns('table'))
    })

    # Use filtered df from db to make table
    output$table <- renderReactable({
      showPageSpinner(
        type = 6,
        size = 1,
        color = 'black',
        caption = 'Building table...'
      )
      df <- filtered_df()

      if ('id' %in% names(df)) df$id <- NULL
      names(df) <- snakecase::to_title_case(names(df))
      names(df) <- sub('Fips', 'FIPS Code', names(df))

      out <- get_reactable(
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
      hidePageSpinner()
      out
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
        write.csv(filtered_df(), file, row.names = FALSE)
      },
      contentType = 'text/csv'
    )

  })
}

## To be copied in the UI
# mod_db_ui("db_1")

## To be copied in the server
# mod_db_server("db_1")
