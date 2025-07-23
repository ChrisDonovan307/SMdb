#' Database Module UI Function
#'
#' @description A shiny Module for database connection and operations.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS
#' @import RMySQL
#' @import dplyr
#' @import dbplyr
mod_database_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' Database Module Server Functions
#'
#' @description Handles database connection and data retrieval operations
#'
#' @param id Internal parameter for {shiny}.
#'
#' @return A list containing reactive functions for database operations
#'
#' @noRd
mod_database_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Establish database connection
    con <- db_connect()

    fips_key <- dbGetQuery(con, 'SELECT * FROM fips_key')
    metadata <- dbGetQuery(con, 'SELECT * FROM metadata')
    framework <- select(metadata, dimension, index, indicator, metric, variable_name)

    # Reactive function to get available FIPS codes
    get_fips_choices <- reactive({
      vars <- dbGetQuery(con, 'SELECT DISTINCT fips FROM metrics ORDER BY fips')
      vars %>%
        filter(fips %in% fips_key$fips) %>%
        pull(fips) %>%
        unique() %>%
        sort()
    })

    # Get framework from metadata, including variable names. Use to populate filters
    get_framework <- reactive({
      tbl(con, 'metadata') %>%
        select(dimension, index, indicator, metric, variable_name) %>%
        collect()
    })

    # # Reactive function to get available metrics
    # get_metric_choices <- reactive({
    #   vars <- dbGetQuery(con, 'SELECT DISTINCT metric FROM metadata ORDER BY metric')
    #   sort(unique(vars$metric))
    # })

    get_metric_choices <- function(dimension_filter = NULL) {
      frame <- get_framework()
      if (!is.null(dimension_filter) && dimension_filter != '') {
        frame <- frame %>%
          filter(dimension == tolower(dimension_filter))
      }
      sort(frame$metric)
    }

    # Function to filter metrics data based on parameters
    get_filtered_data <- function(fips_filter = NULL,
                                  metric_filter = NULL,
                                  dimension_filter = NULL) {
      dat_db <- tbl(con, 'metrics')

      # Dimension
      if (!is.null(dimension_filter) && length(dimension_filter) > 0 && dimension_filter != '') {
        match <- framework$variable_name[framework$dimension == tolower(dimension_filter)]
        dat_db <- filter(dat_db, variable_name %in% match)
      }

      # Filter by FIPS selection
      if (!is.null(fips_filter) && length(fips_filter) > 0 && fips_filter != '') {
        dat_db <- filter(dat_db, fips == fips_filter)
      }

      # Have to join with metadata to get metrics and other
      if (!is.null(metric_filter) && length(metric_filter) > 0 && metric_filter != '') {
        meta_db <- tbl(con, 'metadata')
        dat_db <- meta_db %>%
          select(dimension, index, indicator, metric, variable_name) %>%
          left_join(dat_db) %>%
          filter(metric == metric_filter)
        print('\n*Filtered dat_db*')
      }

      collect(dat_db)
    }

    # Return list of functions that other modules can use
    return(list(
      get_fips_choices = get_fips_choices,
      get_metric_choices = get_metric_choices,
      get_filtered_data = get_filtered_data,
      connection = reactive(con)
    ))
  })
}

## To be copied in the UI
# mod_database_ui("database_1")

## To be copied in the server
# mod_database_server("database_1")
