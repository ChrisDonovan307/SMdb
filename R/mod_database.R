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

  # Don't need a ui for this, all server side
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

    # Function to get available index choices based on inputs and metadata
    get_index_choices <- function(dimension_filter = NULL) {
      if (!is.null(dimension_filter) && any(dimension_filter != '')) {
        out <- metadata %>%
          filter(dimension %in% tolower(dimension_filter)) %>%
          pull(index) %>%
          unique()
      } else {
        out <- metadata$index %>%
          unique()
      }
      sort(out)
    }

    # Get indicator choices
    get_indicator_choices <- function(index_filter = NULL) {
      if (!is.null(index_filter) && any(index_filter != '')) {
        out <- metadata %>%
          filter(index %in% tolower(index_filter)) %>%
          pull(indicator) %>%
          unique()
      } else {
        out <- metadata$indicator %>%
          unique()
      }
      sort(out)
    }

    # Function to get available metric choices based on metadata
    get_metric_choices <- function(indicator_filter = NULL) {
      if (!is.null(indicator_filter) && any(indicator_filter != '')) {
        out <- metadata %>%
          filter(indicator %in% tolower(indicator_filter)) %>%
          pull(metric)
      } else {
        out <- metadata$metric
      }
      sort(out)
    }

    # Function to filter metrics data based on parameters
    # NOTE: this could be pulled out as a utils function
    get_filtered_data <- function(fips_filter = NULL,
                                  dimension_filter = NULL,
                                  index_filter = NULL,
                                  indicator_filter = NULL,
                                  metric_filter = NULL) {
      # frame <- tbl(con, 'metadata') %>%
      #   select(dimension, index, indicator, metric, variable_name) %>%
      #   unique()

      dat_db <- tbl(con, 'metrics')
        # left_join(frame)

      # Start with indicator, if nothing, go backward through index, then dimension
      if (!is.null(indicator_filter) && length(indicator_filter) > 0 && any(indicator_filter != '')) {
        match <- unique(framework$variable_name[framework$indicator_filter %in% tolower(indicator_filter)])
        dat_db <- filter(dat_db, variable_name %in% match)
      } else if (!is.null(dimension_filter) && length(dimension_filter) > 0 && any(dimension_filter != '')) {
        match <- framework$variable_name[framework$dimension %in% tolower(dimension_filter)]
        dat_db <- filter(dat_db, variable_name %in% match)
      } else if (!is.null(index_filter) && length(index_filter) > 0 && any(index_filter != '')) {
        match <- unique(framework$variable_name[framework$index %in% tolower(index_filter)])
        dat_db <- filter(dat_db, variable_name %in% match)
      } else if (!is.null(metric_filter) && length(metric_filter) > 0 && any(metric_filter != '')) {
        match <- unique(framework$variable_name[framework$metric %in% tolower(metric_filter)])
        dat_db <- filter(dat_db, variable_name %in% match)
      }

      # Filter by FIPS selection
      if (!is.null(fips_filter) && length(fips_filter) > 0 && any(fips_filter != '')) {
        dat_db <- filter(dat_db, fips %in% fips_filter)
      }

      # Add framework info
      meta_db <- tbl(con, 'metadata')
      # browser()
      dat_db <- meta_db %>%
        select(dimension, index, indicator, metric, variable_name) %>%
        left_join(dat_db)

      collect(dat_db)
    }

    # Return list of functions that other modules can use
    return(list(
      get_fips_choices = get_fips_choices,
      get_index_choices = get_index_choices,
      get_indicator_choices = get_indicator_choices,
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
