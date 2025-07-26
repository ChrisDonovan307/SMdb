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
#' @import shinycssloaders
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
#' @param query_trigger Reactive value that triggers data refresh (e.g., button click)
#'
#' @return A list containing reactive functions for database operations
#'
#' @noRd
mod_database_server <- function(id,
                                dimension_input = reactive(NULL),
                                index_input = reactive(NULL),
                                indicator_input = reactive(NULL),
                                geography_input = reactive(NULL),
                                meta_input = reactive(NULL),
                                query_trigger = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Establish database connection
    con <- db_connect()

    fips_key <- dbGetQuery(con, 'SELECT * FROM fips_key')
    metadata <- dbGetQuery(con, 'SELECT * FROM metadata')
    framework <- select(metadata, dimension, index, indicator, metric, variable_name)

    # Reactive function to get available FIPS codes
    get_fips_choices <- reactive({
      dbGetQuery(con, 'SELECT DISTINCT fips FROM metrics ORDER BY fips') %>%
        filter(fips %in% fips_key$fips) %>%
        pull(fips) %>%
        unique() %>%
        sort()
    })

    # Reactive function to get index choices based on user inputs and metadata
    get_index_choices <- reactive({
      dimension_filter = dimension_input()
      out <- framework

      if (!is.null(dimension_filter) && any(dimension_filter != '')) {
        out <- out %>%
          filter(dimension %in% tolower(dimension_filter))
      }

      sort(unique(out$index))
    })

    # Get reactive indicator choices based on user inputs
    get_indicator_choices <- reactive({
      dimension_filter = dimension_input()
      index_filter = index_input()
      out <- framework

      if (!is.null(dimension_filter) && any(dimension_filter != '')) {
        out <- out %>%
          filter(dimension %in% tolower(dimension_filter))
      }
      if (!is.null(index_filter) && any(index_filter != '')) {
        out <- out %>%
          filter(index %in% tolower(index_filter))
      }

      sort(unique(out$indicator))
    })

    # Get reactive metric choices based on user inputs
    get_metric_choices <- reactive({
      dimension_filter = dimension_input()
      index_filter = index_input()
      indicator_filter = indicator_input()
      meta_filter = meta_input()
      out <- framework

      # Filter by dimension
      if (!is.null(dimension_filter) && any(dimension_filter != '')) {
        out <- out %>%
          filter(dimension %in% tolower(dimension_filter))
      }

      # Filter by index
      if (!is.null(index_filter) && any(index_filter != '')) {
        out <- out %>%
          filter(index %in% tolower(index_filter))
      }

      # Filter by indicator
      if (!is.null(indicator_filter) && any(indicator_filter != '')) {
        out <- out %>%
          filter(indicator %in% tolower(indicator_filter))
      }

      # Return filtered metrics
      sort(unique(out$metric))
    })

    # internal data ----
    # Function to filter metrics data based on parameters
    # NOTE: this could be pulled out as a utils function
    # and DRY refactor
    get_filtered_data_internal <- function(dimension_filter = NULL,
                                           # fips_filter = NULL,
                                           index_filter = NULL,
                                           indicator_filter = NULL,
                                           metric_filter = NULL,
                                           meta_filter = NULL,
                                           geography_filter = NULL) {

      dat_db <- tbl(con, 'metrics')

      # Filter by geography ----
      if (!is.null(geography_filter) && length(geography_filter) > 0 && any(geography_filter != '')) {
        if (geography_filter == 'Counties') {
          dat_db <- filter(dat_db, str_length(fips) == 5)
        } else if (geography_filter == 'States') {
          dat_db <- filter(dat_db, str_length(fips) == 2)
        }
      }

      # Filter by framework ----
      # Start filtering by indicator, go backward through index, then dimension
      # So we avoid filtering more than we need to if we went dim -> indicator
      if (!is.null(indicator_filter) && length(indicator_filter) > 0 && any(indicator_filter != '')) {
        match <- unique(framework$variable_name[framework$indicator %in% tolower(indicator_filter)])
        dat_db <- filter(dat_db, variable_name %in% match)
      } else if (!is.null(dimension_filter) && length(dimension_filter) > 0 && any(dimension_filter != '')) {
        match <- framework$variable_name[framework$dimension %in% tolower(dimension_filter)]
        dat_db <- filter(dat_db, variable_name %in% match)
      } else if (!is.null(index_filter) && length(index_filter) > 0 && any(index_filter != '')) {
        match <- unique(framework$variable_name[framework$index %in% tolower(index_filter)])
        dat_db <- filter(dat_db, variable_name %in% match)
      } else if (!is.null(metric_filter) && length(metric_filter) > 0 && any(metric_filter != '')) {
        match <- unique(framework$variable_name[tolower(framework$metric) %in% tolower(metric_filter)])
        dat_db <- filter(dat_db, variable_name %in% match)
      }

      # # Filter by FIPS ----
      # if (!is.null(fips_filter) && length(fips_filter) > 0 && any(fips_filter != '')) {
      #   dat_db <- filter(dat_db, fips %in% fips_filter)
      # }

      # Add metadata ----
      # TODO: we can join with fixed metadata object if we want until after
      # we collect dat_db. Should be less querying DB
      if (!is.null(meta_input) && meta_input()) {
        meta_db <- tbl(con, 'metadata')
        dat_db <- meta_db %>%
          select(
            dimension,
            index,
            indicator,
            metric,
            variable_name,
            definition,
            units
          ) %>%
          right_join(dat_db)
      }

      collect(dat_db)
    }

    # get filtered_data ----
    # Store the filtered data that only updates when query button is clicked
    filtered_data <- reactiveVal(data.frame())

    # Update filtered data only when query_trigger changes (button is clicked)
    observeEvent(query_trigger(), {
      req(query_trigger()) # Ensure the trigger is not NULL/0
      showPageSpinner(
        type = 6,
        size = 1,
        color = 'black',
        # color = '#154734',
        caption = HTML('Loading... Large queries can take a few seconds')
      )

      new_data <- get_filtered_data_internal(
        dimension_filter = dimension_input(),
        index_filter = index_input(),
        indicator_filter = indicator_input(),
        geography_filter = geography_input()
      )

      hidePageSpinner()
      filtered_data(new_data)
    })

    # Reactive function that returns the stored filtered data
    get_filtered_data <- reactive({
      filtered_data()
    })

    # Return list of functions that other modules can use
    return(list(
      # get_fips_choices = get_fips_choices,
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
