#' get_reactable
#'
#' @description Create a reactable table with sensible defaults.
#'
#' @return A reactable table object
#' @importFrom reactable reactable
#' @noRd
get_reactable <- function(df,
                          sortable = TRUE,
                          resizable = TRUE,
                          filterable = TRUE,
                          searchable = TRUE,
                          pagination = TRUE,
                          bordered = TRUE,
                          wrap = TRUE,
                          rownames = FALSE,
                          onClick = 'select',
                          striped = TRUE,
                          defaultPageSize = 10,
                          showPageSizeOptions = TRUE,
                          highlight = TRUE,
                          style = list(fontSize = "14px"),
                          compact = TRUE,
                          fullWidth = TRUE,
                          ...) {
  reactable(
    df,
    sortable = sortable,
    resizable = resizable,
    filterable = filterable,
    searchable = searchable,
    pagination = pagination,
    bordered = bordered,
    wrap = wrap,
    rownames = rownames,
    onClick = onClick,
    striped = striped,
    defaultPageSize = defaultPageSize,
    showPageSizeOptions = showPageSizeOptions,
    highlight = highlight,
    style = style,
    compact = compact,
    fullWidth = fullWidth,
    ...
  )
}
