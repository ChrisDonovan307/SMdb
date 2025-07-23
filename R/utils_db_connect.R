#' Connect to DB
#'
#' @description Connect to MySQL DB where metrics data are hosted on Silk WebDB
#'   Currently not set up with any parameters. Accesses using FSRC_READER, which
#'   cannot make any changes to DB. Automatically closes connection onStop.
#' @return A connection object
#' @import RMySQL
#' @import DBI
#' @noRd
db_connect <- function() {
  readRenviron('~/.Renviron')
  con <- dbConnect(
    MySQL(),
    user = Sys.getenv('fsrc_reader'),
    password = Sys.getenv('fsrc_reader_pass'),
    host = Sys.getenv('webdb_host'),
    dbname = 'FSRC_METRICS'
  )
  onStop(function() {
    if (exists("con") && !is.null(con)) {
      dbDisconnect(con)
    }
  })
  return(con)
}
