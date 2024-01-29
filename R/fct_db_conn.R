#' Postgresql database connection.
#'
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres
#'
#' @return PqConnection
#' @export
db_con <- function(){
  db_con <- DBI::dbConnect(RPostgres::Postgres(),
                           host = Sys.getenv("DBMAPDO_HOST"),
                           port = Sys.getenv("DBMAPDO_PORT"),
                           dbname = Sys.getenv("DBMAPDO_NAME"),
                           user      = Sys.getenv("DBMAPDO_USER"),
                           password  = Sys.getenv("DBMAPDO_PASS"))
  return(db_con)
}
