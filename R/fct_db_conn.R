#' Postgresql database connection.
#'
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres
#'
#' @return PqConnection
#' @export
db_con <- function(){
  db_con <- DBI::dbConnect(RPostgres::Postgres(),
                           host = Sys.getenv("DBMAPDO_HOST_TEST"),
                           port = Sys.getenv("DBMAPDO_PORT_TEST"),
                           dbname = Sys.getenv("DBMAPDO_NAME_TEST"),
                           user      = Sys.getenv("DBMAPDO_USER_TEST"),
                           password  = Sys.getenv("DBMAPDO_PASS_TEST"))
  return(db_con)
}
