#' Export talweg metrics to database
#'
#' @param dataset talweg metrics data.frame.
#' @param table_name database table name.
#' @param drop_existing_table if destination table remove with CASCADE.
#' @param db_con DBI connection to database.
#'
#' @importFrom dplyr rename_all
#' @importFrom DBI dbExistsTable dbExecute dbWriteTable
#' @importFrom glue glue
#'
#' @return text
#' @export
pg_export_talweg_metrics <- function(dataset = talweg_metrics_prepared,
                                     table_name = "talweg_metrics",
                                     drop_existing_table = FALSE,
                                     db_con){

  talweg_metrics <- dataset %>%
    rename_all(clean_column_names)

  table_exist <- dbExistsTable(db_con, table_name)
  if (table_exist){
    if (drop_existing_table){
      query <- glue::glue("DROP TABLE {table_name} CASCADE")
      dbExecute(db_con, query)
      cat(glue::glue("{table_name} has been dropped from the database"), "\n")
      dbWriteTable(conn = db_con, name = table_name,
                   value = talweg_metrics)
      cat(glue::glue("{table_name} has been created"), "\n")
    }else{
      stop("Process stopped because the table exists and drop_existing_table is FALSE.")
    }
  } else {
    dbWriteTable(conn = db_con, name = table_name,
                 value = talweg_metrics)
    cat(glue::glue("{table_name} has been created"), "\n")
  }

  query <- glue::glue("
    ALTER TABLE {table_name} ADD COLUMN id SERIAL PRIMARY KEY;")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT {table_name}_unq_axis_measure
    UNIQUE (axis, measure_medial_axis);")
  dbExecute(db_con, query)
  cat(query, "\n")

  return(glue::glue("{table_name} has been successfully set up"))
}
