#' Export continuity area to database
#'
#' @param dataset continuity data.frame.
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
pg_export_continuity_area <- function(dataset = continuity_area_prepared,
                                      table_name = "continuity_area",
                                      drop_existing_table = FALSE,
                                      db_con){

  continuity_area <- dataset %>%
    rename_all(clean_column_names)

  table_exist <- dbExistsTable(db_con, table_name)
  if (table_exist){
    if (drop_existing_table){
      query <- glue::glue("DROP TABLE {table_name} CASCADE")
      dbExecute(db_con, query)
      cat(glue::glue("{table_name} has been dropped from the database"), "\n")
      dbWriteTable(conn = db_con, name = table_name,
                   value = continuity_area)
      cat(glue::glue("{table_name} has been created"), "\n")
    }else{
      stop("Process stopped because the table exists and drop_existing_table is FALSE.")
    }
  } else {
    dbWriteTable(conn = db_con, name = table_name,
                 value = continuity_area)
    cat(glue::glue("{table_name} has been created"), "\n")
  }

  query <- glue::glue("
    ALTER TABLE {table_name} ADD COLUMN id SERIAL PRIMARY KEY;")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD COLUMN hydro_swaths_gid int;")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    UPDATE {table_name}
    SET hydro_swaths_gid = hydro_swaths.gid
    FROM hydro_swaths
    WHERE {table_name}.axis = hydro_swaths.axis
      AND {table_name}.measure_medial_axis = hydro_swaths.measure_medial_axis;")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT fk_{table_name}_hydro_swaths_gid
    FOREIGN KEY(hydro_swaths_gid)
    REFERENCES hydro_swaths(gid);")
  dbExecute(db_con, query)
  cat(query, "\n")

  return(glue::glue("{table_name} has been successfully set up"))
}
