#' Prepare landcover area to database export.
#'
#' @param dataset data.frame landcover imported.
#'
#' @importFrom dplyr filter rename_all rename
#'
#' @return data.frame landcover area prepared.
#' @export
prepare_landcover_area <- function(dataset = input_landcover){

  landcover_area_prepared <- prepare_landcover_continuity_area(dataset) %>%
    rename_all(clean_column_names) %>%
    rename(measure_medial_axis = measure)

  # check for duplicate (should not print red L'axe axe_number a des doublons !)
  landcover_area_prepared_left <- landcover_area_prepared %>%
    filter(side == "left")

  cat("check duplicated for left side", "\n")
  landcover_area_duplicated_left <- check_duplicate(dataset = landcover_area_prepared_left,
                                                    axis_field = "axis",
                                                    measure_field = "measure_medial_axis")
  # clean dataset if duplicated found
  if (nrow(landcover_area_duplicated_left$duplicated_rows)>0){
    clean_duplicated(landcover_area_prepared,
                     landcover_area_duplicated_left$duplicated_rows,
                     axis_field = "axis",
                     measure_field = "measure_medial_axis")
  }

  landcover_area_prepared_right <- landcover_area_prepared %>%
    filter(side == "right")

  cat("check duplicated for right side", "\n")
  landcover_area_duplicated_right <- check_duplicate(dataset = landcover_area_prepared_right,
                                                     axis_field = "axis",
                                                     measure_field = "measure_medial_axis")

  # clean dataset if duplicated found
  if (nrow(landcover_area_duplicated_right$duplicated_rows)>0){
    clean_duplicated(landcover_area_prepared,
                     landcover_area_duplicated_right$duplicated_rows,
                     axis_field = "axis",
                     measure_field = "measure_medial_axis")
  }

  return(landcover_area_prepared)
}

#' Export landcover area to database
#'
#' @param dataset landcover data.frame.
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
pg_export_landcover_area <- function(dataset = landcover_area_prepared,
                                     table_name = "landcover_area",
                                     drop_existing_table = FALSE,
                                     db_con){

  landcover_area <- dataset %>%
    rename_all(clean_column_names) %>%
    rename(measure_medial_axis = measure)

  table_exist <- dbExistsTable(db_con, table_name)
  if (table_exist){
    if (drop_existing_table){
      query <- glue::glue("DROP TABLE {table_name} CASCADE")
      dbExecute(db_con, query)
      cat(glue::glue("{table_name} has been dropped from the database"), "\n")
      dbWriteTable(conn = db_con, name = table_name,
                   value = landcover_area)
      cat(glue::glue("{table_name} has been created"), "\n")
    }else{
      stop("Process stopped because the table exists and drop_existing_table is FALSE.")
    }
  } else {
    dbWriteTable(conn = db_con, name = table_name,
                 value = landcover_area)
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
