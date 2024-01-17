#' Prepare continuity area to database export.
#'
#' @param dataset data.frame continuity imported.
#'
#' @importFrom dplyr filter rename_all rename
#'
#' @return data.frame continuity area prepared.
#' @export
prepare_continuity_area <- function(dataset = input_continuity){

  continuity_area_prepared <- prepare_landcover_continuity_area(dataset) %>%
    rename_all(clean_column_names) %>%
    rename(measure_medial_axis = measure)

  # check for duplicate (should not print red L'axe axe_number a des doublons !)
  continuity_area_prepared_left <- continuity_area_prepared %>%
    filter(side == "left")

  cat("check duplicated for left side", "\n")
  continuity_area_duplicated_left <- check_duplicate(dataset = continuity_area_prepared_left,
                                                    axis_field = "axis",
                                                    measure_field = "measure_medial_axis")
  # clean dataset if duplicated found
  if (nrow(continuity_area_duplicated_left$duplicated_rows)>0){
    clean_duplicated(continuity_area_prepared,
                     continuity_area_duplicated_left$duplicated_rows,
                     axis_field = "axis",
                     measure_field = "measure_medial_axis")
  }

  continuity_area_prepared_right <- continuity_area_prepared %>%
    filter(side == "right")

  cat("check duplicated for right side", "\n")
  continuity_area_duplicated_right <- check_duplicate(dataset = continuity_area_prepared_right,
                                                     axis_field = "axis",
                                                     measure_field = "measure_medial_axis")

  # clean dataset if duplicated found
  if (nrow(continuity_area_duplicated_right$duplicated_rows)>0){
    clean_duplicated(continuity_area_prepared,
                     continuity_area_duplicated_right$duplicated_rows,
                     axis_field = "axis",
                     measure_field = "measure_medial_axis")
  }

  return(continuity_area_prepared)
}

#' Create continuity_area table structure.
#'
#' @param table_name table name.
#' @param db_con DBI database connection.
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute dbDisconnect
#'
#' @return text
#' @export
create_table_continuity_area <- function(table_name = "continuity_area",
                                        db_con){
  query <- glue::glue("
    CREATE TABLE public.{table_name} (
    id BIGSERIAL PRIMARY KEY,
    side text,
    axis double precision,
    measure_medial_axis double precision,
    water_channel double precision,
    active_channel double precision,
    riparian_buffer double precision,
    connected_meadows double precision,
    connected_cultivated double precision,
    disconnected double precision,
    built double precision,
    no_data double precision,
    sum_area double precision,
    water_channel_pc double precision,
    active_channel_pc double precision,
    riparian_buffer_pc double precision,
    connected_meadows_pc double precision,
    connected_cultivated_pc double precision,
    disconnected_pc double precision,
    built_pc double precision,
    no_data_pc double precision,
    hydro_swaths_gid bigint
    );")
  dbExecute(db_con, query)

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT fk_{table_name}_hydro_swaths_gid
    FOREIGN KEY(hydro_swaths_gid)
    REFERENCES hydro_swaths(gid) ON DELETE CASCADE;")
  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return(glue::glue("{table_name} has been successfully created"))
}


















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
