#' Prepare continuity area to database export.
#'
#' @param dataset data.frame continuity imported.
#'
#' @importFrom dplyr filter rename_all rename
#'
#' @return data.frame continuity area prepared.
#' @export
prepare_continuity_area <- function(dataset = input_continuity){

  continuity_area_prepared <- pivot_landcover_continuity_area(dataset) %>%
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
    REFERENCES hydro_swaths(gid) ON DELETE SET NULL;")
  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return(glue::glue("{table_name} has been successfully created"))
}

#' Add trigger function to react from continuity_area insert or delete.
#'
#' @param db_con DBI connection to database.
#' @param table_name table name.
#'
#' @importFrom DBI dbExecute
#' @import glue glue
#'
#' @return text
#' @export
fct_continuity_area_insert_delete_reaction <- function(db_con,
                                                       table_name = "continuity_area"){

  query <- glue::glue("
    CREATE OR REPLACE FUNCTION {table_name}_insert_delete_reaction()
    RETURNS TRIGGER AS $$
    BEGIN
      IF TG_OP = 'INSERT' THEN
        -- update hydro_swaths_gid from {table_name}
        UPDATE {table_name}
        SET hydro_swaths_gid =
          (SELECT hydro_swaths.gid
          FROM hydro_swaths
          WHERE hydro_swaths.axis = {table_name}.axis
           AND hydro_swaths.measure_medial_axis = {table_name}.measure_medial_axis
          LIMIT 1)
        WHERE NEW.id = {table_name}.id;

        RETURN NEW;

      END IF;

    END;
    $$ LANGUAGE plpgsql;")

  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return(cat(glue::glue("{table_name}_insert_delete_reaction function added to database"), "\n"))
}

#' Create trigger to update tables from continuity_area modifications.
#'
#' @param db_con DBI connection to database.
#' @param table_name table name.
#'
#' @importFrom DBI dbExecute dbDisconnect
#' @import glue glue
#'
#' @return text
#' @export
trig_continuity_area <- function(db_con,
                                 table_name = "continuity_area"){

  query <- glue::glue("
    CREATE OR REPLACE TRIGGER after_insert_{table_name}
    AFTER INSERT ON {table_name}
    FOR EACH ROW
    EXECUTE FUNCTION {table_name}_insert_delete_reaction();")
  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return(cat(glue::glue("{table_name} triggers added to database"), "\n"))
}

#' Delete existing rows and insert continuity area to database.
#'
#' @param dataset sf data.frame continuity area.
#' @param table_name text database table name.
#' @param db_con DBI connection to database.
#' @param field_identifier text field identifier name to identified rows to remove.
#'
#' @importFrom DBI dbExecute dbWriteTable dbDisconnect
#' @importFrom glue glue
#'
#' @return text
#' @export
upsert_continuity_area <- function(dataset = continuity_area,
                                  table_name = "continuity_area",
                                  db_con,
                                  field_identifier = "axis"){

  continuity <- dataset %>%
    as.data.frame()

  remove_rows(dataset = continuity,
              field_identifier = field_identifier,
              table_name = table_name)

  dbWriteTable(conn = db_con, name = table_name, value = continuity, append = TRUE)

  rows_insert <- nrow(continuity)

  dbDisconnect(db_con)

  return(glue::glue("{table_name} updated with {rows_insert} inserted"))
}
