#' Prepare continuity width to database export.
#'
#' @param dataset data.frame continuity imported.
#'
#' @importFrom dplyr filter rename_all rename
#'
#' @return data.frame continuity width prepared.
#' @export
prepare_continuity_width <- function(dataset = input_continuity){

  continuity_width_prepared <- pivot_continuity_width(dataset) %>%
    rename_all(clean_column_names) %>%
    rename(measure_medial_axis = measure)

  # check for duplicate (should not print red L'axe axe_number a des doublons !)
  continuity_width_prepared_left <- continuity_width_prepared %>%
    filter(side == "left")

  cat("check duplicated for left side", "\n")
  continuity_width_duplicated_left <- check_duplicate(dataset = continuity_width_prepared_left,
                                                     axis_field = "axis",
                                                     measure_field = "measure_medial_axis")
  # clean dataset if duplicated found
  if (nrow(continuity_width_duplicated_left$duplicated_rows)>0){
    clean_duplicated(continuity_width_prepared,
                     continuity_width_duplicated_left$duplicated_rows,
                     axis_field = "axis",
                     measure_field = "measure_medial_axis")
  }

  continuity_width_prepared_right <- continuity_width_prepared %>%
    filter(side == "right")

  cat("check duplicated for right side", "\n")
  continuity_width_duplicated_right <- check_duplicate(dataset = continuity_width_prepared_right,
                                                      axis_field = "axis",
                                                      measure_field = "measure_medial_axis")

  # clean dataset if duplicated found
  if (nrow(continuity_width_duplicated_right$duplicated_rows)>0){
    clean_duplicated(continuity_width_prepared,
                     continuity_width_duplicated_right$duplicated_rows,
                     axis_field = "axis",
                     measure_field = "measure_medial_axis")
  }

  return(continuity_width_prepared)
}

#' Create continuity_width table structure.
#'
#' @param table_name table name.
#' @param db_con DBI database connection.
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute dbDisconnect
#'
#' @return text
#' @export
create_table_continuity_width <- function(table_name = "continuity_width",
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
    sum_width double precision,
    water_channel_pc double precision,
    active_channel_pc double precision,
    riparian_buffer_pc double precision,
    connected_meadows_pc double precision,
    connected_cultivated_pc double precision,
    disconnected_pc double precision,
    built_pc double precision,
    no_data_pc double precision,
    hydro_swaths_gid bigint,
    -- Constraints
    CONSTRAINT fk_{table_name}_hydro_swaths_gid FOREIGN KEY(hydro_swaths_gid)
      REFERENCES hydro_swaths(gid) ON DELETE SET NULL
    );")
  dbExecute(db_con, query)

  reader <- Sys.getenv("DBMAPDO_DEV_READER")
  query <- glue::glue("
    GRANT SELECT ON {table_name}
    TO {reader};")
  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return(glue::glue("{table_name} has been successfully created"))
}

#' Add trigger function to react from continuity_width insert or delete.
#'
#' @param db_con DBI connection to database.
#' @param table_name table name.
#'
#' @importFrom DBI dbExecute
#' @import glue glue
#'
#' @return text
#' @export
fct_continuity_width_insert_delete_reaction <- function(db_con,
                                                       table_name = "continuity_width"){

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
          WHERE hydro_swaths.axis = NEW.AXIS
            AND hydro_swaths.measure_medial_axis = NEW.measure_medial_axis
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

#' Create trigger to update tables from continuity_width modifications.
#'
#' @param db_con DBI connection to database.
#' @param table_name table name.
#'
#' @importFrom DBI dbExecute dbDisconnect
#' @import glue glue
#'
#' @return text
#' @export
trig_continuity_width <- function(db_con,
                                 table_name = "continuity_width"){

  query <- glue::glue("
    CREATE OR REPLACE TRIGGER after_insert_{table_name}
    AFTER INSERT ON {table_name}
    FOR EACH ROW
    EXECUTE FUNCTION {table_name}_insert_delete_reaction();")
  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return(cat(glue::glue("{table_name} triggers added to database"), "\n"))
}

#' Delete existing rows and insert continuity width to database.
#'
#' @param dataset sf data.frame continuity width.
#' @param table_name text database table name.
#' @param db_con DBI connection to database.
#' @param field_identifier text field identifier name to identified rows to remove.
#'
#' @importFrom DBI dbExecute dbWriteTable dbDisconnect
#' @importFrom glue glue
#'
#' @return text
#' @export
upsert_continuity_width <- function(dataset = continuity_width,
                                   table_name = "continuity_width",
                                   db_con,
                                   field_identifier = "axis"){

  remove_rows(dataset = dataset,
              field_identifier = field_identifier,
              table_name = table_name)

  dbWriteTable(conn = db_con, name = table_name, value = dataset, append = TRUE)

  rows_insert <- nrow(dataset)

  dbDisconnect(db_con)

  return(glue::glue("{table_name} updated with {rows_insert} inserted"))
}

#' Create continuity_width_full_side view
#'
#' @param db_con database connection parameters.
#' @param view_name view name.
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute
#'
#' @return text
#' @export
create_continuity_width_full_side_matview <- function(db_con, view_name = "continuity_width_full_side"){
  query <- glue::glue("
  CREATE MATERIALIZED VIEW {view_name} AS
    SELECT
    	axis,
    	measure_medial_axis,
    	hydro_swaths_gid,
    	SUM(water_channel) AS water_channel,
    	SUM(active_channel) AS active_channel,
    	SUM(riparian_buffer) AS riparian_buffer,
    	SUM(connected_meadows) AS connected_meadows,
    	SUM(connected_cultivated) AS connected_cultivated,
    	SUM(disconnected) AS disconnected,
    	SUM(built) AS built,
    	SUM(no_data) AS no_data,
    	SUM(sum_width) AS sum_width,
    	SUM(water_channel_pc)/2 AS water_channel_pc,
    	SUM(active_channel_pc)/2 AS active_channel_pc,
    	SUM(riparian_buffer_pc)/2 AS riparian_buffer_pc,
    	SUM(connected_meadows_pc)/2 AS connected_meadows_pc,
    	SUM(connected_cultivated_pc)/2 AS connected_cultivated_pc,
    	SUM(disconnected_pc)/2 AS disconnected_pc,
    	SUM(built_pc)/2 AS built_pc,
    	SUM(no_data_pc)/2 AS no_data_pc
    FROM continuity_width
    GROUP BY
    	hydro_swaths_gid,
    	axis,
    	measure_medial_axis;
    ")
  dbExecute(db_con, query)

  query <- glue::glue("
    CREATE INDEX idx_hydro_swaths_gid_{view_name}
    ON {view_name} USING btree(hydro_swaths_gid);")
  dbExecute(db_con, query)

  reader <- Sys.getenv("DBMAPDO_DEV_READER")
  query <- glue::glue("
    GRANT SELECT ON {view_name}
    TO {reader};")
  dbExecute(db_con, query)

  return(glue::glue("{view_name} materialized view successfully created"))
}
