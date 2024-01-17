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

#' Create landcover_area table structure.
#'
#' @param table_name table name.
#' @param db_con DBI database connection.
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute dbDisconnect
#'
#' @return text
#' @export
create_table_landcover_area <- function(table_name = "landcover_area",
                                        db_con){
  query <- glue::glue("
    CREATE TABLE public.{table_name} (
    id BIGSERIAL PRIMARY KEY,
    side text,
    axis bigint,
    measure_medial_axis bigint,
    water_channel double precision,
    gravel_bars double precision,
    natural_open double precision,
    forest double precision,
    grassland double precision,
    crops double precision,
    diffuse_urban double precision,
    dense_urban double precision,
    infrastructures double precision,
    sum_area double precision,
    water_channel_pc double precision,
    gravel_bars_pc double precision,
    natural_open_pc double precision,
    forest_pc double precision,
    grassland_pc double precision,
    crops_pc double precision,
    diffuse_urban_pc double precision,
    dense_urban_pc double precision,
    infrastructures_pc double precision,
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

#' Add trigger function to react from landcover_area insert or delete.
#'
#' @param db_con DBI connection to database.
#' @param table_name table name.
#'
#' @importFrom DBI dbExecute
#' @import glue glue
#'
#' @return text
#' @export
fct_landcover_area_insert_delete_reaction <- function(db_con,
                                                      table_name = "landcover_area"){

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

#' Create trigger to update tables from landcover_area modifications.
#'
#' @param db_con DBI connection to database.
#' @param table_name table name.
#'
#' @importFrom DBI dbExecute dbDisconnect
#' @import glue glue
#'
#' @return text
#' @export
trig_landcover_area <- function(db_con,
                                table_name = "landcover_area"){

  query <- glue::glue("
    CREATE OR REPLACE TRIGGER after_insert_{table_name}
    AFTER INSERT ON {table_name}
    FOR EACH ROW
    EXECUTE FUNCTION {table_name}_insert_delete_reaction();")
  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return(cat(glue::glue("{table_name} triggers added to database"), "\n"))
}

#' Delete existing rows and insert landcover area to database.
#'
#' @param dataset sf data.frame landcover area.
#' @param table_name text database table name.
#' @param db_con DBI connection to database.
#' @param field_identifier text field identifier name to identified rows to remove.
#'
#' @importFrom DBI dbExecute dbWriteTable dbDisconnect
#' @importFrom glue glue
#'
#' @return text
#' @export
upsert_landcover_area <- function(dataset = landcover_area,
                                  table_name = "landcover_area",
                                  db_con,
                                  field_identifier = "axis"){

  landcover <- dataset %>%
    as.data.frame()

  remove_rows(dataset = landcover,
              field_identifier = field_identifier,
              table_name = table_name)

  dbWriteTable(conn = db_con, name = table_name, value = landcover, append = TRUE)

  rows_insert <- nrow(landcover)

  dbDisconnect(db_con)

  return(glue::glue("{table_name} updated with {rows_insert} inserted"))
}
