#' Prepare talweg metrics to database export.
#'
#' @param dataset data.frame hydrologic stations imported.
#'
#' @return data.frame talweg metrics prepared.
#' @export
prepare_talweg_metrics <- function(dataset = input_talweg_metrics){

  talweg_metrics <- dataset %>%
    rename_all(clean_column_names)

  talweg_metrics_duplicated <- check_duplicate(talweg_metrics, axis_field = "axis",measure_field = "measure")

  talweg_metrics_cleaned <- clean_duplicated(dataset = talweg_metrics,
                                              duplicated_dataset = talweg_metrics_duplicated$duplicated_rows,
                                              axis_field = "axis", measure_field = "measure") %>%
    rename("measure_medial_axis" = "measure")

  return(talweg_metrics_cleaned)
}

#' Create talweg_metrics table structure.
#'
#' @param table_name table name.
#' @param db_con DBI database connection.
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute dbDisconnect
#'
#' @return text
#' @export
create_table_talweg_metrics <- function(table_name = "talweg_metrics",
                                               db_con){
  query <- glue::glue("
    CREATE TABLE public.{table_name} (
    id SERIAL PRIMARY KEY,
    swath double precision,
    elevation_talweg double precision,
    elevation_talweg_med double precision,
    height_valley_bottom double precision,
    slope_talweg double precision,
    slope_valley_bottom double precision,
    axis bigint,
    measure_medial_axis bigint,
    sinuosity double precision,
    hydro_swaths_gid bigint
    );")
  dbExecute(db_con, query)

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT {table_name}_unq_axis_measure
    UNIQUE (axis, measure_medial_axis);")
  dbExecute(db_con, query)

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT fk_{table_name}_hydro_swaths_gid
    FOREIGN KEY(hydro_swaths_gid)
    REFERENCES hydro_swaths(gid) ON DELETE SET NULL;")
  dbExecute(db_con, query)

  reader <- Sys.getenv("DBMAPDO_DEV_READER")
  query <- glue::glue("
    GRANT SELECT ON {table_name}
    TO {reader};")
  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return(glue::glue("{table_name} has been successfully created"))
}

#' Add trigger function to react from talweg_metrics insert or delete.
#'
#' @param db_con DBI connection to database.
#' @param table_name table name.
#'
#' @importFrom DBI dbExecute dbDisconnect
#' @import glue glue
#'
#' @return text
#' @export
fct_talweg_metrics_insert_delete_reaction <- function(db_con,
                                                      table_name = "talweg_metrics"){

  query <- glue::glue("
    CREATE OR REPLACE FUNCTION {table_name}_insert_delete_reaction()
    RETURNS TRIGGER AS $$
    BEGIN
      IF TG_OP = 'INSERT' THEN
        -- update hydro_swaths_gid from {table_name}
        UPDATE {table_name}
        SET hydro_swaths_gid =
          (SELECT gid
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

#' Create trigger to update tables from talweg_metrics modifications.
#'
#' @param db_con DBI connection to database.
#' @param table_name table name.
#'
#' @importFrom DBI dbExecute dbDisconnect
#' @import glue glue
#'
#' @return text
#' @export
trig_talweg_metrics <- function(db_con,
                                table_name = "talweg_metrics"){

  query <- glue::glue("
    CREATE OR REPLACE TRIGGER aftet_insert_{table_name}
    AFTER INSERT ON {table_name}
    FOR EACH ROW
    EXECUTE FUNCTION {table_name}_insert_delete_reaction();")
  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return(cat(glue::glue("{table_name} triggers added to database"), "\n"))
}

#' Delete existing rows and insert talweg metrics to database.
#'
#' @param dataset sf data.frame talweg metrics.
#' @param table_name text database table name.
#' @param db_con DBI connection to database.
#' @param field_identifier text field identifier name to identified rows to remove.
#'
#' @importFrom DBI dbExecute dbWriteTable dbDisconnect
#' @importFrom glue glue
#'
#' @return text
#' @export
upsert_talweg_metrics <- function(dataset = talweg_metrics,
                                  table_name = "talweg_metrics",
                                  db_con,
                                  field_identifier = "axis"){

  metrics <- dataset %>%
    as.data.frame()

  remove_rows(dataset = metrics,
              field_identifier = field_identifier,
              table_name = table_name)

  dbWriteTable(conn = db_con, name = table_name, value = metrics, append = TRUE)

  rows_insert <- nrow(metrics)

  dbDisconnect(db_con)

  return(glue::glue("{table_name} updated with {rows_insert} inserted"))
}
