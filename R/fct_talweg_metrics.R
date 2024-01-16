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
    sinuosity double precision
    );")
  dbExecute(db_con, query)

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT {table_name}_unq_axis_measure
    UNIQUE (axis, measure_medial_axis);")
  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return(glue::glue("{table_name} has been successfully created"))
}

#' Add trigger function to react from talweg_metrics insert or delete.
#'
#' @param db_con DBI connection to database.
#'
#' @importFrom DBI dbExecute dbDisconnect
#' @import glue glue
#'
#' @return text
#' @export
fct_talweg_metrics_insert_delete_reaction <- function(db_con){

  query <- glue::glue("
    CREATE OR REPLACE FUNCTION talweg_metrics_insert_delete_reaction()
    RETURNS TRIGGER AS $$
    BEGIN
      IF TG_OP = 'INSERT' THEN
        -- update talweg_metrics_id from hydro_swaths
        UPDATE hydro_swaths
        SET talweg_metrics_id = NEW.id
        WHERE hydro_swaths.axis = NEW.axis
          AND hydro_swaths.measure_medial_axis = NEW.measure_medial_axis;

        RETURN NEW;

      ELSIF TG_OP = 'DELETE' THEN
        -- set talweg_metrics_id to NULL from hydro_swaths
        UPDATE hydro_swaths
        SET talweg_metrics_id = NULL
        WHERE OLD.id = hydro_swaths.talweg_metrics_id;

        RETURN OLD;

      END IF;

    END;
    $$ LANGUAGE plpgsql;")

  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return("talweg_metrics_insert_delete_reaction function added to database")
}

#' Create trigger to update tables from talweg_metrics modifications.
#'
#' @param db_con DBI connection to database.
#'
#' @importFrom DBI dbExecute dbDisconnect
#' @import glue glue
#'
#' @return text
#' @export
trig_talweg_metrics <- function(db_con){

  query <- glue::glue("
    CREATE OR REPLACE TRIGGER aftet_insert_talweg_metrics
    AFTER INSERT ON talweg_metrics
    FOR EACH ROW
    EXECUTE FUNCTION talweg_metrics_insert_delete_reaction();")
  dbExecute(db_con, query)

  query <- glue::glue("
    CREATE OR REPLACE TRIGGER before_delete_talweg_metrics
    BEFORE DELETE ON talweg_metrics
    FOR EACH ROW
    EXECUTE FUNCTION talweg_metrics_insert_delete_reaction();")
  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return("talweg_metrics triggers added to database")
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
