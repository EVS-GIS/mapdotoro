#' Add trigger function to update talweg_metrics_id for insert in hydro_swaths.
#'
#' @param db_con DBI connection to database.
#'
#' @importFrom DBI dbExecute
#' @import glue glue
#'
#' @return text
#' @export
fct_update_hydro_swaths_talweg_metrics_id <- function(db_con){

  query <- glue::glue("
    CREATE OR REPLACE FUNCTION update_hydro_swaths_talweg_metrics_id()
    RETURNS TRIGGER AS $$
    BEGIN
      IF TG_OP = 'INSERT' THEN
        UPDATE hydro_swaths
        SET talweg_metrics_id =
          (SELECT talweg_metrics.id
          FROM talweg_metrics
          WHERE hydro_swaths.axis = talweg_metrics.axis
            AND hydro_swaths.measure_medial_axis = talweg_metrics.measure_medial_axis
          LIMIT 1)
        WHERE NEW.gid = hydro_swaths.gid;

        RETURN NEW;

      END IF;

    END;
    $$ LANGUAGE plpgsql;")

  dbExecute(db_con, query)

  return("update_talweg_metrics_id function adds to database")
}

#' Add trigger function to update talweg_metrics_id from hydro_swaths for insert or delete in talweg_metrics.
#'
#' @param db_con DBI connection to database.
#'
#' @importFrom DBI dbExecute
#' @import glue glue
#'
#' @return text
#' @export
fct_update_hydro_swaths_talweg_metrics_id_talweg_metrics <- function(db_con){

  query <- glue::glue("
    CREATE OR REPLACE FUNCTION update_hydro_swaths_talweg_metrics_id_talweg_metrics()
    RETURNS TRIGGER AS $$
    BEGIN
      IF TG_OP = 'INSERT' THEN
        UPDATE hydro_swaths
        SET talweg_metrics_id = NEW.id
        WHERE hydro_swaths.axis = NEW.axis
          AND hydro_swaths.measure_medial_axis = NEW.measure_medial_axis;

        RETURN NEW;

      ELSIF TG_OP = 'DELETE' THEN
        UPDATE hydro_swaths
        SET talweg_metrics_id = NULL
        WHERE OLD.id = hydro_swaths.talweg_metrics_id;

        RETURN OLD;

      END IF;

    END;
    $$ LANGUAGE plpgsql;")

  dbExecute(db_con, query)

  return("update_talweg_metrics_id function adds to database")
}


#' Create trigger to update talweg_metrics_id in hydro_swaths table.
#'
#' @param db_con DBI connection to database.
#'
#' @importFrom DBI dbExecute
#' @import glue glue
#'
#' @return text
#' @export
trig_update_talweg_metrics_id <- function(db_con){

  query <- glue::glue("
    CREATE OR REPLACE TRIGGER after_insert_hydro_swaths
    AFTER INSERT ON hydro_swaths
    FOR EACH ROW
    EXECUTE FUNCTION update_hydro_swaths_talweg_metrics_id();")

  dbExecute(db_con, query)

  query <- glue::glue("
    CREATE OR REPLACE TRIGGER before_delete_talweg_metrics
    BEFORE DELETE ON talweg_metrics
    FOR EACH ROW
    EXECUTE FUNCTION update_hydro_swaths_talweg_metrics_id_talweg_metrics();")

  dbExecute(db_con, query)

  query <- glue::glue("
    CREATE OR REPLACE TRIGGER aftet_insert_talweg_metrics
    AFTER INSERT ON talweg_metrics
    FOR EACH ROW
    EXECUTE FUNCTION update_hydro_swaths_talweg_metrics_id_talweg_metrics();")

  dbExecute(db_con, query)

  return("trig_update_talweg_metrics_id trigger adds to database")
}



