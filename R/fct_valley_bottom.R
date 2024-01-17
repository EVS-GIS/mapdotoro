#' Prepare valley_bottom to database export.
#'
#' @param dataset data.frame valley_bottom imported.
#'
#' @importFrom dplyr filter rename_all rename select
#'
#' @return data.frame continuity width prepared.
#' @export
prepare_valley_bottom <- function(dataset = input_valley_bottom){

  valley_bottom_prepared <- dataset %>%
    rename_all(clean_column_names) %>%
    rename(measure_medial_axis = measure) %>%
    select (-c(width2, swath)) %>%
    rename(width = width1)

  # check for duplicate (should not print red L'axe axe_number a des doublons !)
  valley_bottom_prepared_left <- valley_bottom_prepared %>%
    filter(side == "left")

  cat("check duplicated for left side", "\n")
  valley_bottom_duplicated_left <- check_duplicate(dataset = valley_bottom_prepared_left,
                                                  axis_field = "axis",
                                                  measure_field = "measure_medial_axis")

  # clean dataset if duplicated found
  if (nrow(valley_bottom_duplicated_left$duplicated_rows)>0){
    clean_duplicated(valley_bottom_prepared,
                     valley_bottom_duplicated_left$duplicated_rows,
                     axis_field = "axis",
                     measure_field = "measure_medial_axis")
  }

  valley_bottom_prepared_right <- valley_bottom_prepared %>%
    filter(side == "right")

  cat("check duplicated for right side", "\n")
  valley_bottom_duplicated_right <- check_duplicate(dataset = valley_bottom_prepared_right,
                                                       axis_field = "axis",
                                                       measure_field = "measure_medial_axis")

  # clean dataset if duplicated found
  if (nrow(valley_bottom_duplicated_right$duplicated_rows)>0){
    clean_duplicated(valley_bottom_prepared,
                     valley_bottom_duplicated_right$duplicated_rows,
                     axis_field = "axis",
                     measure_field = "measure_medial_axis")
  }

  return(valley_bottom_prepared)
}

#' Create valley_bottom table structure.
#'
#' @param table_name table name.
#' @param db_con DBI database connection.
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute dbDisconnect
#'
#' @return text
#' @export
create_table_valley_bottom <- function(table_name = "valley_bottom",
                                       db_con){
  query <- glue::glue("
    CREATE TABLE public.{table_name} (
    id BIGSERIAL PRIMARY KEY,
    axis bigint,
    measure_medial_axis bigint,
    side text,
    area double precision,
    width double precision,
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
