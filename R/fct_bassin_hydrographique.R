#' Prepare bassin_hydrographique dataset to database export.
#'
#' @param dataset bassin_hydrographique sf data.frame
#'
#' @importFrom sf st_geometry
#' @importFrom dplyr select
#'
#' @return sf data.frame
#' @export
prepare_bassin_hydrographique <- function(dataset = input_bassin_hydrographique){

  st_geometry(dataset) <- "geom" # in case if geometry column name is not "geom"

  bassin <- dataset %>%
    rename_all(clean_column_names) %>%
    select(-gid)

  return(bassin)
}

#' Create bassin_hydrographique table structure.
#'
#' @param table_name table name.
#' @param db_con DBI database connection.
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute
#'
#' @return text
#' @export
create_table_bassin_hydrographique <- function(table_name = "bassin_hydrographique",
                                               db_con){
  query <- glue::glue("
    CREATE TABLE public.{table_name} (
    gid SERIAL PRIMARY KEY,
    cdbh text,
    lbbh text,
    numcircadm text,
    projcoordo text,
    display boolean DEFAULT false,
    geom public.geometry
    );")
  dbExecute(db_con, query)

  query <- glue::glue("
    CREATE INDEX idx_geom_{table_name} ON public.{table_name} USING gist (geom);")
  dbExecute(db_con, query)

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT unq_code_bassin
    UNIQUE (cdbh);")
  dbExecute(db_con, query)

  return(glue::glue("{table_name} has been successfully created"))
}

#' Delete existing rows and insert hydrologic bassin to database
#'
#' @param dataset sf data.frame hydrologic bassin.
#' @param table_name text database table name.
#' @param db_con DBI connection to database.
#' @param field_identifier text field identifier name to identified rows to remove.
#'
#' @importFrom sf st_write st_transform
#' @importFrom DBI dbExecute
#' @importFrom glue glue
#' @importFrom rmapshaper ms_simplify
#'
#' @return text
#' @export
upsert_bassin_hydrographique <- function(dataset = bassin_hydrographique,
                                         table_name = "bassin_hydrographique",
                                         db_con,
                                         field_identifier = "cdbh"){

  bassin_hydro <- dataset %>%
    st_transform(crs = 4326) %>%
    ms_simplify(keep = 0.01, keep_shapes = TRUE, weighting = 0.5)

  remove_rows(dataset = bassin_hydro,
              field_identifier = field_identifier,
              table_name = table_name)

  st_write(obj = bassin_hydro, dsn = db_con, layer = table_name, append = TRUE)

  query <- glue::glue("
    UPDATE {table_name}
    SET geom = ST_QuantizeCoordinates(geom, 3, 3);")
  dbExecute(db_con, query)

  query <- glue::glue("
    UPDATE {table_name}
    SET geom = ST_SnapToGrid(geom, 0.000001)")
  dbExecute(db_con, query)

  rows_insert <- nrow(bassin_hydro)

  return(glue::glue("{table_name} updated with {rows_insert} inserted"))
}
