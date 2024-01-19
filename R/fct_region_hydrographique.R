#' Prepare region_hydrographique dataset to database export.
#'
#' @param dataset region_hydrographique sf data.frame
#'
#' @importFrom sf st_geometry st_set_geometry
#'
#' @return sf data.frame
#' @export
prepare_region_hydrographique <- function(dataset = input_region_hydrographique){

  region <- dataset %>%
    st_set_geometry("geom") %>%
    st_transform(2154) %>%
    rename_all(clean_column_names)

  return(region)
}

#' Create region_hydrographique table structure.
#'
#' @param table_name table name.
#' @param db_con DBI database connection.
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute dbDisconnect
#'
#' @return text
#' @export
create_table_region_hydrographique <- function(table_name = "region_hydrographique",
                                               db_con){
  query <- glue::glue("
    CREATE TABLE public.{table_name} (
    gid integer PRIMARY KEY,
    cdregionhy text,
    lbregionhy text,
    cdbh text,
    gid_bassin integer,
    display boolean DEFAULT false,
    geom public.geometry(MultiPolygon)
    );")
  dbExecute(db_con, query)

  query <- glue::glue("
    CREATE INDEX idx_geom_{table_name} ON public.{table_name} USING gist (geom);")
  dbExecute(db_con, query)

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT unq_code_region
    UNIQUE (cdregionhy);")
  dbExecute(db_con, query)

  query <- glue::glue("
    ALTER TABLE {table_name}
    ALTER COLUMN geom TYPE geometry(Multipolygon)
    USING ST_Multi(geom);")
  dbExecute(db_con, query)

  query <- glue::glue("
    ALTER TABLE {table_name}
    ALTER COLUMN geom TYPE geometry(Multipolygon)
    USING ST_Force2D(geom);")
  dbExecute(db_con, query)

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT fk_region_gid_bassin
    FOREIGN KEY(gid_bassin)
    REFERENCES bassin_hydrographique(gid);")
  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return(glue::glue("{table_name} has been successfully created"))
}

#' Delete existing rows and insert hydrologic region to database.
#'
#' @param dataset sf data.frame hydrologic region.
#' @param table_name database table name.
#' @param db_con DBI connection to database.
#' @param field_identifier text field identifier name to identified rows to remove.
#'
#' @importFrom sf st_write st_cast st_zm st_transform
#' @importFrom DBI dbExecute dbDisconnect
#' @importFrom glue glue
#' @importFrom rmapshaper ms_simplify
#'
#' @return text
#' @export
upsert_region_hydrographique <- function(dataset = region_hydrographique,
                                         table_name = "region_hydrographique",
                                         db_con,
                                         field_identifier = "cdregionhy"){

  region_hydro <- dataset %>%
    st_transform(crs = 4326) %>%
    ms_simplify(keep = 0.01, keep_shapes = TRUE, weighting = 0.5) %>%
    st_cast("MULTIPOLYGON") %>%
    st_zm()

  remove_rows(dataset = region_hydro,
              field_identifier = field_identifier,
              table_name = table_name)

  st_write(obj = region_hydro, dsn = db_con, layer = table_name, append = TRUE)

  query <- glue::glue("
    UPDATE {table_name}
    SET geom = ST_QuantizeCoordinates(geom, 3, 3);")
  dbExecute(db_con, query)

  query <- glue::glue("
    UPDATE {table_name}
    SET geom = ST_SnapToGrid(geom, 0.000001)")
  dbExecute(db_con, query)

  query <- glue::glue("
    UPDATE {table_name}
    SET gid_bassin =
      bassin_hydrographique.gid
    FROM bassin_hydrographique
    WHERE bassin_hydrographique.cdbh LIKE {table_name}.cdbh;")
  dbExecute(db_con, query)

  query <- glue::glue("
    UPDATE {table_name}
    SET geom = ST_Force2D(geom);")
  dbExecute(db_con, query)

  rows_insert <- nrow(region_hydro)

  dbDisconnect(db_con)

  return(glue::glue("{table_name} updated with {rows_insert} inserted"))
}
