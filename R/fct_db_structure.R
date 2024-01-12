#' Create bassin_hydrographique table structure
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
  cat(query, "\n")

  query <- glue::glue("
    CREATE INDEX idx_geom_{table_name} ON public.{table_name} USING gist (geom);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT unq_code_bassin
    UNIQUE (cdbh);")
  dbExecute(db_con, query)
  cat(query, "\n")

  return(glue::glue("{table_name} has been successfully created"))
}

#' Create region_hydrographique table structure
#'
#' @param table_name table name.
#'
#' @return text
#' @export
create_table_public.region_hydrographique <- function(table_name = "public.region_hydrographique"){
  query <- glue::glue("
    CREATE TABLE public.region_hydrographique (
    gid SERIAL PRIMARY KEY,
    cdregionhy text,
    lbregionhy text,
    cdbh text,
    gid_bassin integer,
    display boolean DEFAULT false,
    geom public.geometry(MultiPolygon)
    );")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    CREATE INDEX idx_geom_{table_name} ON public.{table_name} USING gist (geom);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT unq_code_region
    UNIQUE (cdregionhy);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    ALTER TABLE {table_name}
    ALTER COLUMN geom TYPE geometry(Multipolygon)
    USING ST_Multi(geom);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    ALTER TABLE {table_name}
    ALTER COLUMN geom TYPE geometry(Multipolygon)
    USING ST_Force2D(geom);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    UPDATE {table_name}
    SET geom = ST_Force2D(geom);")
  dbExecute(db_con, query)
  cat(query, "\n")

  return(glue::glue("{table_name} has been successfully created"))
}








#' Create hydro_swaths table structure
#'
#' @param table_name table name.
#'
#' @return text
#' @export
create_table_hydro_swaths <- function(table_name = "hydro_swaths"){
  query <- glue::glue("
    CREATE TABLE public.{table_name} (
    axis double precision,
    measure_medial_axis double precision,
    drainage double precision,
    measure_from_outlet double precision,
    length double precision,
    strahler integer,
    gid_region double precision,
    geom public.geometry,
    talweg_metrics_id integer,
    hydro_axis_gid integer
    );")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    ALTER TABLE {table_name} ADD COLUMN gid SERIAL PRIMARY KEY;")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT {table_name}_unq_axis_measure
    UNIQUE (axis, measure_medial_axis);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    CREATE INDEX idx_geom_{table_name}
    ON {table_name} USING GIST (geom);")
  dbExecute(db_con, query)
  cat(query, "\n")

  return(glue::glue("{table_name} has been successfully created"))
}

