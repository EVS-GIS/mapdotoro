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
#' @param db_con DBI database connection.
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute
#'
#' @return text
#' @export
create_table_region_hydrographique <- function(table_name = "region_hydrographique",
                                               db_con){
  query <- glue::glue("
    CREATE TABLE public.{table_name} (
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
    ALTER TABLE {table_name}
    ADD CONSTRAINT fk_region_gid_bassin
    FOREIGN KEY(gid_bassin)
    REFERENCES bassin_hydrographique(gid);")
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
    gid SERIAL PRIMARY KEY,
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

#' Create roe table structure
#'
#' @param table_name table name.
#' @param db_con DBI database connection.
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute
#'
#' @return text
#' @export
create_table_roe <- function(table_name = "roe",
                             db_con){

  query <- glue::glue("
    CREATE TABLE public.{table_name} (
    gid SERIAL PRIMARY KEY,
    cdobstecou text,
    stobstecou text,
    cdmodevali text,
    lbmodevali text,
    cdetouvrag text,
    lbetouvrag text,
    nomprincip text,
    nomseconda text,
    cdtypeouvr text,
    lbtypeouvr text,
    coordxpoin double precision,
    coordypoin double precision,
    typecoordp text,
    cdtypedisp text,
    lbtypedisp text,
    cdtypedi_1 text,
    lbtypedi_1 text,
    cdtypedi_2 text,
    lbtypedi_2 text,
    cdtypedi_3 text,
    lbtypedi_3 text,
    cdtypedi_4 text,
    lbtypedi_4 text,
    cdtypeelmo text,
    lbtypeelmo text,
    cdtypeel_1 text,
    lbtypeel_1 text,
    cdtypeel_2 text,
    lbtypeel_2 text,
    cdtypedi_5 text,
    lbtypedi_5 text,
    cdtypedi_6 text,
    lbtypedi_6 text,
    cdtypedi_7 text,
    lbtypedi_7 text,
    cdusageobs text,
    lbusageobs text,
    cdusageo_1 text,
    lbusageo_1 text,
    cdusageo_2 text,
    lbusageo_2 text,
    cdusageo_3 text,
    lbusageo_3 text,
    hautmaxter text,
    hautchutet text,
    cdhautchut text,
    lbhautchut text,
    datemajobs date,
    datevalido date,
    grenobstec text,
    ouvragelie text,
    idtronconh text,
    nomentiteh text,
    cdtronconh text,
    cdentitehy text,
    cdzonehydr text,
    idtronco_1 text,
    cddepartem text,
    lbdepartem text,
    cdcommune text,
    lbcommune text,
    numcircadm text,
    nomcircadm text,
    cdeumassed text,
    altipointc double precision,
    nomlimiteh text,
    denmaxouvr double precision,
    pkobstecou double precision,
    gid_region int,
    geom public.geometry
);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT unq_cdobstecou
    UNIQUE (cdobstecou);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    CREATE INDEX idx_geom_{table_name}
    ON {table_name} USING GIST (geom);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT fk_{table_name}_gid_region
    FOREIGN KEY(gid_region)
    REFERENCES region_hydrographique(gid);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    CREATE INDEX idx_gid_region_{table_name}
    ON {table_name} USING btree(gid_region);")
  dbExecute(db_con, query)
  cat(query, "\n")

  return(glue::glue("{table_name} has been successfully created"))
}
