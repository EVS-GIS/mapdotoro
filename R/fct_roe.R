#' Prepare roe dataset to database export.
#'
#' @param dataset sf data.frame roe.
#' @param region_hydro sf data.frame hydrographic regions to set spatial join on gid_region.
#'
#' @importFrom dplyr select mutate
#' @importFrom sf st_join st_geometry st_within st_transform
#'
#' @return sf data.frame
#' @export
prepare_roe <- function(dataset = input_roe,
                        region_hydro = region_hydrographique){

  st_geometry(dataset) <- "geom" # in case if geometry column name is not "geom"

  roe <- dataset %>%
    rename_all(clean_column_names) %>%
    select(-gid) %>%
    st_transform(2154) %>%
    st_join(region_hydro, join = st_within) %>%
    mutate(gid_region = gid) %>%
    select(-colnames(region_hydro)[colnames(region_hydro) != "geom"])

  return(roe)
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
    gid_region integer,
    geom public.geometry
    );")
  dbExecute(db_con, query)

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT unq_cdobstecou
    UNIQUE (cdobstecou);")
  dbExecute(db_con, query)

  query <- glue::glue("
    CREATE INDEX idx_geom_{table_name}
    ON {table_name} USING GIST (geom);")
  dbExecute(db_con, query)

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT fk_{table_name}_gid_region
    FOREIGN KEY(gid_region)
    REFERENCES region_hydrographique(gid);")
  dbExecute(db_con, query)

  query <- glue::glue("
    CREATE INDEX idx_gid_region_{table_name}
    ON {table_name} USING btree(gid_region);")
  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return(glue::glue("{table_name} has been successfully created"))
}

#' Delete existing rows and insert roe to database
#'
#' @param dataset sf data.frame roe.
#' @param table_name text database table name.
#' @param db_con DBI connection to database.
#' @param field_identifier text field identifier name to identified rows to remove.
#'
#' @importFrom sf st_write st_transform
#' @importFrom DBI dbExecute dbDisconnect dbDisconnect
#' @importFrom glue glue
#'
#' @return text
#' @export
upsert_roe <- function(dataset = roe,
                       table_name = "roe",
                       db_con,
                       field_identifier = "cdobstecou"
){

  roe_data <- dataset %>%
    st_transform(crs = 4326)

  remove_rows(dataset = roe_data,
              field_identifier = field_identifier,
              table_name = table_name)

  st_write(obj = roe_data, dsn = db_con, layer = table_name, append = TRUE)

  rows_insert <- nrow(roe_data)

  dbDisconnect(db_con)

  return(glue::glue("{table_name} updated with {rows_insert} inserted"))
}
