#' Prepare roe dataset to database export.
#'
#' @param dataset sf data.frame roe.
#' @param region_hydro sf data.frame hydrographic regions to set spatial join on gid_region.
#' @param troncon_bdtopo_id data.frame with id_troncon and axis.
#' @param hydro_axis sf data.frame with hydrologic axis prepared.
#'
#' @importFrom dplyr select mutate filter rename left_join rename_all left_join
#' @importFrom qgisprocess qgis_configure qgis_run_algorithm_p
#' @importFrom sf st_join st_geometry st_contains st_transform st_drop_geometry st_zm st_filter
#'
#' @return sf data.frame
#' @export
prepare_roe <- function(dataset = input_roe,
                        region_hydro = region_hydrographique,
                        troncon_bdtopo_id = input_troncon_bdtopo_id,
                        hydro_axis = hydro_axis){

  # swaths_axis <- unique(swaths$AXIS)

  roe <- dataset %>%
    st_transform(2154)

  roe_prepared <- region_hydro %>%
    # st_contains with region_hydro far more faster than st_within with roe!
    st_join(roe, join = st_contains, prepared = TRUE) %>%
    filter(!is.na(CdObstEcou)) %>%
    select(CdObstEcou, gid.x) %>%
    rename(gid_region = gid.x) %>%
    st_drop_geometry() %>%
    left_join(roe, by = "CdObstEcou") %>%
    rename_all(clean_column_names) %>%
    st_as_sf() %>%
    # add axis from troncon BDTOPO id
    left_join(troncon_bdtopo_id, by = c("idtronco_1" = "id_troncon")) %>%
    filter(!is.na(axis)) %>%  # drop NA
    filter(axis %in% unique(hydro_axis$axis))

  qgis_configure() # qgis_process configuration set up

  # snap ROE on axis
  roe_prepared_snaped <- roe_prepared %>%
    qgis_run_algorithm_p("native:snapgeometries",
                         BEHAVIOR = 0,
                         REFERENCE_LAYER = hydro_swaths_and_axis$hydro_axis,
                         TOLERANCE = 100)

  # get only ROE fitted on lines for ROE locate upstreams below stream network resolution
  roe_prepared_on_axis <- st_read(roe_prepared_snaped$OUTPUT) %>%
    st_filter(hydro_swaths_and_axis$hydro_axis, .predicate = st_intersects)

  # axis preparation for ROE linear referencing
  identifynetworknodes <- hydro_swaths_and_axis$hydro_axis %>%
    qgis_run_algorithm_p("fct:identifynetworknodes",
                         QUANTIZATION = 100000000)

  # if some duplicated NODES
  identifynetworknodes_fixed <- st_read(identifynetworknodes$OUTPUT) %>%
    filter(NODEA != NODEB)

  # add M measure to network
  measurenetworkfromoutlet <- identifynetworknodes_fixed %>%
    select(-length) %>% # can't have 2 length field
    qgis_run_algorithm_p("fct:measurenetworkfromoutlet",
                         FROM_NODE_FIELD = "NODEA",
                         TO_NODE_FIELD = "NODEB")

  # ROE linear referencing
  roe_along_network <- roe_prepared_on_axis %>%
    qgis_run_algorithm_p("fct:locatepointalongnetwork",
                         LINEAR_REFERENCE = st_read(measurenetworkfromoutlet$OUTPUT),
                         AXIS_ID_FIELD = "axis")

  # rename and remove columns
  roe_referenced <- st_read(pointsalongnetwork$OUTPUT) %>%
    rename("distance_axis" = "LOCM") %>%
    select(-c("AXIS_0", "DISTANCE", "SIDE")) %>%
    st_zm()

  return(roe_referenced)
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
    gid bigint PRIMARY KEY,
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
    axis bigint,
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

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT fk_{table_name}_axis
    FOREIGN KEY(axis)
    REFERENCES hydro_axis(axis);")
  dbExecute(db_con, query)

  reader <- Sys.getenv("DBMAPDO_DEV_READER")
  query <- glue::glue("
    GRANT SELECT ON {table_name}
    TO {reader};")
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
