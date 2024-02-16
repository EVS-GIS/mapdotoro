#' Get hydrologic sites from Hubeau API.
#'
#' @importFrom httr2 request req_perform resp_body_json
#' @importFrom dplyr bind_rows filter distinct
#' @importFrom sf st_as_sf
#'
#' @return sf data.frame with hydrometric sites
#' @export
import_hydro_sites <- function(){

  url = "https://hubeau.eaufrance.fr/api/v1/hydrometrie/referentiel/sites?format=json&size=10000"

  api_response <- request(url) %>%
    req_perform()

  data_api <- resp_body_json(api_response)

  # remove geometry column
  filtered_data_list <- lapply(data_api$data, function(x) {
    x[c(names(x)[names(x) != "geometry"])]
  })

  sites <- bind_rows(filtered_data_list) %>%
    st_as_sf(coords = c("longitude_site","latitude_site"), crs = 4326) %>%
    filter(statut_site == 1) %>% # with hydrologic signification (see : http://id.eaufrance.fr/ddd/HYD/2.1/StatutSiteHydro)
    filter(!is.na(code_site)) %>%
    distinct(code_site, .keep_all = TRUE) # remove duplicates

  return(sites)
}

#' Prepare hydrometric sites to database export.
#'
#' @param dataset sf data.frame hydrometric sites imported.
#' @param region_hydro sf data.frame hydrologic regions to set gid_region.
#'
#' @importFrom dplyr mutate select filter rename left_join
#' @importFrom sf st_join st_contains st_transform st_set_geometry st_drop_geometry st_as_sf
#'
#' @return sf data.frame hydrometric sites prepared.
#' @export
prepare_hydro_sites <- function(dataset = input_hydro_sites,
                                region_hydro = region_hydrographique){

  sites <- dataset %>%
    st_set_geometry("geom") %>%
    st_transform(2154)

  sites_prepared <- region_hydro %>%
    st_join(sites, join = st_contains, prepared = TRUE) %>% # st_contains with region_hydro far more faster than st_within!
    filter(!is.na(code_site)) %>%
    select(code_site, gid) %>%
    rename(gid_region = gid) %>%
    st_drop_geometry() %>%
    left_join(sites, by = "code_site") %>%
    rename_all(clean_column_names) %>%
    mutate(url_site = paste0("https://www.hydro.eaufrance.fr/sitehydro/",code_site,"/fiche")) %>%
    select(code_site, url_site, libelle_site, type_site, altitude_site,
           code_systeme_alti_site, surface_bv, statut_site, code_entite_hydro_site,
           code_troncon_hydro_site, code_zone_hydro_site,
           code_cours_eau, uri_cours_eau, grandeur_hydro, date_maj_site,
           libelle_cours_eau, gid_region, geom) %>%
    st_as_sf()

  return(sites_prepared)
}

#' Create hydro_sites table structure.
#'
#' @param table_name table name.
#' @param db_con DBI database connection.
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute dbDisconnect
#'
#' @return text
#' @export
create_table_hydro_sites <- function(table_name = "hydro_sites",
                                               db_con){
  query <- glue::glue("
    CREATE TABLE public.{table_name} (
    gid SERIAL PRIMARY KEY,
    code_site text,
    url_site text,
    libelle_site text,
    type_site text,
    altitude_site double precision,
    code_systeme_alti_site integer,
    surface_bv double precision,
    statut_site integer,
    code_entite_hydro_site text,
    code_troncon_hydro_site text,
    code_zone_hydro_site text,
    code_cours_eau text,
    uri_cours_eau text,
    grandeur_hydro text,
    date_maj_site text,
    libelle_cours_eau text,
    gid_region integer,
    geom public.geometry,
    -- constraints
    CONSTRAINT {table_name}_unq_code_site UNIQUE (code_site),
    CONSTRAINT fk_{table_name}_gid_region FOREIGN KEY(gid_region)
      REFERENCES region_hydrographique(gid)
);")
  dbExecute(db_con, query)

  query <- glue::glue("
    CREATE INDEX idx_geom_{table_name}
    ON {table_name} USING GIST (geom);")
  dbExecute(db_con, query)

  query <- glue::glue("
    CREATE INDEX idx_gid_region_{table_name}
    ON hydro_sites USING btree(gid_region);")
  dbExecute(db_con, query)

  reader <- Sys.getenv("DBMAPDO_DEV_READER")
  query <- glue::glue("
    GRANT SELECT ON {table_name}
    TO {reader};")
  dbExecute(db_con, query)

  dbDisconnect(db_con)

  return(glue::glue("{table_name} has been successfully created"))
}

#' Delete existing rows and insert hydrometric sites to database.
#'
#' @param dataset sf data.frame hydrometric sites
#' @param table_name text database table name.
#' @param db_con DBI connection to database.
#' @param field_identifier text field identifier name to identified rows to remove.
#'
#' @importFrom sf st_write st_transform
#' @importFrom DBI dbExecute dbDisconnect
#' @importFrom glue glue
#'
#' @return text
#' @export
upsert_hydro_sites <- function(dataset = hydro_sites,
                                  table_name = "hydro_sites",
                                  db_con,
                                  field_identifier = "code_site"){

  sites <- dataset %>%
    st_transform(crs = 4326)

  remove_rows(dataset = sites,
              field_identifier = field_identifier,
              table_name = table_name)

  st_write(obj = sites, dsn = db_con, layer = table_name, append = TRUE)

  rows_insert <- nrow(sites)

  dbDisconnect(db_con)

  return(glue::glue("{table_name} updated with {rows_insert} inserted"))
}
