#' Get hydrologic stations from Hubeau API.
#'
#' @param url text ecoulement stations from hubeau API.
#'
#' @importFrom httr2 request req_perform resp_body_json
#' @importFrom dplyr bind_rows filter
#' @importFrom sf st_as_sf
#'
#' @return sf data.frame with hydrologic stations
#' @export
import_hydro_stations <- function(url = "https://hubeau.eaufrance.fr/api/v1/ecoulement/stations?format=json"){
  api_response <- request(url) %>%
    req_perform()

  data_api <- resp_body_json(api_response)

  # remove geometry column
  filtered_data_list <- lapply(data_api$data, function(x) {
    x[c(names(x)[names(x) != "geometry"])]
  })

  stations <- bind_rows(filtered_data_list) %>%
    st_as_sf(coords = c("longitude","latitude"), crs = 4326) %>%
    filter(etat_station == "Active") %>%
    filter(!is.na(code_station))

  return(stations)
}

#' Prepare hydrologic stations to database export.
#'
#' @param dataset sf data.frame hydrologic stations imported.
#' @param region_hydro sf data.frame hydrologic regions to set gid_region.
#'
#' @importFrom dplyr mutate select
#' @importFrom sf st_join st_within
#'
#' @return sf data.frame hydrologic stations prepared.
#' @export
prepare_hydro_stations <- function(dataset = input_hydro_stations,
                                   region_hydro = region_hydrographique){

  st_geometry(dataset) <- "geom" # in case if geometry column name is not "geom"

  stations <- dataset %>%
    rename_all(clean_column_names) %>%
    st_join(region_hydro, join = st_within) %>%
    mutate(gid_region = gid) %>%
    select(-colnames(region_hydro)[colnames(region_hydro) != "geom"])

  return(stations)
}

#' Create hydro_stations table structure.
#'
#' @param table_name table name.
#' @param db_con DBI database connection.
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute
#'
#' @return text
#' @export
create_table_hydro_stations <- function(table_name = "hydro_stations",
                                               db_con){
  query <- glue::glue("
    CREATE TABLE public.{table_name} (
    gid SERIAL PRIMARY KEY,
    code_station text,
    libelle_station text,
    uri_station text,
    code_departement text,
    libelle_departement text,
    code_commune text,
    libelle_commune text,
    code_region text,
    libelle_region text,
    code_bassin text,
    libelle_bassin text,
    coordonnee_x_station double precision,
    coordonnee_y_station double precision,
    code_projection_station text,
    libelle_projection_station text,
    code_epsg_station text,
    code_cours_eau text,
    libelle_cours_eau text,
    uri_cours_eau text,
    etat_station text,
    date_maj_station text,
    gid_region integer,
    geom public.geometry
);")
  dbExecute(db_con, query)

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT unq_code_station
    UNIQUE (code_station);")
  dbExecute(db_con, query)

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
    ON hydro_stations USING btree(gid_region);")
  dbExecute(db_con, query)
  cat(query, "\n")

  return(glue::glue("{table_name} has been successfully created"))
}

#' Delete existing rows and insert hydrologic stations to database.
#'
#' @param dataset sf data.frame hydrologic stations.
#' @param table_name text database table name.
#' @param db_con DBI connection to database.
#' @param field_identifier text field identifier name to identified rows to remove.
#'
#' @importFrom sf st_write st_transform
#' @importFrom DBI dbExecute
#' @importFrom glue glue
#'
#' @return text
#' @export
upsert_hydro_stations <- function(dataset = hydro_stations,
                                  table_name = "hydro_stations",
                                  db_con,
                                  field_identifier = "code_station"){

  stations <- dataset %>%
    st_transform(crs = 4326)

  remove_rows(dataset = stations,
              field_identifier = field_identifier,
              table_name = table_name)

  st_write(obj = stations, dsn = db_con, layer = table_name, append = TRUE)

  rows_insert <- nrow(stations)

  return(glue::glue("{table_name} updated with {rows_insert} inserted"))
}
