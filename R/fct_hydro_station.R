#' Export hydrometric stations from hubeau API
#'
#' @param url hubeau url for ecoulement stations.
#' @param table_name database table name.
#' @param drop_existing_table if destination table remove with CASCADE.
#' @param db_con DBI connection to database.
#' @param region_hydrographique_file_path file path to region hydrographique dataset.
#'
#' @importFrom httr2 request req_perform resp_body_json
#' @importFrom dplyr bind_rows filter rename_all mutate select rename
#' @importFrom sf st_as_sf st_geometry st_write st_read st_join st_within
#' @importFrom DBI dbExistsTable dbExecute
#' @importFrom glue glue
#'
#' @return text
#' @export
pg_export_hubeau <- function(url = "https://hubeau.eaufrance.fr/api/v1/ecoulement/stations?format=json",
                             table_name = "hydro_stations",
                             drop_existing_table = FALSE,
                             db_con,
                             region_hydrographique_file_path = file.path("data-raw",
                                                                         "raw-datasets",
                                                                         "region_hydrographique.gpkg")){

  # region hydro is used to perform a spatial join and set gid_region in hubeau stations
  region_hydro <- st_read(dsn = file.path(region_hydrographique_file_path))
  st_geometry(region_hydro) <- "geom" # in case if geometry column name is not "geom"

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
    filter(!is.na(code_station)) %>%
    rename_all(clean_column_names) %>%
    st_join(region_hydro, join = st_within) %>%
    mutate(gid_region = gid) %>%
    select(-colnames(region_hydro)[colnames(region_hydro) != "geom"])

  st_geometry(stations) <- "geom" # in case if geometry column name is not "geom"

  table_exist <- dbExistsTable(db_con, table_name)

  if (table_exist){
    if (drop_existing_table){
      query <- glue::glue("DROP TABLE {table_name} CASCADE")
      dbExecute(db_con, query)
      cat(glue::glue("{table_name} has been dropped from the database"), "\n")
      st_write(stations, db_con, table_name, driver = "PostgreSQL")
      cat(glue::glue("{table_name} has been created"), "\n")
    }else{
      stop("Process stopped because the table exists and drop_existing_table is FALSE.")
    }
  } else {
    st_write(stations, db_con, table_name, driver = "PostgreSQL")
    cat(glue::glue("{table_name} has been created"), "\n")
  }

  query <- glue::glue("
    ALTER TABLE {table_name} ADD COLUMN gid SERIAL PRIMARY KEY;")
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
    ON hydro_stations USING btree(gid_region);")
  dbExecute(db_con, query)
  cat(query, "\n")

  return(glue::glue("{table_name} has been successfully set up"))
}
