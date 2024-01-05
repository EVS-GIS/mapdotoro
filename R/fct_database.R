#' #' Write t
#' #'
#' #' @param table_name
#' #' @param drop_existing_table
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' write_pgtable <- function(table_name,
#'                           drop_existing_table){
#'   table_exist <- dbExistsTable(db_con, table_name)
#'   if (table_exist){
#'     if (drop_existing_table){
#'       query <- glue::glue("DROP TABLE {table_name} CASCADE")
#'       dbExecute(db_con, query)
#'       cat(glue::glue("{table_name} has been dropped from the database"), "\n")
#'       st_write(bassin_hydro, db_con, table_name, driver = "PostgreSQL")
#'     }else{
#'       stop("Process stopped because the table exists and drop_existing_table is FALSE.")
#'     }
#'   } else {
#'     st_write(bassin_hydro, db_con, table_name, driver = "PostgreSQL")
#'   }
#'   return(glue::glue("{table_name} has been successfully created"), "\n")
#' }

#' Export bassin_hydrographique data to postgreslq table
#'
#' @param dataset bassin_hydrographique sf data.frame.
#' @param table_name bassin_hydrographique table name.
#' @param drop_existing_table if destination table remove with CASCADE.
#' @param db_con database connection.
#'
#' @importFrom dplyr rename_all select
#' @importFrom rmapshaper ms_simplify
#' @importFrom DBI dbExistsTable dbExecute
#' @importFrom sf st_write st_transform st_geometry
#' @importFrom glue glue
#'
#' @return text
#' @export
pg_export_bassin_hydrographique <- function(dataset = bassin_hydrographique,
                                            table_name = "bassin_hydrographique",
                                            drop_existing_table = FALSE,
                                            db_con){

  bassin_hydro <- dataset %>%
    rename_all(clean_column_names) %>%
    st_transform(crs = 4326) %>%
    ms_simplify(keep = 0.01, keep_shapes = TRUE, weighting = 0.5)

  st_geometry(bassin_hydro) <- "geom" # in case if geometry column name is not "geom"

  table_exist <- dbExistsTable(db_con, table_name)
  if (table_exist){
    if (drop_existing_table){
      query <- glue::glue("DROP TABLE {table_name} CASCADE")
      dbExecute(db_con, query)
      cat(glue::glue("{table_name} has been dropped from the database"), "\n")
      st_write(bassin_hydro, db_con, table_name, driver = "PostgreSQL")
      cat(glue::glue("{table_name} has been created"), "\n")
    }else{
      stop("Process stopped because the table exists and drop_existing_table is FALSE.")
    }
  } else {
    st_write(bassin_hydro, db_con, table_name, driver = "PostgreSQL")
    cat(glue::glue("{table_name} has been created"), "\n")
  }

  query <- glue::glue("
    ALTER TABLE {table_name} ADD PRIMARY KEY (gid);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT unq_code_bassin
    UNIQUE (cdbh);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    CREATE INDEX idx_geom_{table_name}
    ON {table_name} USING GIST (geom);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    UPDATE {table_name}
    SET geom = ST_QuantizeCoordinates(geom, 3, 3);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    UPDATE {table_name}
    SET geom = ST_SnapToGrid(geom, 0.000001)")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD COLUMN display bool;")
  dbExecute(db_con, query)
  cat(query, "\n")

  return(glue::glue("{table_name} has been successfully set up"))
}

#' Set display column value for bassin or region table.
#'
#' @param table_name bassin or region table name.
#' @param display_gid_bassin A numeric vector with gid bassin value.
#'
#' @importFrom DBI dbExistsTable dbExecute
#'
#' @return text
#' @export
set_displayed_bassin_region <- function(table_name,
                                        displayed_gid){

  displayed_gid <- toString(displayed_gid)

  table_exist <- dbExistsTable(db_con, table_name)

  if (table_exist){
    query <- glue::glue("
    UPDATE {table_name}
    SET display = TRUE
    WHERE gid in ({displayed_gid});")
    dbExecute(db_con, query)
    cat(query, "\n")

    query <- glue::glue("
    UPDATE {table_name}
    SET display = FALSE
    WHERE gid not in ({displayed_gid}); ")
    dbExecute(db_con, query)
    cat(query, "\n")
  } else {
    stop(glue::glue("{table_name} not existing in database."))
  }

  return(glue::glue("{table_name} display column set up"))
}

#' Export region_hydrographique data to postgreslq table
#'
#' @param dataset region_hydrographique sf data.frame.
#' @param table_name region_hydrographique table name.
#' @param drop_existing_table if destination table remove with CASCADE.
#' @param db_con database connection.
#'
#' @importFrom dplyr rename_all select rename
#' @importFrom rmapshaper ms_simplify
#' @importFrom DBI dbExistsTable dbExecute
#' @importFrom sf st_write st_transform st_geometry
#' @importFrom glue glue
#'
#' @return text
#' @export
pg_export_region_hydrographique <- function(dataset = region_hydrographique,
                                            table_name = "region_hydrographique",
                                            drop_existing_table = FALSE,
                                            db_con){

  bassin_hydro <- dataset %>%
    rename_all(clean_column_names) %>%
    st_transform(crs = 4326) %>%
    ms_simplify(keep = 0.01, keep_shapes = TRUE, weighting = 0.5)

  st_geometry(bassin_hydro) <- "geom" # in case if geometry column name is not "geom"

  table_exist <- dbExistsTable(db_con, table_name)
  if (table_exist){
    if (drop_existing_table){
      query <- glue::glue("DROP TABLE {table_name} CASCADE")
      dbExecute(db_con, query)
      cat(glue::glue("{table_name} has been dropped from the database"), "\n")
      st_write(bassin_hydro, db_con, table_name, driver = "PostgreSQL")
      cat(glue::glue("{table_name} has been created"), "\n")
    }else{
      stop("Process stopped because the table exists and drop_existing_table is FALSE.")
    }
  } else {
    st_write(bassin_hydro, db_con, table_name, driver = "PostgreSQL")
    cat(glue::glue("{table_name} has been created"), "\n")
  }

  query <- glue::glue("
    ALTER TABLE {table_name} ADD PRIMARY KEY (gid);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT unq_code_region
    UNIQUE (cdregionhy);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    CREATE INDEX idx_code_region
    ON {table_name} USING btree(cdregionhy);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD COLUMN gid_bassin int;")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    UPDATE {table_name}
    SET gid_bassin =
      bassin_hydrographique.gid
    FROM bassin_hydrographique
    WHERE bassin_hydrographique.cdbh LIKE {table_name}.cdbh;")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT fk_region_gid_bassin
    FOREIGN KEY(gid_bassin)
    REFERENCES bassin_hydrographique(gid);")
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

  query <- glue::glue("
    CREATE INDEX idx_geom_{table_name}
    ON {table_name} USING GIST (geom);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    UPDATE public.{table_name}
    SET geom = ST_QuantizeCoordinates(geom, 3, 3);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    UPDATE public.{table_name}
    SET geom = ST_SnapToGrid(geom, 0.000001)")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD COLUMN display bool;")
  dbExecute(db_con, query)
  cat(query, "\n")

  return(glue::glue("{table_name} has been successfully set up"))
}

#' Export roe data to postgreslq table
#'
#' @param dataset roe sf data.frame.
#' @param table_name roe table name.
#' @param drop_existing_table if destination table remove with CASCADE.
#' @param db_con database connection.
#' @param region_hydrographique_file_path file path to region hydrographique dataset.
#'
#' @importFrom dplyr rename_all select rename mutate
#' @importFrom rmapshaper ms_simplify
#' @importFrom DBI dbExistsTable dbExecute
#' @importFrom sf st_write st_transform st_read st_join st_within st_geometry
#' @importFrom glue glue
#'
#' @return text
#' @export
pg_export_roe <- function(dataset = roe,
                          table_name = "roe",
                          drop_existing_table = FALSE,
                          db_con,
                          region_hydrographique_file_path = file.path("data-raw",
                                                                      "raw-datasets",
                                                                      "region_hydrographique.gpkg")){

  # region hydro is used to perform a spatial join and set gid_region in roe
  region_hydro <- st_read(dsn = file.path(region_hydrographique_file_path))
  st_geometry(region_hydro) <- "geom" # in case if geometry column name is not "geom"

  roe <- dataset %>%
    rename_all(clean_column_names) %>%
    st_transform(crs = 4326) %>%
    rename("gid_roe" = "gid") %>%
    st_join(region_hydro, join = st_within) %>%
    mutate(gid_region = gid) %>%
    select(-colnames(region_hydro)[colnames(region_hydro) != "geom"]) %>%
    rename("gid" = "gid_roe")

  st_geometry(roe) <- "geom" # in case if geometry column name is not "geom"

  table_exist <- dbExistsTable(db_con, table_name)
  if (table_exist){
    if (drop_existing_table){
      query <- glue::glue("DROP TABLE {table_name} CASCADE")
      dbExecute(db_con, query)
      cat(glue::glue("{table_name} has been dropped from the database"), "\n")
      st_write(roe, db_con, table_name, driver = "PostgreSQL")
      cat(glue::glue("{table_name} has been created"), "\n")
    }else{
      stop("Process stopped because the table exists and drop_existing_table is FALSE.")
    }
  } else {
    st_write(roe, db_con, table_name, driver = "PostgreSQL")
    cat(glue::glue("{table_name} has been created"), "\n")
  }

  query <- glue::glue("
    ALTER TABLE {table_name} ADD PRIMARY KEY (gid);")
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

  return(glue::glue("{table_name} has been successfully set up"))
}

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

#' Export hydrologic network splited by swaths to database
#'
#' @param dataset hydrologic network splited sf data.frame.
#' @param table_name database table name.
#' @param drop_existing_table if destination table remove with CASCADE.
#' @param db_con DBI connection to database.
#' @param region_hydrographique_file_path file path to region hydrographique dataset.
#'
#' @importFrom dplyr rename_all mutate select
#' @importFrom sf st_geometry st_write st_read st_join st_within st_transform
#' @importFrom DBI dbExistsTable dbExecute
#' @importFrom glue glue
#'
#' @return text
#' @export
pg_export_hydro_swaths <- function(dataset = hydro_swaths_measured,
                                   table_name = "hydro_swaths",
                                   drop_existing_table = FALSE,
                                   db_con,
                                   region_hydrographique_file_path = file.path("data-raw",
                                                                               "raw-datasets",
                                                                               "region_hydrographique.gpkg")){

  # region hydro is used to perform a spatial join and set gid_region in hubeau stations
  region_hydro <- st_read(dsn = file.path(region_hydrographique_file_path))
  st_geometry(region_hydro) <- "geom" # in case if geometry column name is not "geom"

  hydro_swaths <- dataset %>%
    rename_all(clean_column_names) %>%
    st_transform(crs = 4326) %>%
    st_join(region_hydro, join = st_within) %>%
    mutate(gid_region = gid) %>%
    select(-colnames(region_hydro)[colnames(region_hydro) != "geom"])

  st_geometry(hydro_swaths) <- "geom" # in case if geometry column name is not "geom"

  table_exist <- dbExistsTable(db_con, table_name)
  if (table_exist){
    if (drop_existing_table){
      query <- glue::glue("DROP TABLE {table_name} CASCADE")
      dbExecute(db_con, query)
      cat(glue::glue("{table_name} has been dropped from the database"), "\n")
      st_write(hydro_swaths, db_con, table_name, driver = "PostgreSQL")
      cat(glue::glue("{table_name} has been created"), "\n")
    }else{
      stop("Process stopped because the table exists and drop_existing_table is FALSE.")
    }
  } else {
    st_write(hydro_swaths, db_con, table_name, driver = "PostgreSQL")
    cat(glue::glue("{table_name} has been created"), "\n")
  }

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

  return(glue::glue("{table_name} has been successfully set up"))
}

#' Export hydrologic network axis to database
#'
#' @param dataset hydrologic network axis sf data.frame.
#' @param table_name database table name.
#' @param drop_existing_table if destination table remove with CASCADE.
#' @param db_con DBI connection to database.
#' @param region_hydrographique_file_path file path to region hydrographique dataset.
#'
#' @importFrom dplyr rename_all mutate select
#' @importFrom sf st_geometry st_write st_read st_join st_within st_transform st_simplify
#' @importFrom DBI dbExistsTable dbExecute
#' @importFrom glue glue
#'
#' @return text
#' @export
pg_export_hydro_axis <- function(dataset = hydro_axis,
                                 table_name = "hydro_axis",
                                 drop_existing_table = FALSE,
                                 db_con,
                                 region_hydrographique_file_path = file.path("data-raw",
                                                                             "raw-datasets",
                                                                             "region_hydrographique.gpkg")){

  # region hydro is used to perform a spatial join and set gid_region in hubeau stations
  region_hydro <- st_read(dsn = file.path(region_hydrographique_file_path)) %>%
    st_transform(crs = 2154)
  st_geometry(region_hydro) <- "geom" # in case if geometry column name is not "geom"

  hydro_axis <- dataset %>%
    rename_all(clean_column_names) %>%
    st_join(region_hydro, join = st_within) %>%
    mutate(gid_region = gid) %>%
    select(-colnames(region_hydro)[colnames(region_hydro) != "geom"]) %>%
    st_simplify(preserveTopology = TRUE, dTolerance = 100) %>%
    st_transform(crs = 4326)

  st_geometry(hydro_axis) <- "geom" # in case if geometry column name is not "geom"

  table_exist <- dbExistsTable(db_con, table_name)
  if (table_exist){
    if (drop_existing_table){
      query <- glue::glue("DROP TABLE {table_name} CASCADE")
      dbExecute(db_con, query)
      cat(glue::glue("{table_name} has been dropped from the database"), "\n")
      st_write(hydro_axis, db_con, table_name, driver = "PostgreSQL")
      cat(glue::glue("{table_name} has been created"), "\n")
    }else{
      stop("Process stopped because the table exists and drop_existing_table is FALSE.")
    }
  } else {
    st_write(hydro_axis, db_con, table_name, driver = "PostgreSQL")
    cat(glue::glue("{table_name} has been created"), "\n")
  }

  query <- glue::glue("
    ALTER TABLE {table_name} ADD COLUMN gid SERIAL PRIMARY KEY;")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT {table_name}_unq_axis
    UNIQUE (axis);")
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

  return(glue::glue("{table_name} has been successfully set up"))
}
