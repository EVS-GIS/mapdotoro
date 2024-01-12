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
