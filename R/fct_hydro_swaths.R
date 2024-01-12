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

#' Delete existing rows and insert hydrologic network splited by swaths to database
#'
#' @param dataset hydrologic network splited sf data.frame.
#' @param table_name database table name.
#' @param db_con DBI connection to database.
#' @param region_hydrographique_file_path file path to region hydrographique dataset.
#'
#' @importFrom sf st_write
#' @importFrom DBI dbExecute
#' @importFrom glue glue
#'
#' @return text
#' @export
upsert_hydro_swaths <- function(dataset = hydro_swaths_prepared,
                                table_name = "hydro_swaths",
                                db_con){

  # rows to removed in database
  rows_to_remove <- paste0("(", toString(unique(dataset$axis)), ")")

  # remove rows in table
  query <- glue::glue("
    DELETE FROM {table_name} WHERE axis IN {rows_to_remove};")
  dbExecute(db_con, query)
  cat(query, "\n")

  st_write(obj = dataset, dsn = db_con, layer = table_name, append = TRUE)

  return(glue::glue("{table_name} updated"))
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

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD COLUMN talweg_metrics_id int;")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    UPDATE {table_name}
    SET talweg_metrics_id = talweg_metrics.id
    FROM talweg_metrics
    WHERE {table_name}.axis = talweg_metrics.axis
      AND {table_name}.measure_medial_axis = talweg_metrics.measure_medial_axis;")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT fk_{table_name}_talweg_metrics_id
    FOREIGN KEY(talweg_metrics_id)
    REFERENCES talweg_metrics(id);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD COLUMN hydro_axis_gid int;")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    UPDATE {table_name}
    SET hydro_axis_gid = hydro_axis.gid
    FROM hydro_axis
    WHERE {table_name}.axis = hydro_axis.axis;")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    ALTER TABLE {table_name}
    ADD CONSTRAINT fk_{table_name}_hydro_axis_gid
    FOREIGN KEY(hydro_axis_gid)
    REFERENCES hydro_axis(gid);")
  dbExecute(db_con, query)
  cat(query, "\n")

  return(glue::glue("{table_name} has been successfully set up"))
}
