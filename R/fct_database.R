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
#' @importFrom sf st_write st_transform
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
    ALTER TABLE {table_name}
    ADD CONSTRAINT unq_code_bassin
    UNIQUE (cdbh);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    CREATE INDEX idx_code_bassin
    ON {table_name} USING btree(cdbh);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    ALTER TABLE {table_name} ADD PRIMARY KEY (gid);")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    CREATE INDEX idx_gid_{table_name}
    ON {table_name} USING btree(gid);")
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

  return(glue::glue("{table_name} display column set"))
}

#' Export region_hydrographique data to postgreslq table
#'
#' @param dataset region_hydrographique sf data.frame.
#' @param table_name region_hydrographique table name.
#' @param drop_existing_table if destination table remove with CASCADE.
#' @param db_con database connection.
#'
#' @importFrom dplyr rename_all select
#' @importFrom rmapshaper ms_simplify
#' @importFrom DBI dbExistsTable dbExecute
#' @importFrom sf st_write st_transform
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
    CREATE INDEX idx_gid_{table_name}
    ON {table_name} USING btree(gid);")
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
