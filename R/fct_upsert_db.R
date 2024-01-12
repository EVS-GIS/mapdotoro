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

#' Delete existing rows and insert hydrologic bassin to database
#'
#' @param dataset sf data.frame hydrologic bassin.
#' @param table_name text database table name.
#' @param db_con DBI connection to database.
#' @param field_identifier text field identifier name to identified rows to remove.
#'
#' @importFrom sf st_write st_transform
#' @importFrom DBI dbExecute
#' @importFrom glue glue
#' @importFrom rmapshaper ms_simplify
#'
#' @return text
#' @export
upsert_bassin_hydrographique <- function(dataset = bassin_hydrographique,
                                table_name = "bassin_hydrographique",
                                db_con,
                                field_identifier = "cdbh"
                                ){

  bassin_hydro <- dataset %>%
    st_transform(crs = 4326) %>%
    ms_simplify(keep = 0.01, keep_shapes = TRUE, weighting = 0.5)

  remove_rows(dataset = bassin_hydro,
              field_identifier = field_identifier,
              table_name = table_name)

  st_write(obj = bassin_hydro, dsn = db_con, layer = table_name, append = TRUE)

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

  return(glue::glue("{table_name} updated"))
}

#' Delete existing rows and insert hydrologic region to database
#'
#' @param dataset sf data.frame hydrologic region.
#' @param table_name database table name.
#' @param db_con DBI connection to database.
#'
#' @importFrom sf st_write st_cast st_zm st_transform
#' @importFrom DBI dbExecute
#' @importFrom glue glue
#' @importFrom rmapshaper ms_simplify
#'
#' @return text
#' @export
upsert_region_hydrographique <- function(dataset = region_hydrographique,
                                         table_name = "region_hydrographique",
                                         db_con,
                                         field_identifier = "cdregionhy"){

  region_hydro <- dataset %>%
    st_transform(crs = 4326) %>%
    ms_simplify(keep = 0.01, keep_shapes = TRUE, weighting = 0.5) %>%
    st_cast("MULTIPOLYGON") %>%
    st_zm()

  remove_rows(dataset = region_hydro,
              field_identifier = field_identifier,
              table_name = table_name)

  st_write(obj = region_hydro, dsn = db_con, layer = table_name, append = TRUE)

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
    UPDATE {table_name}
    SET gid_bassin =
      bassin_hydrographique.gid
    FROM bassin_hydrographique
    WHERE bassin_hydrographique.cdbh LIKE {table_name}.cdbh;")
  dbExecute(db_con, query)
  cat(query, "\n")

  query <- glue::glue("
    UPDATE {table_name}
    SET geom = ST_Force2D(geom);")
  dbExecute(db_con, query)
  cat(query, "\n")

  return(glue::glue("{table_name} updated"))
}

#' Delete existing rows and insert roe to database
#'
#' @param dataset sf data.frame roe.
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

  return(glue::glue("{table_name} updated"))
}
















#' Set display column value for bassin or region table.
#'
#' @param table_name bassin or region table name.
#' @param display_codes_bassin_or_region A vector with the list of cdbh for bassin, cdregionhy for region value to set the displayed polygons.
#' @param db_con DBI connection to database.
#' @param field_identifier text field identifier name to identified rows to remove.
#'
#' @importFrom glue glue
#' @importFrom DBI dbExistsTable dbExecute
#'
#' @return text
#' @export
set_displayed_bassin_region <- function(table_name,
                                        display_codes_bassin_or_region,
                                        field_identifier,
                                        db_con){

  displayed <- paste0("('", paste(display_codes_bassin_or_region, collapse = "','"), "')")

  table_exist <- dbExistsTable(db_con, table_name)

  if (table_exist){
    query <- glue::glue("
    UPDATE {table_name}
    SET display = TRUE
    WHERE {field_identifier} in {displayed};")
    dbExecute(db_con, query)
    cat(query, "\n")

    query <- glue::glue("
    UPDATE {table_name}
    SET display = FALSE
    WHERE {field_identifier} not in {displayed}; ")
    dbExecute(db_con, query)
    cat(query, "\n")
  } else {
    stop(glue::glue("{table_name} not existing in database."))
  }

  return(glue::glue("{table_name} display column set up"))
}

#' Remove rows in database table based on field identifier.
#'
#' @param dataset sf data.frame dataset.
#' @param field_identifier text field identifier name to identified rows to remove.
#' @param table_name text database table name.
#'
#' @importFrom glue glue
#' @importFrom DBI dbExecute
#'
#' @return text number of row deleted.
#' @export
remove_rows <- function(dataset,
                        field_identifier,
                        table_name){

  field_identifier_value <- unique(dataset[[field_identifier]])

  # rows to removed in database
  if (length(field_identifier_value)>0){
    if(is.character(field_identifier_value)){
      rows_to_remove <- paste0("('", paste(field_identifier_value, collapse = "','"), "')")
    }else{
      rows_to_remove <- paste0("(", toString(field_identifier_value), ")")
    }
    # remove rows in table
    query <- glue::glue("
    DELETE FROM {table_name} WHERE {field_identifier} IN {rows_to_remove};")
    deleted_rows <- dbExecute(db_con, query)
    cat(query, "\n")

  }else{
    deleted_rows <- 0
  }

  return(cat("Row deleted :",toString(deleted_rows), "\n"))
}
