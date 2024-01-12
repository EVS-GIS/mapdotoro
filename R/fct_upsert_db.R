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
#' @importFrom sf st_write
#' @importFrom DBI dbExecute
#' @importFrom glue glue
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

  remove_rows(dataset = dataset,
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
#' @importFrom sf st_write
#' @importFrom DBI dbExecute
#' @importFrom glue glue
#'
#' @return text
#' @export
upsert_region_hydrographique <- function(dataset = region_hydrographique,
                                         table_name = "region_hydrographique",
                                         db_con){

  region_hydro <- dataset %>%
    st_transform(crs = 4326) %>%
    ms_simplify(keep = 0.01, keep_shapes = TRUE, weighting = 0.5)

  # rows to removed in database
  rows_to_remove <- paste0("(", toString(unique(region_hydro$cdbh)), ")")

  # remove rows in table
  query <- glue::glue("
    DELETE FROM {table_name} WHERE axis IN {rows_to_remove};")
  dbExecute(db_con, query)
  cat(query, "\n")

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

  return(glue::glue("{table_name} updated"))
}

#' Set display column value for bassin or region table.
#'
#' @param table_name bassin or region table name.
#' @param code_with_data A vector with the list of cdbh for bassin, cdregionhy for region value to set the displayed polygons.
#'
#' @importFrom DBI dbExistsTable dbExecute
#'
#' @return text
#' @export
set_displayed_bassin_region <- function(table_name,
                                        code_with_data){

  displayed_gid <- toString(code_with_data)

  table_exist <- dbExistsTable(db_con, table_name)

  if (table_exist){
    query <- glue::glue("
    UPDATE {table_name}
    SET display = TRUE
    WHERE gid in ({code_with_data});")
    dbExecute(db_con, query)
    cat(query, "\n")

    query <- glue::glue("
    UPDATE {table_name}
    SET display = FALSE
    WHERE gid not in ({code_with_data}); ")
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

  return(cat("Row deleted :",toString(deleted_rows)))
}
