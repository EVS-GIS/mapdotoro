#' Prepare landcover or continuity dataset to have area for each label
#'
#' @param dataset A landuse or continuity data.frame.
#'
#' @importFrom dplyr mutate select rename_with rename sym
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_replace_all
#' @importFrom tidyselect everything
#'
#' @return data.frame
#' @export
#'
#' @examples
#' landcover_prepared <- prepare_landcover_continuity_area(landcover)
prepare_landcover_continuity_area <- function(dataset){
  # pivot landcover label on area_ha
  landcover_prepared <- dataset %>%
    mutate(area_ha = area/10000) %>%  # convert m2 to ha
    select(label, side, area_ha, axis, measure) %>%
    pivot_wider(names_from = label, values_from = area_ha) %>%
    mutate(sum_area = rowSums(select(., unique(dataset$label)), na.rm = TRUE)) # add sum for all surface in swaths

  # create columns with area in % of the total surface
  for (label in unique(dataset$label)){
    col_name <- paste0(label,"_pc")
    landcover_prepared <- landcover_prepared %>%
      mutate({{col_name}} := ifelse(is.na(!!sym(label)) |
                                      is.na(sum_area) |
                                      sum_area == 0, NA,
                                    !!sym(label) /
                                      sum_area*100))
  }

  return(landcover_prepared)
}

#' Format column names
#'
#' @param names the column names to format.
#'
#' @return A data frame with modified column names.
#' @export
#'
#' @examples
#' df <- data.frame("Column.1" = 1:3, "Column 2" = 4:6, "Column-3" = 7:9)
#' df %>%
#'   rename_all(clean_column_names)
clean_column_names <- function(names) {
  names %>%
    tolower() %>%
    gsub("\\.", "_", .) %>%
    gsub(" ", "_", .) %>%
    gsub("-", "_", .)
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

    query <- glue::glue("
    UPDATE {table_name}
    SET display = FALSE
    WHERE {field_identifier} not in {displayed}; ")
    dbExecute(db_con, query)
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

  }else{
    deleted_rows <- 0
  }

  return(cat("Row deleted :",toString(deleted_rows), "\n"))
}

#' Check for duplicate measure in streams axis.
#'
#' @param dataset sf data.frame.
#' @param axis_field axis stream id.
#' @param measure_field axis stream measure from exutoire.
#'
#' @importFrom glue glue
#' @importFrom dplyr filter
#' @importFrom sf st_sf st_drop_geometry
#'
#' @return a list with a sf data.frame with all duplicated rows and a data.frame with the number of duplicated by axis.
#' @export
#'
#' @examples
#' check_duplicate(swaths)
check_duplicate <- function(dataset,
                            axis_field = "AXIS",
                            measure_field = "M"){

  duplicated_rows <- dataset[0, ]
  duplicated_summary <- data.frame()
  # check for duplicate in M field for each axis_selected
  for (axis_selected in unique(dataset[[axis_field]])){
    net_axe <- dataset %>%
      dplyr::filter(.data[[axis_field]]==axis_selected & !is.na(.data[[measure_field]])) # !is.na to not set NA as duplicate in the axis
    if (any(duplicated(net_axe[[measure_field]])) ==TRUE){ # if duplicate identified
      duplicate <- glue::glue("L'axe {axis_selected} a des doublons")
      # message(duplicate)
      # two duplicates indices set to return all the rows involved in duplicate and not return only the first/last one.
      duplicated_rows <- rbind(duplicated_rows, net_axe[duplicated(net_axe[[measure_field]]) |
                                                          duplicated(net_axe[[measure_field]], fromLast = TRUE), ])
      # for summary we need only to have the number of duplicate measure for each axes, no need to count all the rows but just the first one.
      duplicated_summary <- rbind(duplicated_summary, data.frame(
        axis = axis_selected,
        num_duplicate = count(net_axe[duplicated(net_axe[[measure_field]]), ])$n
      ))
    }
  }

  cat(nrow(duplicated_rows), "duplicated", axis_field, measure_field, "found for", nrow(dataset), "rows.", "\n")

  return(list(duplicated_rows = duplicated_rows, duplicated_summary = duplicated_summary))
}

#' Drop all the duplicated rows from a duplicated sf data.frame.
#'
#' @param dataset sf data.frame.
#' @param duplicated_dataset sf data.frame.
#' @param axis_field axis stream id.
#' @param measure_field axis stream measure from exutoire.
#'
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr anti_join
#'
#' @return sf data.frame.
#' @export
#'
#' @examples
#' clean_duplicated(swaths,
#'                  check_duplicate(swaths))
clean_duplicated <- function(dataset,
                             duplicated_dataset,
                             axis_field = "AXIS",
                             measure_field = "M"){

  # drop geometry to perform anti_join
  duplicated_rows <- duplicated_dataset %>%
    st_drop_geometry()

  cleaned_dataset <- dplyr::anti_join(dataset, duplicated_rows, by = c(axis_field, measure_field))

  cat(nrow(cleaned_dataset), "rows kept,", nrow(duplicated_rows), "removed rows", "\n")
  return(cleaned_dataset)
}
