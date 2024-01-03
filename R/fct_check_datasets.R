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
  # check for duplicate in M field for each axis
  for (axis in unique(dataset[[axis_field]])){
    net_axe <- dataset %>%
      dplyr::filter(.data[[axis_field]]==axis & !is.na(.data[[measure_field]])) # !is.na to not set NA as duplicate in the axis
    if (any(duplicated(net_axe[[measure_field]])) ==TRUE){ # if duplicate identified
      duplicate <- glue::glue("L'axe {axis} a des doublons")
      message(duplicate)
      # two duplicates indices set to return all the rows involved in duplicate and not return only the first/last one.
      duplicated_rows <- rbind(duplicated_rows, net_axe[duplicated(net_axe[[measure_field]]) |
                                                          duplicated(net_axe[[measure_field]], fromLast = TRUE), ])
      # for summary we need only to have the number of duplicate measure for each axes, no need to count all the rows but just the first one.
      duplicated_summary <- rbind(duplicated_summary, data.frame(
        axis = axis,
        num_duplicate = count(net_axe[duplicated(net_axe[[measure_field]]), ])$n
      ))
    }
  }

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
  return(cleaned_dataset)
}


