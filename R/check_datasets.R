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
#' @return sf data.frame with all duplicated rows.
#' @export
#'
#' @examples
#' check_duplicate(swaths)
check_duplicate <- function(dataset,
                            axis_field = "AXIS",
                            measure_field = "M"){

  duplicated_rows <- dataset[0, ]
  # check for duplicate in M field for each axis
  for (axis in unique(dataset[[axis_field]])){
    net_axe <- dataset %>%
      dplyr::filter(.data[[axis_field]]==axis)
    if (any(duplicated(net_axe[[measure_field]]))==TRUE){
      duplicate <- glue::glue("L'axe {axis} a des doublons")
      message(duplicate)
      duplicated_rows <- rbind(duplicated_rows, net_axe[duplicated(net_axe[[measure_field]]) |
                                                          duplicated(net_axe[[measure_field]], fromLast = TRUE), ])
    }
  }

  return(duplicated_rows)
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


