#' Prepare landcover or continuity dataset to have area for each label
#'
#' @param dataset A landuse or continuity data.frame.
#'
#' @importFrom dplyr mutate select rename_with rename sym
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_replace_all
#' @importFrom tidyselect everything
#'
#' @return data.frame.
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

  # standard column names
  landcover_prepared <- landcover_prepared %>%
    rename_with(~str_replace_all(., " ", "_"), everything()) %>%
    rename_with(tolower) %>%
    rename(measure_medial_axis = measure)

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
