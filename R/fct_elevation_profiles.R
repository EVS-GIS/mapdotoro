#' Prepare elevation profiles to database export.
#'
#' @param dataset data.frame elevation profiles imported.
#'
#' @importFrom dplyr filter group_by summarise first
#'
#' @return data.frame elevation profiles prepared.
#' @export
prepare_elevation_profiles <- function(dataset = input_elevation_profiles){

  profiles <- dataset %>%
    # get only median profile
    filter(quantile== 50) %>%
    # left right side not need
    group_by(sample) %>%
    summarise(quantile = first(quantile),
                     density = sum(density),
                     mean = sum(mean),
                     profile = first(profile),
                     axis = first(axis),
                     measure = first(measure),
                     distance = first(distance)) %>%
    # remove row when no sample exist, generally at the profile edges (profile = 0 and not NA)
    filter(density > 0)

  return(profiles)
}


