## code to prepare `elevation_profiles` dataset goes here

elevation_profiles <- readr::read_csv(file.path("data-raw", "raw-datasets", "SWATH_ELEVATION_PROFILES.csv")) %>%
  dplyr::filter(axis == 2000796122) %>%
  dplyr::filter(quantile== 50) %>%
  dplyr::filter(measure == 103700 | measure == 5100) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(quantile = dplyr::first(quantile),
                   density = sum(density),
                   mean = sum(mean),
                   profile = dplyr::first(profile),
                   axis = dplyr::first(axis),
                   measure = dplyr::first(measure),
                   distance = dplyr::first(distance)) %>%
  dplyr::filter(density > 0)

usethis::use_data(elevation_profiles, overwrite = TRUE)
checkhelper::use_data_doc(name = "elevation_profiles")
attachment::att_amend_desc()
