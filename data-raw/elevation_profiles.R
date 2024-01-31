## code to prepare `elevation_profiles` dataset goes here

elevation_profiles <- readr::read_csv(file.path("data-raw", "raw-datasets", "SWATH_ELEVATION_PROFILES.csv")) %>%
  dplyr::filter(axis == 2000796122) %>%
  dplyr::filter(measure == 103700 | measure == 5100)

usethis::use_data(elevation_profiles, overwrite = TRUE)
checkhelper::use_data_doc(name = "elevation_profiles")
attachment::att_amend_desc()
