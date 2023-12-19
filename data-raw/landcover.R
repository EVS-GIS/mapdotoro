## code to prepare `landcover` dataset goes here

landcover <- readr::read_csv(file.path("data-raw", "raw-datasets", "WIDTH_LANDCOVER.csv")) %>%
  dplyr::filter(axis == 2000796122)

usethis::use_data(landcover, overwrite = TRUE)
checkhelper::use_data_doc(name = "landcover")
attachment::att_amend_desc()
