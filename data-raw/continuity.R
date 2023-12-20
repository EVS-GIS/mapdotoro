## code to prepare `continuity` dataset goes here

continuity <- readr::read_csv(file.path("data-raw", "raw-datasets", "WIDTH_CONTINUITY.csv")) %>%
  dplyr::filter(axis == 2000796122)

usethis::use_data(continuity, overwrite = TRUE)
checkhelper::use_data_doc(name = "continuity")
attachment::att_amend_desc()
