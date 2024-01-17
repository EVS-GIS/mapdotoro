## code to prepare `valley_bottom` dataset goes here

valley_bottom <- readr::read_csv(file.path("data-raw", "raw-datasets", "WIDTH_VALLEY_BOTTOM.csv")) %>%
  dplyr::filter(axis == 2000796122)

usethis::use_data(valley_bottom, overwrite = TRUE)
checkhelper::use_data_doc(name = "valley_bottom")
attachment::att_amend_desc()
