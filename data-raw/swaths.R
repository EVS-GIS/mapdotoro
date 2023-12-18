## code to prepare `swaths` dataset goes here

swaths <- sf::st_read(dsn = file.path("data-raw", "raw-datasets", "SWATHS_MEDIALAXIS.shp")) %>%
  dplyr::filter(AXIS == "2000796122")

usethis::use_data(swaths, overwrite = TRUE)
checkhelper::use_data_doc(name = "swaths")
attachment::att_amend_desc()
