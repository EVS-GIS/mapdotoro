## code to prepare `roe` dataset goes here

roe <- sf::st_read(dsn = file.path("data-raw", "raw-datasets", "roe.gpkg")) %>%
  dplyr::slice(1:10)


usethis::use_data(roe, overwrite = TRUE)
checkhelper::use_data_doc(name = "roe")
attachment::att_amend_desc()
