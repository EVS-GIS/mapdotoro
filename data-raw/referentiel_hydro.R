## code to prepare `referentiel_hydro` dataset goes here

referentiel_hydro <- sf::st_read(dsn = file.path("data-raw", "raw-datasets", "REFERENTIEL_HYDRO.shp")) %>%
  dplyr::filter(AXIS == 2000796122)

usethis::use_data(referentiel_hydro, overwrite = TRUE)
checkhelper::use_data_doc(name = "referentiel_hydro")
attachment::att_amend_desc()
