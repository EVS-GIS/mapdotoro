## code to prepare `referentiel_hydro` dataset goes here

referentiel_hydro <- sf::st_read(dsn = file.path("data-raw", "REFERENTIEL_HYDRO.shp")) %>%
  dplyr::filter(TOPONYME == "le Drac")

usethis::use_data(referentiel_hydro, overwrite = TRUE)
checkhelper::use_data_doc(name = "referentiel_hydro")
attachment::att_amend_desc()
