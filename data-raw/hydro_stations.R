## code to prepare `hydro_stations` dataset goes here

hydro_stations <- import_hydro_stations(url = "https://hubeau.eaufrance.fr/api/v1/ecoulement/stations?format=json") %>%
  dplyr::slice(1:10)

usethis::use_data(hydro_stations, overwrite = TRUE)
checkhelper::use_data_doc(name = "hydro_stations")
attachment::att_amend_desc()
