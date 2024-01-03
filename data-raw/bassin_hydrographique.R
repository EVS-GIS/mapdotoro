## code to prepare `bassin_hydrographique` dataset goes here

bassin_hydrographique <- sf::st_read(dsn = file.path("data-raw", "raw-datasets", "bassin_hydrographique.gpkg"), layer = "bassin_hydrographique") %>%
  dplyr::filter(LbBH %in% "Rhône-Méditerranée")

usethis::use_data(bassin_hydrographique, overwrite = TRUE)
checkhelper::use_data_doc(name = "bassin_hydrographique")
attachment::att_amend_desc()

