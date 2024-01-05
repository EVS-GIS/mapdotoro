## code to prepare `region_hydrographique` dataset goes here

region_hydrographique <- sf::st_read(dsn = file.path("data-raw", "raw-datasets", "region_hydrographique.gpkg")) %>%
  dplyr::filter(LbRegionHy %in% "L'Isère")

usethis::use_data(region_hydrographique, overwrite = TRUE)
checkhelper::use_data_doc(name = "region_hydrographique")
attachment::att_amend_desc()
