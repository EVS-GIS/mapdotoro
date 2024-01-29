## code to prepare `roe` dataset goes here

region_isere <- sf::st_read(dsn = file.path("data-raw", "raw-datasets", "region_hydrographique.gpkg")) %>%
  dplyr::filter(LbRegionHy %in% "L'Isère")
roe <- sf::st_read(dsn = file.path("data-raw", "raw-datasets", "roe.gpkg")) %>%
  sf::st_intersection(region_isere) %>%
  select(-gid.1) %>%
  select(-colnames(region_isere)[colnames(region_isere) != "geom"]) # remove region_isere columns

usethis::use_data(roe, overwrite = TRUE)
checkhelper::use_data_doc(name = "roe")
attachment::att_amend_desc()
