## code to prepare `troncon_bdtopo_id` dataset goes here

troncon_bdtopo_id <- readr::read_csv(file.path("data-raw", "raw-datasets", "troncon_bdtopo_id.csv")) %>%
  dplyr::filter(axis == 2000796122)

usethis::use_data(troncon_bdtopo_id, overwrite = TRUE)
checkhelper::use_data_doc(name = "troncon_bdtopo_id")
attachment::att_amend_desc()
