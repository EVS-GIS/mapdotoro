## code to prepare `hydro_sites` dataset goes here

hydro_sites <- import_hydro_sites() %>%
  dplyr::slice(1:10)

usethis::use_data(hydro_sites, overwrite = TRUE)
checkhelper::use_data_doc(name = "hydro_sites")
attachment::att_amend_desc()
