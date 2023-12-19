## code to prepare `talweg_metrics` dataset goes here

talweg_metrics <- readr::read_csv(file.path("data-raw", "raw-datasets", "TALWEG_METRICS.csv")) %>%
  dplyr::filter(axis == 2000796122)

usethis::use_data(talweg_metrics, overwrite = TRUE)
checkhelper::use_data_doc(name = "talweg_metrics")
attachment::att_amend_desc()
