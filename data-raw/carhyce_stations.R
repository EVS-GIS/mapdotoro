## code to prepare `carhyce_stations` dataset goes here
library(tidyverse)
read_export_carhyce=function(file){
  read_delim(file, 
             delim = ";", escape_double = FALSE, locale = locale(date_names = "fr", 
                                                                 decimal_mark = ","),
             trim_ws = TRUE) %>% 
    janitor::clean_names() %>%
    mutate(name_station=localisation_station_de_mesure) %>% 
    dplyr::select(code_station,name_station, x, y) %>% 
    mutate(code_station=as.character(code_station)) %>% 
    group_by(code_station) %>% 
    summarise(name_station=name_station[1],
              x=x[1],
              y=y[1],
              .groups="drop")  # errors: same station, various geometries
}

# Keep only station in metropolitan France
carhyce_stations_toutes=read_export_carhyce("data-raw/raw-datasets/Operations_2026-04-01.csv")
carhyce_stations_domtom=tibble::tibble(file=paste0("data-raw/raw-datasets/Operations_2026-04-03_",c(971:974,976),".csv")) %>%
  mutate(data=purrr::map(file,read_export_carhyce)) %>% 
  tidyr::unnest(data)

# Define CRS and change to WGS84
carhyce_stations=carhyce_stations_toutes %>% 
  filter(!(code_station %in% carhyce_stations_domtom$code_station)) %>% 
  sf::st_as_sf(coords=c("x","y")) %>%
  sf::st_set_crs("EPSG:2154") %>%
  sf::st_transform(4326) %>% 
  filter(!(code_station %in% c("03149900","02051170")))


db_con = db_con()
sf::st_write(obj = carhyce_stations,
         dsn = db_con,
         layer = "carhyce_stations",
         append = TRUE,
         layer_options = "GEOMETRY_NAME=geom")

reader <- Sys.getenv("DBMAPDO_DEV_READER")
query <- glue::glue("
    GRANT SELECT ON carhyce_stations
    TO {reader};")
DBI::dbExecute(db_con, query)

# library(leaflet)
# leaflet(carhyce_stations) %>%
#   addTiles() %>% 
#   addMarkers(popup=~code_station)

usethis::use_data(carhyce_stations, overwrite = TRUE)
checkhelper::use_data_doc(name = "carhyce_stations")
attachment::att_amend_desc()