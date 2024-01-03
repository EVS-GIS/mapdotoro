
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mapdotoro

<!-- badges: start -->
<!-- badges: end -->

Mapdotoro aim to prepared the Fluvial Corridor Toolbox data outputs to
the shiny application mapdoapp.

## Installation

You can install the development version of mapdotoro from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("EVS-GIS/mapdotoro")
```

## Workflow

Libraries

``` r
library(dplyr)
library(sf)
library(tmap)
library(mapdotoro)
library(tidyr)
library(lwgeom)
library(qgisprocess)
library(DBI)
```

Testing data

``` r
bassin_hydrographique <- bassin_hydrographique %>% 
  rename_with(tolower)
region_hydrographique <- region_hydrographique %>% 
  rename_with(tolower)
roe <- roe %>% 
  rename_with(tolower)
referentiel_hydro <- referentiel_hydro
swaths <- swaths
talweg_metrics <- talweg_metrics
landcover <- landcover
continuity <- continuity
```

Datasets from the Fluvial Corridor Toolbox

``` r
bassin_hydrographique <- st_read(dsn = file.path("data-raw", "raw-datasets",
                                                 "bassin_hydrographique.gpkg"), 
                                 layer = "bassin_hydrographique") %>% 
  rename_with(tolower)
region_hydrographique <- st_read(dsn = file.path("data-raw", "raw-datasets",
                                                 "region_hydrographique.gpkg"), 
                                 layer = "region_hydrographique") %>% 
  rename_with(tolower)
roe <- st_read(dsn = file.path("data-raw", "raw-datasets", "roe.gpkg"), layer = "roe") %>% 
  rename_with(tolower)
referentiel_hydro <- st_read(dsn = file.path("data-raw", "raw-datasets", "REFERENTIEL_HYDRO.shp"))
swaths <- st_read(dsn = file.path("data-raw", "raw-datasets", "SWATHS_MEDIALAXIS.shp"))
talweg_metrics <- readr::read_csv(file.path("data-raw", "raw-datasets", "TALWEG_METRICS.csv"))
landcover <- readr::read_csv(file.path("data-raw", "raw-datasets", "WIDTH_LANDCOVER.csv"))
continuity <- readr::read_csv(file.path("data-raw", "raw-datasets", "WIDTH_CONTINUITY.csv"))
```

Check swaths

``` r
swaths_valid <- swaths %>% 
  dplyr::filter(VALUE == 2) # keep only valid swaths

swaths_duplicated <- check_duplicate(swaths_valid)

# check number of duplicate
print(swaths_duplicated$duplicated_summary)
```

Display swaths and duplicated

``` r
# map duplicate
tmap::tmap_mode("view")
map <- tmap::tm_shape(bassin_hydrographique) +
  tmap::tm_polygons()+
tmap::tm_shape(region_hydrographique) +
  tmap::tm_polygons()+
tmap::tm_shape(swaths) +
  tmap::tm_polygons()+
tmap::tm_shape(swaths_duplicated$duplicated_rows) +
  tmap::tm_polygons(col = "red")+
tmap::tm_shape(referentiel_hydro) +
  tmap::tm_lines(col= "blue")
map
```

clean swaths

``` r
swaths_cleaned <- clean_duplicated(dataset = swaths_valid,
                                   duplicated_dataset = swaths_duplicated$duplicated_rows)
```

clip referentiel hydro by swaths

``` r
hydro_swaths <- st_sf(st_sfc(crs = 2154)) # create an empty sf dataset
# clip by axis
for (axis in unique(swaths_cleaned$AXIS)){
  swaths_axe <- swaths_cleaned %>%
    filter(AXIS == axis) # get swaths by axis
  hydro_axe <- referentiel_hydro %>%
    filter(AXIS == axis) %>% # get streams by axis
    st_zm () %>% 
    st_combine() %>% 
    st_sf() %>% 
    st_line_merge()
  hydro_swaths_axis <- hydro_axe %>% 
    st_split(swaths_axe) %>%
    st_collection_extract("LINESTRING") %>%
    st_join(swaths_axe, join = st_within) %>% 
    mutate(AXIS = ifelse(is.na(AXIS), axis, AXIS))
  hydro_swaths <- rbind(hydro_swaths, hydro_swaths_axis) # fill the output dataset by axis
}
```

clean hydro_swaths

``` r
hydro_swaths <- hydro_swaths %>%
  mutate(id = row_number()) %>% # create an unique id
  mutate(LENG = st_length(.)) # add length in swath
  # mutate(M = ifelse(LENG < units::set_units(10, m), NA, M)) # length is unit object

hydro_swaths_duplicated <- check_duplicate(hydro_swaths)
hydro_swaths_duplicated$duplicated_rows
hydro_swaths_duplicated$duplicated_summary

hydro_swaths_prepared_clean <- st_sf(st_sfc(crs = 2154)) # create an empty sf dataset
for (measure in unique(hydro_swaths_duplicated$duplicated_rows$M)){
  duplicate_to_fix <- hydro_swaths_duplicated$duplicated_rows %>% 
    filter(M == measure) %>% 
    filter(LENG < max(LENG)) %>% # we take the shortest lines
    mutate(M = -1) # set the shortest line to -1
  hydro_swaths_prepared_clean <- rbind(hydro_swaths_prepared_clean, duplicate_to_fix)
}

hydro_swaths_prepared_clean <- hydro_swaths_prepared_clean %>%
  st_drop_geometry() %>% 
  select (id, M)

hydro_swaths_cleaned <- merge(hydro_swaths, hydro_swaths_prepared_clean, by = "id", all.x = TRUE) %>% 
  mutate(M.x = ifelse(!is.na(M.y) & M.y == -1, NA, M.x)) %>% # the lines set to -1 are NA in hydro_swaths
  select(-M.y) %>% 
  rename("M" = M.x) %>% 
  select(AXIS, M, DRAINAGE, geometry)
```

Measure network from outlet

``` r
identifynetworknodes <- hydro_swaths_cleaned %>% 
  qgis_run_algorithm_p("fct:identifynetworknodes",
                       QUANTIZATION = 100000000)

hydro_swaths_identified <- st_read(identifynetworknodes$OUTPUT) %>% 
  filter(NODEA != NODEB) # remove row when NODEA = NODEB (when distance is too short the node id is the same)

measurenetworkfromoutlet <- hydro_swaths_identified %>% 
  qgis_run_algorithm_p("fct:measurenetworkfromoutlet",
                       FROM_NODE_FIELD = "NODEA",
                       TO_NODE_FIELD = "NODEB")

hydro_swaths_measured <- st_read(measurenetworkfromoutlet$OUTPUT) %>% 
  st_zm() %>% 
  select(-NODEA, -NODEB) %>% 
  rename_with(tolower) %>% 
  rename(measure_medial_axis = m,
         measure_from_outlet = measure)
```

prepare talweg metric

``` r
talweg_metrics_prepared <- talweg_metrics %>%
  rename("measure_medial_axis" = "measure")
```

Prepare landcover and continuity area

``` r
landcover_prepared <- prepare_landcover_continuity_area(landcover)

continuity_prepared <- prepare_landcover_continuity_area(continuity)
```

Database connection

``` r
db_con <- DBI::dbConnect(RPostgres::Postgres(),
                        host = Sys.getenv("DBMAPDO_HOST_TEST"),
                        port = Sys.getenv("DBMAPDO_PORT_TEST"),
                        dbname = Sys.getenv("DBMAPDO_NAME_TEST"),
                        user      = Sys.getenv("DBMAPDO_USER_TEST"),
                        password  = Sys.getenv("DBMAPDO_PASS_TEST"))
```

Export tables

``` r
# export bassin_hydrographique


pg_export_bassin_hydrographique(dataset = bassin_hydrographique,
                                table_name = "bassin_hydrographique",
                                drop_existing_table = TRUE,
                                db_con)
set_displayed_bassin_region(table_name = "bassin_hydrographique",
                     displayed_gid = c(6))

pg_export_region_hydrographique(dataset = region_hydrographique,
                                table_name = "region_hydrographique",
                                drop_existing_table = TRUE,
                                db_con)
set_displayed_bassin_region(table_name = "bassin_hydrographique",
                     displayed_gid = c(11, 16, 31, 33))
```
