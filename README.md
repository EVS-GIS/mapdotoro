
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

### Libraries

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

### Testing data

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

### Datasets from the Fluvial Corridor Toolbox

``` r
input_bassin_hydrographique <- st_read(dsn = file.path("data-raw", "raw-datasets",
                                                 "bassin_hydrographique.gpkg"))

input_region_hydrographique <- st_read(dsn = file.path("data-raw", "raw-datasets",
                                                 "region_hydrographique.gpkg"))

input_roe <- st_read(dsn = file.path("data-raw", "raw-datasets", "roe.gpkg"))

referentiel_hydro <- st_read(dsn = file.path("data-raw", "raw-datasets", "REFERENTIEL_HYDRO.shp"))
swaths <- st_read(dsn = file.path("data-raw", "raw-datasets", "SWATHS_MEDIALAXIS.shp"))
talweg_metrics <- readr::read_csv(file.path("data-raw", "raw-datasets", "TALWEG_METRICS.csv"))
landcover <- readr::read_csv(file.path("data-raw", "raw-datasets", "WIDTH_LANDCOVER.csv"))
continuity <- readr::read_csv(file.path("data-raw", "raw-datasets", "WIDTH_CONTINUITY.csv"))
```

### Prepare bassin, region, ROE

``` r
bassin_hydrographique <- prepare_bassin_hydrographique(input_bassin_hydrographique)

region_hydrographique <- prepare_region_hydrographique(input_region_hydrographique)

roe <- prepare_roe(input_roe,
                   region_hydro = region_hydrographique)
```

### Database connection

``` r
db_con <- DBI::dbConnect(RPostgres::Postgres(),
                        host = Sys.getenv("DBMAPDO_HOST_TEST"),
                        port = Sys.getenv("DBMAPDO_PORT_TEST"),
                        dbname = Sys.getenv("DBMAPDO_NAME_TEST"),
                        user      = Sys.getenv("DBMAPDO_USER_TEST"),
                        password  = Sys.getenv("DBMAPDO_PASS_TEST"))
```

### Set database structure

``` r
create_table_bassin_hydrographique(table_name = "bassin_hydrographique",
                                    db_con = db_con)
create_table_region_hydrographique(table_name = "region_hydrographique",
                                   db_con)
create_table_roe(table_name = "roe",
                 db_con = db_con)
```

### Update and insert database

``` r
upsert_bassin_hydrographique(dataset = bassin_hydrographique,
                             table_name = "bassin_hydrographique",
                             db_con = db_con,
                             field_identifier = "cdbh")
set_displayed_bassin_region(table_name = "bassin_hydrographique",
                            display_codes_bassin_or_region = c("06"),
                            field_identifier = "cdbh",
                            db_con = db_con)

upsert_region_hydrographique(dataset = region_hydrographique,
                             table_name = "region_hydrographique",
                             db_con = db_con,
                             field_identifier = "cdregionhy")
set_displayed_bassin_region(table_name = "region_hydrographique",
                            display_codes_bassin_or_region = c("W"),
                            field_identifier = "cdregionhy",
                            db_con = db_con)

upsert_roe(dataset = roe,
           table_name = "roe", 
           db_con = db_con, 
           field_identifier = "cdobstecou")
```

### Check swaths

``` r
swaths_valid <- swaths %>% 
  dplyr::filter(VALUE == 2) # keep only valid swaths

swaths_duplicated <- check_duplicate(swaths_valid)

# check number of duplicate
print(swaths_duplicated$duplicated_summary)
```

### Display swaths and duplicated

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

### Clean swaths

``` r
swaths_cleaned <- clean_duplicated(dataset = swaths_valid,
                                   duplicated_dataset = swaths_duplicated$duplicated_rows)
```

### Clip referentiel hydro by swaths

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
  hydro_swaths_axis <- hydro_axe %>% # split referentiel hydro
    st_split(swaths_axe) %>%
    st_collection_extract("LINESTRING") %>%
    st_join(swaths_axe, join = st_within) %>% 
    mutate(AXIS = ifelse(is.na(AXIS), axis, AXIS))
  hydro_swaths <- rbind(hydro_swaths, hydro_swaths_axis) # fill the output dataset by axis
}
```

### Clean hydro_swaths

``` r
hydro_swaths <- hydro_swaths %>%
  mutate(id = row_number()) %>% # create an unique id
  mutate(LENG = st_length(.)) # add length in swath

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

### hydro_swaths measure network from outlet and strahler

``` r
# prepare network for measurenetworkfromoutlet
identifynetworknodes <- hydro_swaths_cleaned %>% 
  qgis_run_algorithm_p("fct:identifynetworknodes",
                       QUANTIZATION = 100000000)

# remove row when NODEA = NODEB (when distance is too short the node id is the same)
hydro_swaths_identified <- st_read(identifynetworknodes$OUTPUT) %>% 
  filter(NODEA != NODEB) 

# create measure network from outlet field, measure = 0 from each axis beginning
measurenetworkfromoutlet <- hydro_swaths_identified %>% 
  qgis_run_algorithm_p("fct:measurenetworkfromoutlet",
                       FROM_NODE_FIELD = "NODEA",
                       TO_NODE_FIELD = "NODEB")

# need interger fid and not numeric to perform QGIS spatial join
referentiel_hydro$fid <- as.integer(referentiel_hydro$fid)

# create small buffer before spatial join
referentiel_hydro_buff <- referentiel_hydro %>% 
  st_zm() %>% 
  st_buffer(dist = 1)

# spatial join with QGIS working better than sf with biggest overlap
hydro_swaths_strahler <- st_read(measurenetworkfromoutlet$OUTPUT) %>% 
  qgis_run_algorithm_p("native:joinattributesbylocation",
                       DISCARD_NONMATCHING = FALSE,
                       JOIN = referentiel_hydro_buff,
                       JOIN_FIELDS = "STRAHLER",
                       METHOD = 2, # attribute with biggest overlap
                       PREDICATE = 0, # intersect
                       PREFIX = "")

# format attributes
hydro_swaths_prepared <- st_read(hydro_swaths_strahler$OUTPUT) %>% 
  st_zm() %>% 
  select(-NODEA, -NODEB) %>% 
  rename_all(clean_column_names) %>% 
  rename(measure_medial_axis = m,
         measure_from_outlet = measure) %>% 
  st_transform(crs = 4326) %>%
    st_join(region_hydro, join = st_within) %>%
    mutate(gid_region = gid) %>%
    select(-colnames(region_hydro)[colnames(region_hydro) != "geom"])



# st_write(obj = hydro_swaths_prepared, dsn = "./data-raw/temp/hydro_swaths_prepared.gpkg", layer = "hydro_swaths_prepared", delete_dsn = TRUE)
```

### Prepare hydro_axis

``` r
# prepare referentiel hydro to join with axis sf data.frame and get TOPONYME
referentiel_hydro_no_geom <- referentiel_hydro %>%
  st_drop_geometry() %>%
  group_by(AXIS) %>% 
  select(AXIS, TOPONYME)

# hydro_axis preparation
hydro_axis <- hydro_swaths_measured %>%
  group_by(axis) %>%
  summarise(length = sum(length),
            geom = st_union(geom)) %>% # union geom and recalculate length
  left_join(referentiel_hydro_no_geom, by = c("axis" = "AXIS"), multiple = "first") %>% # add TOPONYME field
  rename_with(tolower)
```

### Prepare talweg metric

``` r
talweg_metrics_duplicated <- check_duplicate(talweg_metrics, axis_field = "axis",measure_field = "measure")

talweg_metrics_prepared <- clean_duplicated(dataset = talweg_metrics, duplicated_dataset = talweg_metrics_duplicated$duplicated_rows, axis_field = "axis", measure_field = "measure") %>% 
  rename("measure_medial_axis" = "measure")
```

### Prepare landcover area

``` r
landcover_area_prepared <- prepare_landcover_continuity_area(landcover)

# check for duplicate (should not print red L'axe axe_number a des doublons !)
landcover_area_prepared_left <- landcover_area_prepared %>% 
  filter(side == "left")

landcover_area_duplicated_left <- check_duplicate(dataset = landcover_area_prepared_left, axis_field = "axis", measure_field = "measure_medial_axis")

landcover_area_prepared_right <- landcover_area_prepared %>% 
  filter(side == "right")

landcover_area_duplicated_right <- check_duplicate(dataset = landcover_area_prepared_right, axis_field = "axis", measure_field = "measure_medial_axis")
```

### Prepare continuity area

``` r
continuity_area_prepared <- prepare_landcover_continuity_area(continuity)

# check for duplicate (should not print red L'axe axe_number a des doublons !)
continuity_area_prepared_left <- continuity_area_prepared %>% 
  filter(side == "left")

continuity_area_duplicated_left <- check_duplicate(dataset = continuity_area_prepared_left, axis_field = "axis", measure_field = "measure_medial_axis")

continuity_area_prepared_right <- continuity_area_prepared %>% 
  filter(side == "right")

continuity_area_duplicated_right <- check_duplicate(dataset = continuity_area_prepared_right, axis_field = "axis", measure_field = "measure_medial_axis")
```

### Database connection

``` r
db_con <- DBI::dbConnect(RPostgres::Postgres(),
                        host = Sys.getenv("DBMAPDO_HOST_TEST"),
                        port = Sys.getenv("DBMAPDO_PORT_TEST"),
                        dbname = Sys.getenv("DBMAPDO_NAME_TEST"),
                        user      = Sys.getenv("DBMAPDO_USER_TEST"),
                        password  = Sys.getenv("DBMAPDO_PASS_TEST"))
```

### Create database structure

``` r
create_table_bassin_hydrographique(table_name = "bassin_hydrographique")
create_table_hydro_swaths(table_name = "hydro_swaths")
```

### update example

``` r
hydro_swaths_prepared_exemple <- hydro_swaths_prepared %>% 
  slice_head(n=3)
hydro_swaths_prepared_exemple[2, "length"] <- 999
st_write(hydro_swaths_prepared_exemple, db_con, "hydro_swaths", driver = "PostgreSQL", append = TRUE)



hydro_swaths_prepared_exemple2 <- hydro_swaths_prepared %>% 
  slice_head(n=4)

# Create fake data
fake_data <- data.frame(
  axis = c(2000788927, 2000788928, 2000788929, 2000788930, 2000788931),
  measure_medial_axis = c(20000, 20500, 19800, 21000, 21500),
  drainage = c(8.5, 8.2, 9.0, 8.8, 9.5),
  measure_from_outlet = c(19500.12, 19800.45, 19350.78, 20500.32, 21000.21),
  length = c(180.5, 195.2, 175.8, 200.3, 215.7),
  strahler = c(2, 1, 2, 3, 1)
)

# Create fake geometries
fake_geom <- st_sfc(
  st_linestring(matrix(c(990000, 6523000, 990005, 6523010, 990010, 6523020, 990015, 6523030), ncol = 2)),
  st_linestring(matrix(c(990020, 6523040, 990025, 6523050, 990030, 6523060, 990035, 6523070), ncol = 2)),
  st_linestring(matrix(c(990040, 6523080, 990045, 6523090, 990050, 6523100, 990055, 6523110), ncol = 2)),
  st_linestring(matrix(c(990060, 6523120, 990065, 6523130, 990070, 6523140, 990075, 6523150), ncol = 2)),
  st_linestring(matrix(c(990080, 6523160, 990085, 6523170, 990090, 6523180, 990095, 6523190), ncol = 2))
)

# Create sf data frame
fake_sf <- st_sf(
  axis = fake_data$axis,
  measure_medial_axis = fake_data$measure_medial_axis,
  drainage = fake_data$drainage,
  measure_from_outlet = fake_data$measure_from_outlet,
  length = fake_data$length,
  strahler = fake_data$strahler,
  geom = fake_geom
) %>% 
  st_set_crs(2154)

hydro_swaths_prepared_exemple3 <- rbind(hydro_swaths_prepared_exemple2, fake_sf)

# hydro_swaths_table <- st_read(dsn = db_con, layer = "hydro_swaths") %>% 
#   st_drop_geometry()
# 
# common_rows <- hydro_swaths_prepared_exemple3[hydro_swaths_prepared_exemple3$axis %in% hydro_swaths_table$axis, ]

axis_list <- paste0("(", toString(unique(hydro_swaths_prepared_exemple3$axis)), ")")

query <- glue::glue("DELETE FROM hydro_swaths WHERE axis IN {axis_list};")
DBI::dbExecute(db_con, query)

st_write(obj = hydro_swaths_prepared_exemple3, dsn = db_con, layer = "hydro_swaths", append = TRUE)

upsert_hydro_swaths(dataset = hydro_swaths_prepared_exemple3, table_name = "hydro_swaths", db_con = db_con)
```

### Update existing values

``` r
pg_export_hydro_swaths(dataset = hydro_swaths_measured,
                       table_name = "hydro_swaths",
                       drop_existing_table = TRUE,
                       db_con = db_con,
                       region_hydrographique_file_path = file.path("data-raw",
                                                                   "raw-datasets",
                                                                   "region_hydrographique.gpkg"))
```

### Export tables

``` r
# export bassin_hydrographique


pg_export_bassin_hydrographique(dataset = bassin_hydrographique,
                                table_name = "bassin_hydrographique",
                                drop_existing_table = TRUE,
                                db_con = db_con)

set_displayed_bassin_region(table_name = "bassin_hydrographique",
                     displayed_gid = c(6))

pg_export_region_hydrographique(dataset = region_hydrographique,
                                table_name = "region_hydrographique",
                                drop_existing_table = TRUE,
                                db_con = db_con)
set_displayed_bassin_region(table_name = "bassin_hydrographique",
                     displayed_gid = c(11, 16, 31, 33))

pg_export_roe(dataset = roe,
              table_name = "roe",
              drop_existing_table = TRUE,
              db_con = db_con,
              region_hydrographique_file_path = file.path("data-raw",
                                                          "raw-datasets",
                                                          "region_hydrographique.gpkg"))

pg_export_hubeau(url = "https://hubeau.eaufrance.fr/api/v1/ecoulement/stations?format=json",
                 table_name = "hydro_stations",
                 drop_existing_table = TRUE,
                 db_con = db_con,
                 region_hydrographique_file_path = file.path("data-raw",
                                                             "raw-datasets",
                                                             "region_hydrographique.gpkg"))

pg_export_talweg_metrics(dataset = talweg_metrics_prepared,
                     table_name = "talweg_metrics",
                     drop_existing_table = TRUE,
                     db_con = db_con)

pg_export_hydro_axis(dataset = hydro_axis,
                     table_name = "hydro_axis",
                     drop_existing_table = TRUE,
                     db_con = db_con,
                     region_hydrographique_file_path = file.path("data-raw",
                                                                 "raw-datasets",
                                                                 "region_hydrographique.gpkg"))

pg_export_hydro_swaths(dataset = hydro_swaths_measured,
                       table_name = "hydro_swaths",
                       drop_existing_table = TRUE,
                       db_con = db_con,
                       region_hydrographique_file_path = file.path("data-raw",
                                                                   "raw-datasets",
                                                                   "region_hydrographique.gpkg"))

pg_export_landcover_area(dataset = landcover_area_prepared,
                     table_name = "landcover_area",
                     drop_existing_table = TRUE,
                     db_con = db_con)

pg_export_continuity_area(dataset = continuity_area_prepared,
                     table_name = "continuity_area",
                     drop_existing_table = TRUE,
                     db_con = db_con)
```

### Create view for mapdoapp

``` r
network_metrics_view(db_con = db_con, view_name = "network_metrics")

landcover_full_side_view(db_con = db_con, view_name = "landcover_area_full_side")

continuity_full_side_view(db_con = db_con, view_name = "continuity_area_full_side")
```
