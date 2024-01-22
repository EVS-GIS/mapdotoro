
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

mapdotoro use qgisprocess package, [QGIS](https://www.qgis.org/en/site/)
with the [Fluvial Corridor Toolbox](https://github.com/EVS-GIS/fct-qgis)
plugin installed.

Tested with QGIS version 3.28.13 and Fluvial Corridor Toolbox version
1.0.11.

## Workflow

### Libraries and QGIS configuration

``` r
library(dplyr)
library(sf)
library(tmap)
library(mapdotoro)
library(tidyr)
library(lwgeom)
library(qgisprocess)
library(DBI)

qgis_configure()
```

### Testing data

``` r
input_bassin_hydrographique <- bassin_hydrographique
input_region_hydrographique <- region_hydrographique
input_roe <- roe
input_hydro_stations <- hydro_stations
input_referentiel_hydro <- referentiel_hydro
input_swaths <- swaths
input_talweg_metrics <- talweg_metrics
input_landcover <- landcover
input_continuity <- continuity
input_valley_bottom <- valley_bottom
```

### Datasets from the Fluvial Corridor Toolbox

``` r
input_bassin_hydrographique <- sf::st_read(dsn = file.path("data-raw", "raw-datasets",
                                                 "bassin_hydrographique.gpkg"))

input_region_hydrographique <- sf::st_read(dsn = file.path("data-raw", "raw-datasets",
                                                 "region_hydrographique.gpkg"))

input_roe <- sf::st_read(dsn = file.path("data-raw", "raw-datasets", "roe.gpkg"))

input_hydro_stations <- import_hydro_stations(url = "https://hubeau.eaufrance.fr/api/v1/ecoulement/stations?format=json")

input_talweg_metrics <- readr::read_csv(file.path("data-raw", "raw-datasets", "TALWEG_METRICS.csv"))

input_referentiel_hydro <- sf::st_read(dsn = file.path("data-raw", "raw-datasets", "REFERENTIEL_HYDRO.shp"))
input_swaths <- sf::st_read(dsn = file.path("data-raw", "raw-datasets", "SWATHS_MEDIALAXIS.shp"))

input_landcover <- readr::read_csv(file.path("data-raw", "raw-datasets", "WIDTH_LANDCOVER.csv"))
input_continuity <- readr::read_csv(file.path("data-raw", "raw-datasets", "WIDTH_CONTINUITY.csv"))
input_valley_bottom <- readr::read_csv(file.path("data-raw", "raw-datasets", "WIDTH_VALLEY_BOTTOM.csv"))
```

### Prepare dataset

``` r
bassin_hydrographique <- prepare_bassin_hydrographique(input_bassin_hydrographique)

region_hydrographique <- prepare_region_hydrographique(input_region_hydrographique)

roe <- prepare_roe(input_roe,
                   region_hydro = region_hydrographique)

hydro_stations <- prepare_hydro_stations(dataset = input_hydro_stations,
                                         region_hydro = region_hydrographique)

talweg_metrics <- prepare_talweg_metrics(dataset = input_talweg_metrics)

landcover_area <- prepare_landcover_area(dataset = input_landcover)

continuity_area <- prepare_continuity_area(dataset = input_continuity)

continuity_width <- prepare_continuity_width(dataset = input_continuity)

valley_bottom <- prepare_valley_bottom(dataset = input_valley_bottom)

hydro_swaths_and_axis <- prepare_hydro_swaths_and_axis(swaths_dataset = input_swaths,
                                                       referentiel_hydro_dataset = input_referentiel_hydro,
                                                       region_hydro = region_hydrographique)
```

### Set database structure

``` r
create_table_bassin_hydrographique(table_name = "bassin_hydrographique",
                                    db_con = db_con())

create_table_region_hydrographique(table_name = "region_hydrographique",
                                   db_con = db_con())

create_table_roe(table_name = "roe",
                 db_con = db_con())

create_table_hydro_stations(table_name = "hydro_stations",
                            db_con = db_con())

create_table_hydro_axis(table_name = "hydro_axis",
                                   db_con = db_con())

create_table_hydro_swaths(table_name = "hydro_swaths",
                          db_con = db_con())

create_table_talweg_metrics(table_name = "talweg_metrics",
                            db_con = db_con())

create_table_landcover_area(table_name = "landcover_area",
                            db_con = db_con())

create_table_continuity_area(table_name = "continuity_area",
                             db_con = db_con())

create_table_continuity_width(table_name = "continuity_width",
                              db_con = db_con())

create_table_valley_bottom(table_name = "valley_bottom",
                           db_con = db_con())
```

### Add functions and triggers to Postgresql database

``` r
# hydro_swaths triggers
fct_hydro_swaths_insert_delete_reaction(db_con = db_con(), table_name = "hydro_swaths")
trig_hydro_swaths(db_con = db_con(), table_name = "hydro_swaths")

# talweg_metrics triggers
fct_talweg_metrics_insert_delete_reaction(db_con = db_con(), table_name = "talweg_metrics")
trig_talweg_metrics(db_con = db_con(), table_name = "talweg_metrics")

# landcover_area triggers
fct_landcover_area_insert_delete_reaction(db_con = db_con(), table_name = "landcover_area")
trig_landcover_area(db_con = db_con(), table_name = "landcover_area")

# continuity_area triggers
fct_continuity_area_insert_delete_reaction(db_con = db_con(), table_name = "continuity_area")
trig_continuity_area(db_con = db_con(), table_name = "continuity_area")

# continuity_width triggers
fct_continuity_width_insert_delete_reaction(db_con = db_con(), table_name = "continuity_width")
trig_continuity_width(db_con = db_con(), table_name = "continuity_width")

# valley_bottom triggers
fct_valley_bottom_insert_delete_reaction(db_con = db_con(), table_name = "valley_bottom")
trig_valley_bottom(db_con = db_con(), table_name = "valley_bottom")
```

### Create views

``` r
create_landcover_area_full_side_matview(db_con = db_con(), view_name = "landcover_area_full_side")
create_continuity_width_full_side_matview(db_con = db_con(), view_name = "continuity_width_full_side")
create_continuity_area_full_side_matview(db_con = db_con(), view_name = "continuity_area_full_side")
create_valley_bottom_full_side_matview(db_con = db_con(), view_name = "valley_bottom_full_side")
create_network_metrics_matview(db_con = db_con(), view_name = "network_metrics")
create_network_axis_matview(db_con = db_con(), view_name = "network_axis")
```

### Update and insert database, refresh materialized view

``` r
upsert_bassin_hydrographique(dataset = bassin_hydrographique,
                             table_name = "bassin_hydrographique",
                             db_con = db_con(),
                             field_identifier = "cdbh")

set_displayed_bassin_region(table_name = "bassin_hydrographique",
                            display_codes_bassin_or_region = c("06"),
                            field_identifier = "cdbh",
                            db_con = db_con())

upsert_region_hydrographique(dataset = region_hydrographique,
                             table_name = "region_hydrographique",
                             db_con = db_con(),
                             field_identifier = "cdregionhy")

set_displayed_bassin_region(table_name = "region_hydrographique",
                            display_codes_bassin_or_region = c("W"),
                            field_identifier = "cdregionhy",
                            db_con = db_con())

upsert_roe(dataset = roe,
           table_name = "roe", 
           db_con = db_con(), 
           field_identifier = "cdobstecou")

upsert_hydro_stations(dataset = hydro_stations,
                      table_name = "hydro_stations",
                      db_con = db_con(),
                      field_identifier = "code_station")

upsert_hydro_swaths_and_axis(hydro_swaths_dataset = hydro_swaths_and_axis$hydro_swaths,
                             hydro_swaths_table_name = "hydro_swaths",
                             hydro_axis_dataset = hydro_swaths_and_axis$hydro_axis,
                             hydro_axis_table_name = "hydro_axis",
                             db_con = db_con(),
                             field_identifier = "axis")

upsert_talweg_metrics(dataset = talweg_metrics,
                      table_name = "talweg_metrics",
                      db_con = db_con(),
                      field_identifier = "axis")

upsert_landcover_area(dataset = landcover_area,
                      table_name = "landcover_area",
                      db_con(),
                      field_identifier = "axis")

upsert_continuity_area(dataset = continuity_area,
                      table_name = "continuity_area",
                      db_con(),
                      field_identifier = "axis")

upsert_continuity_width(dataset = continuity_width,
                      table_name = "continuity_width",
                      db_con(),
                      field_identifier = "axis")

upsert_valley_bottom(dataset = valley_bottom,
                     table_name = "valley_bottom",
                     db_con(),
                     field_identifier = "axis")

refresh_all_materialized_views(db_con = db_con())
```
