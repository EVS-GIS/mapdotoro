
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

Testing dataset can be found

``` r
library(dplyr)
library(sf)
library(tmap)
library(mapdotoro)
library(tidyr)
library(lwgeom)
library(qgisprocess)

bassin_hydrographique <- bassin_hydrographique
region_hydrographique <- region_hydrographique
referentiel_hydro <- referentiel_hydro
swaths <- swaths
talweg_metrics <- talweg_metrics
landcover <- landcover
continuity <- continuity
```

set geometry column name

``` r
st_geometry(bassin_hydrographique) <- "geometry"
st_geometry(region_hydrographique) <- "geometry"
st_geometry(referentiel_hydro) <- "geometry"
st_geometry(swaths) <- "geometry"
```

Map dataset

``` r
tmap::tmap_mode("view")
map <- tmap::tm_shape(bassin_hydrographique) +
  tmap::tm_polygons()+
tmap::tm_shape(region_hydrographique) +
  tmap::tm_polygons()+
tmap::tm_shape(swaths) +
  tmap::tm_polygons()+
tmap::tm_shape(referentiel_hydro) +
  tmap::tm_lines(col= "blue")
map
```

clean swaths

``` r
swaths_data <- swaths %>% 
  dplyr::filter(VALUE == 2) # keep only valid swaths

duplicated_swaths <- check_duplicate(swaths_data)
cleaned_swaths <- clean_duplicated(dataset = swaths_data,
                                   duplicated_dataset = duplicated_swaths)
  # dplyr::mutate(id = row_number())
```

clip referentiel hydro by swaths

``` r
hydro_swaths <- st_sf(st_sfc(crs = 2154)) # create an empty sf dataset
# clip by axis
for (axis in unique(cleaned_swaths$AXIS)){
  swaths_axe <- cleaned_swaths %>%
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
hydro_swaths_len <- hydro_swaths %>%
  mutate(id = row_number()) %>% 
  mutate(LENG = st_length(.))
  # mutate(M = ifelse(LENG < units::set_units(10, m), NA, M)) # length is unit object

hydro_swaths_len_dupl <- check_duplicate(hydro_swaths_len)

hydro_swaths_len_dupl_corr <- st_sf(st_sfc(crs = 2154)) # create an empty sf dataset
for (measure in unique(hydro_swaths_len_dupl$M)){
  hydro_swaths_len_dupl_measure <- hydro_swaths_len_dupl %>% 
    filter(M == measure) %>% 
    filter(LENG < max(LENG)) %>% 
    mutate(M = -1)
  hydro_swaths_len_dupl_corr <- rbind(hydro_swaths_len_dupl_corr, hydro_swaths_len_dupl_measure)
}

hydro_swaths_len_dupl_corr <- hydro_swaths_len_dupl_corr %>%
  st_drop_geometry() %>% 
  select (id, M)

merged_hydro_swaths <- merge(hydro_swaths_len, hydro_swaths_len_dupl_corr, by = "id", all.x = TRUE) %>% 
  mutate(M.x = ifelse(!is.na(M.y) & M.y == -1, NA, M.x)) %>% 
  select(-M.y) %>% 
  rename("M" = M.x) %>% 
  select(AXIS, M, DRAINAGE, geometry)
```

Measure network from outlet

``` r
identifynetworknodes <- merged_hydro_swaths %>% 
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
talweg_metrics <- talweg_metrics %>% 
  select(-swath)
```

Prepare landcover and continuity area

``` r
landcover_prepared <- prepare_landcover_continuity_area(landcover)

continuity_prepared <- prepare_landcover_continuity_area(continuity)
```
