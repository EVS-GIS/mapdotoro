
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
data(bassin_hydrographique)
data(region_hydrographique)
data(referentiel_hydro)
data(swaths)
```

Map dataset

``` r
tmap::tmap_mode("view")
tmap::tm_shape(bassin_hydrographique) +
  tmap::tm_polygons()+
tmap::tm_shape(region_hydrographique) +
  tmap::tm_polygons()+
tmap::tm_shape(swaths) +
  tmap::tm_polygons()+
tmap::tm_shape(referentiel_hydro) +
  tmap::tm_lines()
```
