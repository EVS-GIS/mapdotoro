
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
#> Warning: le package 'dplyr' a été compilé avec la version R 4.2.3
#> 
#> Attachement du package : 'dplyr'
#> Les objets suivants sont masqués depuis 'package:stats':
#> 
#>     filter, lag
#> Les objets suivants sont masqués depuis 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(sf)
#> Warning: le package 'sf' a été compilé avec la version R 4.2.3
#> Linking to GEOS 3.9.3, GDAL 3.5.2, PROJ 8.2.1; sf_use_s2() is TRUE
library(tmap)
#> Warning: le package 'tmap' a été compilé avec la version R 4.2.3
#> Breaking News: tmap 3.x is retiring. Please test v4, e.g. with
#> remotes::install_github('r-tmap/tmap')
library(mapdotoro)

data(bassin_hydrographique)
data(region_hydrographique)
data(referentiel_hydro)
data(swaths)
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

clean dataset

``` r
swaths_data <- swaths %>% 
  dplyr::filter(VALUE == 2) # keep only valid swaths

sf::st_geometry(swaths_data) <- "geom" # standard geometry column

duplicated_swaths <- check_duplicate(swaths_data)
#> L'axe 2000796122 a des doublons
cleaned_swaths <- clean_duplicated(dataset = swaths_data,
                                   duplicated_dataset = duplicated_swaths)
```

``` r
map + tmap::tm_shape(duplicated_swaths) + 
  tmap::tm_polygons(col = "red")
```
