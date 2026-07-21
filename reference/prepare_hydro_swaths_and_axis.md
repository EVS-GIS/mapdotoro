# Prepare hydro_swaths dataset to database export.

Prepare hydro_swaths dataset to database export.

## Usage

``` r
prepare_hydro_swaths_and_axis(
  swaths_dataset = input_swaths,
  referentiel_hydro_dataset = input_referentiel_hydro,
  region_hydro = region_hydrographique
)
```

## Arguments

- swaths_dataset:

  sf data.frame swaths dataset.

- referentiel_hydro_dataset:

  sf data.frame hydrographic network dataset.

- region_hydro:

  sf data.frame region_hydrographique dataset prepared.

## Value

list with two sf data.frame
