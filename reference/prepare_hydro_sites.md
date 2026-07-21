# Prepare hydrometric sites to database export.

Prepare hydrometric sites to database export.

## Usage

``` r
prepare_hydro_sites(
  dataset = input_hydro_sites,
  region_hydro = region_hydrographique
)
```

## Arguments

- dataset:

  sf data.frame hydrometric sites imported.

- region_hydro:

  sf data.frame hydrologic regions to set gid_region.

## Value

sf data.frame hydrometric sites prepared.
