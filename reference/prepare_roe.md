# Prepare roe dataset to database export.

Prepare roe dataset to database export.

## Usage

``` r
prepare_roe(
  dataset = input_roe,
  region_hydro = region_hydrographique,
  troncon_bdtopo_id = input_troncon_bdtopo_id,
  hydro_axis = hydro_axis
)
```

## Arguments

- dataset:

  sf data.frame roe.

- region_hydro:

  sf data.frame hydrographic regions to set spatial join on gid_region.

- troncon_bdtopo_id:

  data.frame with id_troncon and axis.

- hydro_axis:

  sf data.frame with hydrologic axis prepared.

## Value

sf data.frame
