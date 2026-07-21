# Delete existing rows and insert hydrologic network splited by swaths to database.

Delete existing rows and insert hydrologic network splited by swaths to
database.

## Usage

``` r
upsert_hydro_swaths_and_axis(
  hydro_swaths_dataset = hydro_swaths,
  hydro_swaths_table_name = "hydro_swaths",
  hydro_axis_dataset = hydro_axis,
  hydro_axis_table_name = "hydro_axis",
  db_con,
  field_identifier = "axis"
)
```

## Arguments

- hydro_swaths_dataset:

  sf data.frame hydro swaths prepared.

- hydro_swaths_table_name:

  text hydro_swaths table name.

- hydro_axis_dataset:

  sf data.frame hydro axis prepared.

- hydro_axis_table_name:

  text hydro_axis table name.

- db_con:

  DBI connection to database.

- field_identifier:

  text field identifier name to identified rows to remove.

## Value

text
