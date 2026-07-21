# Delete existing rows and insert landcover area to database.

Delete existing rows and insert landcover area to database.

## Usage

``` r
upsert_landcover_area(
  dataset = landcover_area,
  table_name = "landcover_area",
  db_con,
  field_identifier = "axis"
)
```

## Arguments

- dataset:

  sf data.frame landcover area.

- table_name:

  text database table name.

- db_con:

  DBI connection to database.

- field_identifier:

  text field identifier name to identified rows to remove.

## Value

text
