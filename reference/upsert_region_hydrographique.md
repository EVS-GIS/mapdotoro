# Delete existing rows and insert hydrologic region to database.

Delete existing rows and insert hydrologic region to database.

## Usage

``` r
upsert_region_hydrographique(
  dataset = region_hydrographique,
  table_name = "region_hydrographique",
  db_con,
  field_identifier = "cdregionhy"
)
```

## Arguments

- dataset:

  sf data.frame hydrologic region.

- table_name:

  database table name.

- db_con:

  DBI connection to database.

- field_identifier:

  text field identifier name to identified rows to remove.

## Value

text
