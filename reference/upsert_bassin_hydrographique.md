# Delete existing rows and insert hydrologic bassin to database

Delete existing rows and insert hydrologic bassin to database

## Usage

``` r
upsert_bassin_hydrographique(
  dataset = bassin_hydrographique,
  table_name = "bassin_hydrographique",
  db_con,
  field_identifier = "cdbh"
)
```

## Arguments

- dataset:

  sf data.frame hydrologic bassin.

- table_name:

  text database table name.

- db_con:

  DBI connection to database.

- field_identifier:

  text field identifier name to identified rows to remove.

## Value

text
