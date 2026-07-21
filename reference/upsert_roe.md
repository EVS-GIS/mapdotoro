# Delete existing rows and insert roe to database

Delete existing rows and insert roe to database

## Usage

``` r
upsert_roe(
  dataset = roe,
  table_name = "roe",
  db_con,
  field_identifier = "cdobstecou"
)
```

## Arguments

- dataset:

  sf data.frame roe.

- table_name:

  text database table name.

- db_con:

  DBI connection to database.

- field_identifier:

  text field identifier name to identified rows to remove.

## Value

text
