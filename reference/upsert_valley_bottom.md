# Delete existing rows and insert valley botom to database.

Delete existing rows and insert valley botom to database.

## Usage

``` r
upsert_valley_bottom(
  dataset = valley_bottom,
  table_name = "valley_bottom",
  db_con,
  field_identifier = "axis"
)
```

## Arguments

- dataset:

  sf data.frame valley bottom.

- table_name:

  text database table name.

- db_con:

  DBI connection to database.

- field_identifier:

  text field identifier name to identified rows to remove.

## Value

text
