# Delete existing rows and insert continuity width to database.

Delete existing rows and insert continuity width to database.

## Usage

``` r
upsert_continuity_width(
  dataset = continuity_width,
  table_name = "continuity_width",
  db_con,
  field_identifier = "axis"
)
```

## Arguments

- dataset:

  sf data.frame continuity width.

- table_name:

  text database table name.

- db_con:

  DBI connection to database.

- field_identifier:

  text field identifier name to identified rows to remove.

## Value

text
