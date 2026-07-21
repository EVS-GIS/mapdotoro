# Delete existing rows and insert continuity area to database.

Delete existing rows and insert continuity area to database.

## Usage

``` r
upsert_continuity_area(
  dataset = continuity_area,
  table_name = "continuity_area",
  db_con,
  field_identifier = "axis"
)
```

## Arguments

- dataset:

  data.frame continuity area.

- table_name:

  text database table name.

- db_con:

  DBI connection to database.

- field_identifier:

  text field identifier name to identified rows to remove.

## Value

text
