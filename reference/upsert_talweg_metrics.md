# Delete existing rows and insert talweg metrics to database.

Delete existing rows and insert talweg metrics to database.

## Usage

``` r
upsert_talweg_metrics(
  dataset = talweg_metrics,
  table_name = "talweg_metrics",
  db_con,
  field_identifier = "axis"
)
```

## Arguments

- dataset:

  data.frame talweg metrics.

- table_name:

  text database table name.

- db_con:

  DBI connection to database.

- field_identifier:

  text field identifier name to identified rows to remove.

## Value

text
