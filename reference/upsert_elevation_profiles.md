# Delete existing rows and insert elevation profiles to database.

Delete existing rows and insert elevation profiles to database.

## Usage

``` r
upsert_elevation_profiles(
  dataset = elevation_profiles,
  table_name = "elevation_profiles",
  db_con,
  field_identifier = "axis"
)
```

## Arguments

- dataset:

  sf data.frame elevation profiles.

- table_name:

  text database table name.

- db_con:

  DBI connection to database.

- field_identifier:

  text field identifier name to identified rows to remove.

## Value

text
