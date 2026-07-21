# Delete existing rows and insert hydrometric sites to database.

Delete existing rows and insert hydrometric sites to database.

## Usage

``` r
upsert_hydro_sites(
  dataset = hydro_sites,
  table_name = "hydro_sites",
  db_con,
  field_identifier = "code_site"
)
```

## Arguments

- dataset:

  sf data.frame hydrometric sites

- table_name:

  text database table name.

- db_con:

  DBI connection to database.

- field_identifier:

  text field identifier name to identified rows to remove.

## Value

text
