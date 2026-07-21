# Set display column value for bassin or region table.

Set the display field in hydrographic bassin or region database tables.
The display field is used for mapdoapp to show only the wanted bassins
and regions data.

## Usage

``` r
set_displayed_bassin_region(
  table_name,
  display_codes_bassin_or_region,
  field_identifier,
  db_con
)
```

## Arguments

- table_name:

  bassin or region table name.

- display_codes_bassin_or_region:

  A vector with the list of cdbh for bassin, cdregionhy for region value
  to set the displayed polygons.

- field_identifier:

  text field identifier name to identified rows to remove.

- db_con:

  DBI connection to database.

## Value

text
