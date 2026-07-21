# Remove rows in database table based on field identifier.

All the rows in the database table are removed based on values in field
identifier in the dataset.

## Usage

``` r
remove_rows(dataset, field_identifier, table_name)
```

## Arguments

- dataset:

  data.frame dataset.

- field_identifier:

  text field identifier name to identified rows to remove.

- table_name:

  text database table name.

## Value

text number of row deleted.
