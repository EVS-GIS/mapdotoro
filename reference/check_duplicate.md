# Check for duplicate measure in streams axis.

Check for duplicate measure in streams axis.

## Usage

``` r
check_duplicate(dataset, axis_field = "AXIS", measure_field = "M")
```

## Arguments

- dataset:

  data.frame.

- axis_field:

  axis stream id.

- measure_field:

  axis stream measure from exutoire.

## Value

a list with a sf data.frame with all duplicated rows and a data.frame
with the number of duplicated by axis.
