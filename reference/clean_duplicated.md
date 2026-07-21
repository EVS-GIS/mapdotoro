# Drop all the duplicated rows from a duplicated data.frame.

Drop all the duplicated rows from a duplicated data.frame.

## Usage

``` r
clean_duplicated(
  dataset,
  duplicated_dataset,
  axis_field = "AXIS",
  measure_field = "M"
)
```

## Arguments

- dataset:

  data.frame.

- duplicated_dataset:

  data.frame produce by check_duplicate function.

- axis_field:

  axis stream id.

- measure_field:

  axis stream measure.

## Value

sf data.frame.
