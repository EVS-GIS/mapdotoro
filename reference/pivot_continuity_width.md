# Prepare continuity dataset to have width for each label

Each landcover and continuity dataset have several rows for each swath
by label and left or right side. We need to pivot the dataset to have
two row for each side, and width labels are summarized in columns.

## Usage

``` r
pivot_continuity_width(dataset)
```

## Arguments

- dataset:

  A continuity data.frame.

## Value

data.frame
