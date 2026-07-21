# Prepare landcover or continuity dataset to have area for each label.

Each landcover and continuity dataset have several rows for each swath
by label and left or right side. We need to pivot the dataset to have
two row for each side, and area labels are summarized in columns.

## Usage

``` r
pivot_landcover_continuity_area(dataset)
```

## Arguments

- dataset:

  A landuse or continuity data.frame.

## Value

data.frame
