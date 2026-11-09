# Column names of the aggregated outbreak time series

Column names of the aggregated outbreak time series

## Usage

``` r
aggregation_cols(aggregation)
```

## Arguments

- aggregation:

  a time interval, standardised by
  [`match_aggregation()`](https://epiforecasts.io/ringbp/dev/reference/match_aggregation.md),
  excluding `"none"`.

## Value

A named `character` vector with the time interval (`time`) and case
count (`cases`) column names.
