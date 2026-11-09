# Check and standardise a time interval for aggregation

Check and standardise a time interval for aggregation

## Usage

``` r
match_aggregation(aggregation)
```

## Arguments

- aggregation:

  a time interval, see the `aggregation` argument of
  [`sim_opts()`](https://epiforecasts.io/ringbp/dev/reference/sim_opts.md).
  `character` strings are case insensitive.

## Value

A `character` string (`"none"`, `"daily"`, `"weekly"` or `"monthly"`) or
an `integer` scalar (number of days). An error is thrown if
`aggregation` is invalid.
