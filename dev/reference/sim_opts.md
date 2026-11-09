# Create a list of simulation control options for the ringbp model

Create a list of simulation control options for the ringbp model

## Usage

``` r
sim_opts(cap_max_days = 350, cap_cases = 5000, aggregation = "weekly")
```

## Arguments

- cap_max_days:

  a positive `integer` scalar: the maximum number of days of the
  outbreak simulation, counted from the exposure of the initial cases on
  day 0. The outbreak simulation stops once all new infections occur
  after `cap_max_days`, so every case with symptom onset before
  `cap_max_days` is simulated.

  `cap_max_days` must be a whole number of `aggregation` time intervals
  (e.g. a multiple of 7 for `"weekly"` aggregation, or of 30 for
  `"monthly"`), so the output contains every time interval up to
  `cap_max_days` and the final interval ends at `cap_max_days`. The
  default of 350 days is 50 weeks.

- cap_cases:

  a positive `integer` scalar: number of cumulative cases at which the
  branching process (simulation) was terminated

- aggregation:

  a `character` string or a positive `integer` scalar: the time interval
  to aggregate the simulated cases to, by symptom onset time. One of:

  - `"none"`: no aggregation, the output contains the symptom onset time
    of each case with symptom onset before `cap_max_days` (i.e. a line
    list). See
    [`outbreak_model()`](https://epiforecasts.io/ringbp/dev/reference/outbreak_model.md)
    for the output columns.

  - `"daily"` (or `"day"`, `"days"`): number of cases each day.

  - `"weekly"` (or `"week"`, `"weeks"`): number of cases each week (7
    days). This is the default.

  - `"monthly"` (or `"month"`, `"months"`): number of cases each month
    (30 days).

  - A positive `integer`: number of cases in each period of that many
    days (e.g. `14` for fortnightly cases).

  `character` strings are case insensitive (e.g. `"Weekly"` is the same
  as `"weekly"`).

  Intervals are fixed-length blocks of days counted from the exposure of
  the initial cases on day 0, not calendar weeks or months. The output
  can be aggregated to a different interval after the simulation using
  [`aggregate()`](https://epiforecasts.io/ringbp/dev/reference/aggregate.ringbp.md),
  if the simulation can be divided exactly into the new interval (i.e.
  `cap_max_days` is a multiple of the new interval).

## Value

A `list` with class `<ringbp_sim_opts>`.

## Examples

``` r
# default simulation control options
sim_opts()
#> $cap_max_days
#> [1] 350
#> 
#> $cap_cases
#> [1] 5000
#> 
#> $aggregation
#> [1] "weekly"
#> 
#> attr(,"class")
#> [1] "ringbp_sim_opts"

# specifying custom simulation control options
sim_opts(
  cap_max_days = 140,
  cap_cases = 1000
)
#> $cap_max_days
#> [1] 140
#> 
#> $cap_cases
#> [1] 1000
#> 
#> $aggregation
#> [1] "weekly"
#> 
#> attr(,"class")
#> [1] "ringbp_sim_opts"

# daily case counts
sim_opts(aggregation = "daily")
#> $cap_max_days
#> [1] 350
#> 
#> $cap_cases
#> [1] 5000
#> 
#> $aggregation
#> [1] "daily"
#> 
#> attr(,"class")
#> [1] "ringbp_sim_opts"

# monthly case counts, cap_max_days must be a multiple of 30
sim_opts(cap_max_days = 360, aggregation = "monthly")
#> $cap_max_days
#> [1] 360
#> 
#> $cap_cases
#> [1] 5000
#> 
#> $aggregation
#> [1] "monthly"
#> 
#> attr(,"class")
#> [1] "ringbp_sim_opts"

# no aggregation (case line list)
sim_opts(aggregation = "none")
#> $cap_max_days
#> [1] 350
#> 
#> $cap_cases
#> [1] 5000
#> 
#> $aggregation
#> [1] "none"
#> 
#> attr(,"class")
#> [1] "ringbp_sim_opts"
```
