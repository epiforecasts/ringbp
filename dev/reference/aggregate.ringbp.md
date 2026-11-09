# Aggregate ringbp outbreak simulations by time interval

Aggregates the simulated cases in the `$outbreak_ts` element of a
`<ringbp>` object (returned by
[`outbreak_model()`](https://epiforecasts.io/ringbp/dev/reference/outbreak_model.md)
or
[`scenario_sim()`](https://epiforecasts.io/ringbp/dev/reference/scenario_sim.md))
into the number of new cases, by symptom onset time, in each time
interval.

This is called internally by
[`outbreak_model()`](https://epiforecasts.io/ringbp/dev/reference/outbreak_model.md)
using the `aggregation` argument of
[`sim_opts()`](https://epiforecasts.io/ringbp/dev/reference/sim_opts.md),
and can also be called by users to aggregate simulation output to a
different time interval after the simulation.

## Usage

``` r
# S3 method for class 'ringbp'
aggregate(x, interval, ...)
```

## Arguments

- x:

  a `<ringbp>` object: the output of
  [`outbreak_model()`](https://epiforecasts.io/ringbp/dev/reference/outbreak_model.md)
  or
  [`scenario_sim()`](https://epiforecasts.io/ringbp/dev/reference/scenario_sim.md).

- interval:

  the time interval to aggregate the outbreak simulation to, accepts the
  same values as the `aggregation` argument of
  [`sim_opts()`](https://epiforecasts.io/ringbp/dev/reference/sim_opts.md).

- ...:

  [dots](https://rdrr.io/r/base/dots.html) Not used; any extra arguments
  are ignored with a warning.

## Value

A `<ringbp>` object with the `$outbreak_ts` element aggregated to
`interval`. The `$outbreak_stats` element is unchanged. See
[`outbreak_model()`](https://epiforecasts.io/ringbp/dev/reference/outbreak_model.md)
for a description of the `$outbreak_ts` columns for each time interval.

## Details

Time in the ringbp model is measured in days since the exposure of the
initial cases (day 0); there are no calendar dates. The time intervals
are therefore fixed-length blocks of days counted from day 0 (a
`"weekly"` interval is 7 days and a `"monthly"` interval is 30 days),
and are not aligned to calendar weeks or months.

Aggregated output contains every interval from 0 up to the end of the
simulation, the final interval ends at `cap_max_days` (see
[`sim_opts()`](https://epiforecasts.io/ringbp/dev/reference/sim_opts.md)).
Intervals with no cases are included with zero cases. Only cases with
symptom onset before `cap_max_days` are included, as cases with a later
symptom onset are not fully simulated.

Aggregation cannot be undone, so a `<ringbp>` object can only be
aggregated if it is disaggregated (`"none"`, i.e. a case line list), or
if each new interval is made up of whole intervals of the current
aggregation, for example `"daily"` can be aggregated to `"weekly"` or
`"monthly"`, and `"weekly"` can be aggregated to `14` (days), but
`"weekly"` cannot be aggregated to `"monthly"` or `"daily"`. To be able
to aggregate to any interval use `aggregation = "none"` in
[`sim_opts()`](https://epiforecasts.io/ringbp/dev/reference/sim_opts.md)
when running the simulation.

The simulation must also be divided exactly into the new interval, so no
simulated cases are dropped and the final interval is complete. For
example, a simulation with `cap_max_days = 350` can be aggregated from
`"weekly"` to `14` (days), but not to `28` (days) as 350 days is not a
whole number of 28 day intervals.

## Examples

``` r
set.seed(1)
res <- scenario_sim(
  n = 2,
  initial_cases = 1,
  offspring = offspring_opts(
    community = \(n) rnbinom(n = n, mu = 2.5, size = 0.16),
    isolated = \(n) rnbinom(n = n, mu = 0.5, size = 1)
  ),
  delays = delay_opts(
    incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
    onset_to_isolation = \(n) rweibull(n = n, shape = 1.65, scale = 4.28)
  ),
  event_probs = event_prob_opts(
    asymptomatic = 0,
    presymptomatic_transmission = 0.5,
    symptomatic_traced = 0.2
  ),
  interventions = intervention_opts(),
  sim = sim_opts(cap_max_days = 70, aggregation = "none")
)

# case line list
res$outbreak_ts
#>       sim     onset cumulative
#>     <int>     <num>      <int>
#>  1:     1  2.305809          1
#>  2:     1  5.992616          2
#>  3:     2  6.185432          1
#>  4:     2  8.127922          2
#>  5:     2  9.269283          3
#>  6:     2  9.306320          4
#>  7:     2 11.162158          5
#>  8:     2 11.735224          6
#>  9:     2 15.484999          7
#> 10:     2 16.083808          8
#> 11:     2 16.261532          9
#> 12:     2 16.314829         10
#> 13:     2 16.989381         11
#> 14:     2 17.044812         12
#> 15:     2 17.471405         13
#> 16:     2 18.370284         14
#> 17:     2 21.022480         15
#> 18:     2 21.793299         16
#> 19:     2 22.790005         17
#> 20:     2 23.218196         18
#> 21:     2 24.163121         19
#> 22:     2 24.542748         20
#> 23:     2 24.557298         21
#> 24:     2 24.752679         22
#> 25:     2 25.342676         23
#> 26:     2 26.088106         24
#>       sim     onset cumulative
#>     <int>     <num>      <int>

# aggregate to daily cases
aggregate(res, interval = "daily")
#> $outbreak_ts
#>        sim   day daily_cases cumulative
#>      <int> <int>       <int>      <int>
#>   1:     1     0           0          0
#>   2:     1     1           0          0
#>   3:     1     2           1          1
#>   4:     1     3           0          1
#>   5:     1     4           0          1
#>  ---                                   
#> 136:     2    65           0         24
#> 137:     2    66           0         24
#> 138:     2    67           0         24
#> 139:     2    68           0         24
#> 140:     2    69           0         24
#> 
#> $outbreak_stats
#>      sim effective_r0 cases_per_gen
#>    <int>        <num>        <list>
#> 1:     1     0.500000           1,0
#> 2:     2     2.007143     7,4,7,5,0
#> 
#> attr(,"class")
#> [1] "ringbp"

# aggregate to fortnightly cases
aggregate(res, interval = 14)
#> $outbreak_ts
#>       sim period period_cases cumulative
#>     <int>  <int>        <int>      <int>
#>  1:     1      0            2          2
#>  2:     1      1            0          2
#>  3:     1      2            0          2
#>  4:     1      3            0          2
#>  5:     1      4            0          2
#>  6:     2      0            6          6
#>  7:     2      1           18         24
#>  8:     2      2            0         24
#>  9:     2      3            0         24
#> 10:     2      4            0         24
#> 
#> $outbreak_stats
#>      sim effective_r0 cases_per_gen
#>    <int>        <num>        <list>
#> 1:     1     0.500000           1,0
#> 2:     2     2.007143     7,4,7,5,0
#> 
#> attr(,"class")
#> [1] "ringbp"
```
