# Outbreak extinction functions

`extinct_prob()`: Calculate proportion of runs that have controlled
outbreak

`detect_extinct()`: Calculate whether outbreaks went extinct or not

## Usage

``` r
extinct_prob(scenario, extinction_time = NULL)

detect_extinct(scenario, extinction_time = NULL)
```

## Arguments

- scenario:

  a `list` output by
  [`scenario_sim()`](https://epiforecasts.io/ringbp/dev/reference/scenario_sim.md),
  containing the `$outbreak_ts` `data.table` of cases used to determine
  extinction

- extinction_time:

  By default `NULL` but also accepts a positive `integer` scalar or
  `integer` vector to test if the outbreak has gone extinct (i.e. no new
  cases) by this time (zero indexed), in the time interval of the
  aggregated `scenario` (e.g. weeks for weekly aggregated data):

  - By default (`NULL`) the extinction status is stored in the output of
    [`scenario_sim()`](https://epiforecasts.io/ringbp/dev/reference/scenario_sim.md)
    which is supplied to the `scenario` argument. If `extinction_time`
    is not specified or specified as `NULL` then the pre-computed
    extinction status from the outbreak simulation will be used. This is
    defined as all infectious cases have had the opportunity to transmit
    but no new cases are generated.

  - A single `integer` to test if extinction has occurred by this time.
    For example, for weekly data, `extinction_time = 5` tests whether
    the outbreak is extinct by week 5 (inclusive) until the end of the
    outbreak.

  - An `integer` vector of length two can be supplied to provide the
    lower and upper bounds (inclusive) of the time range to test for
    whether extinction occurred by this window. For example, for weekly
    data, `extinction_time = c(5, 10)` will test whether the outbreak
    went extinct by week 5 and there were no new cases between weeks 5
    and 10 (inclusive).

  - An `integer` vector of length *n* can be supplied to provide the
    times to test for whether extinction occurred by this window. For
    example, for weekly data, `extinction_time = 12:16` will test that
    there are no new infections between 12 and 16 weeks after the
    initial cases (inclusive). These integer sequences will most likely
    be contiguous but the function does allow non-contiguous integer
    sequences.

  If extinction occurs before the `extinction_time` window then the
  outbreak extinction is considered extinct, however, if the extinction
  occurs within the `extinction_time` window it is not considered
  extinct. Therefore, using a single `integer` for `extinction_time` and
  thinking of this as "*has the outbreak gone extinct by time X*".

## Value

`extinct_prob()`: a single `numeric` with the probability of extinction

`detect_extinct()`: a `data.table`, with two columns `sim` and
`extinct`, for a binary classification of whether the outbreak went
extinct in each simulation replicate. `1` is an outbreak that went
extinct, `0` if not.

## Details

The data passed to `scenario` has to be produced by
[`scenario_sim()`](https://epiforecasts.io/ringbp/dev/reference/scenario_sim.md).
It cannot be produced by
[`outbreak_model()`](https://epiforecasts.io/ringbp/dev/reference/outbreak_model.md)
as it requires the `sim` column, which is only appended in
[`scenario_sim()`](https://epiforecasts.io/ringbp/dev/reference/scenario_sim.md).

***Warning***: the `$outbreak_ts` element of the
[`scenario_sim()`](https://epiforecasts.io/ringbp/dev/reference/scenario_sim.md)
output carries an `cap_cases` attribute which is used by
`extinct_prob()` and `detect_extinct()`, therefore if you modify the
output of
[`scenario_sim()`](https://epiforecasts.io/ringbp/dev/reference/scenario_sim.md)
before passing to `extinct_prob()` be careful not to drop the attribute
(e.g. from subsetting the `data.table`).

The `extinction_time` is in the time interval the `scenario` is
aggregated to (set by the `aggregation` argument in
[`sim_opts()`](https://epiforecasts.io/ringbp/dev/reference/sim_opts.md),
or by
[`aggregate()`](https://epiforecasts.io/ringbp/dev/reference/aggregate.ringbp.md)),
for example weeks for the default `"weekly"` aggregation, or days for
`"daily"` aggregation. If the `scenario` is not aggregated
(`aggregation = "none"`) it is aggregated to daily cases, with a
warning, and `extinction_time` is in days.

## Examples

``` r
res <- scenario_sim(
  n = 10,
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
  interventions = intervention_opts(quarantine = FALSE),
  sim = sim_opts(cap_max_days = 350, cap_cases = 4500)
)

# calculate probability of extinction
extinct_prob(res)
#> Calculating extinction using the extinction status from the simulation.
#> [1] 0.9

# determine if each outbreak simulation replicate has gone extinct
detect_extinct(res)
#> Calculating extinction using the extinction status from the simulation.
#>       sim extinct
#>     <int>   <int>
#>  1:     1       1
#>  2:     2       1
#>  3:     3       1
#>  4:     4       1
#>  5:     5       1
#>  6:     6       1
#>  7:     7       1
#>  8:     8       0
#>  9:     9       1
#> 10:    10       1

# calculate extinction in the last 2 weeks of the simulated outbreak
# (i.e. the penultimate and last week of the outbreak)
extinct_prob(res, extinction_time = max(res$outbreak_ts$week) - 1)
#> Calculating extinction as no new cases within weeks: 48 to 49 (inclusive).
#> [1] 0.9

# calculate extinction as no new cases between weeks 12 and 16 of the outbreak
extinct_prob(res, extinction_time = 12:16)
#> Calculating extinction as no new cases within weeks: 12 to 16 (inclusive).
#> [1] 0.9
```
