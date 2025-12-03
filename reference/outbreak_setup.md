# Set up initial cases for branching process

Set up initial cases for branching process

## Usage

``` r
outbreak_setup(initial_cases, delays, event_probs)
```

## Arguments

- initial_cases:

  a non-negative `integer` scalar: number of initial or starting cases
  which are all assumed to be missed.

- delays:

  a `list` with class `<ringbp_delay_opts>`: the delay distribution
  `function`s for the ringbp model, returned by
  [`delay_opts()`](https://epiforecasts.io/ringbp/reference/delay_opts.md).
  Contains two elements: `incubation_period` and `onset_to_isolation`

- event_probs:

  a `list` with class `<ringbp_event_prob_opts>`: the event
  probabilities for the ringbp model, returned by
  [`event_prob_opts()`](https://epiforecasts.io/ringbp/reference/event_prob_opts.md).
  Contains three elements: `asymptomatic`, `presymptomatic_transmission`
  and `symptomatic_ascertained`

## Value

`data.table` of cases in outbreak so far. `data.table` columns are:

- `$exposure`: `numeric`

- `$asymptomatic`: `logical`

- `$caseid`: `integer`

- `$infector`: `numeric`

- `$missed`: `logical`

- `$onset`: `numeric`

- `$new_cases`: `integer`

- `$isolated_time`: `numeric`

- `$sampled`: `logical`

## Examples

``` r
delays <- delay_opts(
  incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
  onset_to_isolation = \(n) rweibull(n = n, shape = 1.65, scale = 4.28)
)
event_probs <- event_prob_opts(
  asymptomatic = 0,
  presymptomatic_transmission = 0.15,
  symptomatic_ascertained = 0
)

# generate initial cases
case_data <- outbreak_setup(
  initial_cases = 5,
  delays = delays,
  event_probs = event_probs
)
case_data
#> Index: <asymptomatic>
#>    exposure asymptomatic caseid infector missed    onset new_cases
#>       <num>       <lgcl>  <int>    <num> <lgcl>    <num>     <int>
#> 1:        0        FALSE      1        0   TRUE 5.494867        NA
#> 2:        0        FALSE      2        0   TRUE 4.273768        NA
#> 3:        0        FALSE      3        0   TRUE 9.144926        NA
#> 4:        0        FALSE      4        0   TRUE 4.549269        NA
#> 5:        0        FALSE      5        0   TRUE 5.327302        NA
#>    isolated_time sampled
#>            <num>  <lgcl>
#> 1:     13.630713   FALSE
#> 2:     12.000550   FALSE
#> 3:      9.839796   FALSE
#> 4:      7.850404   FALSE
#> 5:      7.479154   FALSE
```
