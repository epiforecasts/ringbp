# Set up initial cases for branching process

Set up initial cases for branching process

## Usage

``` r
outbreak_setup(initial_cases, delays, event_probs)
```

## Arguments

- initial_cases:

  a non-negative `integer` scalar: number of initial or starting cases
  which are all assumed to be missed by contact tracing (i.e. tracing
  ascertainment = 0).

- delays:

  a `list` with class `<ringbp_delay_opts>`: the delay distribution
  `function`s for the ringbp model, returned by
  [`delay_opts()`](https://epiforecasts.io/ringbp/reference/delay_opts.md).
  Contains 4 elements: `incubation_period`, `onset_to_isolation`,
  `latent_period` and `onset_to_self_isolation`

- event_probs:

  a `list` with class `<ringbp_event_prob_opts>`: the event
  probabilities for the ringbp model, returned by
  [`event_prob_opts()`](https://epiforecasts.io/ringbp/reference/event_prob_opts.md).
  Contains 5 elements: `asymptomatic`, `presymptomatic_transmission`,
  `alpha`, `symptomatic_traced` and `symptomatic_self_isolate`

## Value

`data.table` of cases in outbreak so far. `data.table` columns are:

- `$exposure`: `numeric`

- `$asymptomatic`: `logical`

- `$caseid`: `integer`

- `$infector`: `numeric`

- `$traced`: `logical`

- `$onset`: `numeric`

- `$new_cases`: `integer`

- `$self_isolate`: `logical`

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
  symptomatic_traced = 0
)

# generate initial cases
case_data <- outbreak_setup(
  initial_cases = 5,
  delays = delays,
  event_probs = event_probs
)
case_data
#> Index: <asymptomatic>
#>    exposure asymptomatic caseid infector traced     onset new_cases
#>       <num>       <lgcl>  <int>    <num> <lgcl>     <num>     <int>
#> 1:        0        FALSE      1        0  FALSE  6.615255        NA
#> 2:        0        FALSE      2        0  FALSE 10.912476        NA
#> 3:        0        FALSE      3        0  FALSE  5.494867        NA
#> 4:        0        FALSE      4        0  FALSE  4.273768        NA
#> 5:        0        FALSE      5        0  FALSE  9.144926        NA
#>    self_isolate isolated_time sampled
#>          <lgcl>         <num>  <lgcl>
#> 1:        FALSE      9.212328   FALSE
#> 2:        FALSE     14.155064   FALSE
#> 3:        FALSE     13.630713   FALSE
#> 4:        FALSE     12.000550   FALSE
#> 5:        FALSE      9.839796   FALSE
```
