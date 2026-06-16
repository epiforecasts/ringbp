# Set up initial cases for branching process

Set up initial cases for branching process

## Usage

``` r
outbreak_setup(initial_cases, delays, event_probs, interventions)
```

## Arguments

- initial_cases:

  a non-negative `integer` scalar: number of initial or starting cases
  which are all assumed to be missed by contact tracing (i.e. tracing
  ascertainment = 0).

- delays:

  a `list` with class `<ringbp_delay_opts>`: the delay distribution
  `function`s for the ringbp model, returned by
  [`delay_opts()`](https://epiforecasts.io/ringbp/dev/reference/delay_opts.md).
  Contains 4 elements: `incubation_period`, `onset_to_isolation`,
  `latent_period` and `onset_to_self_isolation`

- event_probs:

  a `list` with class `<ringbp_event_prob_opts>`: the event
  probabilities for the ringbp model, returned by
  [`event_prob_opts()`](https://epiforecasts.io/ringbp/dev/reference/event_prob_opts.md).
  Contains 5 elements: `asymptomatic`, `presymptomatic_transmission`,
  `alpha`, `symptomatic_traced` and `symptomatic_self_isolate`

- interventions:

  a `list` with class `<ringbp_intervention_opts>`: the intervention
  settings for the ringbp model, returned by
  [`intervention_opts()`](https://epiforecasts.io/ringbp/dev/reference/intervention_opts.md).
  Contains 2 elements: `quarantine` and `test_sensitivity`

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
interventions <- intervention_opts()

# generate initial cases
case_data <- outbreak_setup(
  initial_cases = 5,
  delays = delays,
  event_probs = event_probs,
  interventions = interventions
)
case_data
#> Index: <self_isolate__asymptomatic>
#>    exposure asymptomatic caseid infector traced    onset new_cases self_isolate
#>       <num>       <lgcl>  <int>    <num> <lgcl>    <num>     <int>       <lgcl>
#> 1:        0        FALSE      1        0  FALSE 7.382506        NA        FALSE
#> 2:        0        FALSE      2        0  FALSE 3.488553        NA        FALSE
#> 3:        0        FALSE      3        0  FALSE 8.338751        NA        FALSE
#> 4:        0        FALSE      4        0  FALSE 5.577247        NA        FALSE
#> 5:        0        FALSE      5        0  FALSE 5.306659        NA        FALSE
#>    isolated_time sampled
#>            <num>  <lgcl>
#> 1:     11.098008   FALSE
#> 2:     10.795869   FALSE
#> 3:     14.142051   FALSE
#> 4:      6.343947   FALSE
#> 5:     11.410506   FALSE
```
