# Run a single instance of the branching process model

Run a single instance of the branching process model

## Usage

``` r
outbreak_model(
  initial_cases,
  offspring,
  delays,
  event_probs,
  interventions,
  sim
)
```

## Arguments

- initial_cases:

  a non-negative `integer` scalar: number of initial or starting cases
  which are all assumed to be missed.

- offspring:

  a `list` with class `<ringbp_offspring_opts>`: the offspring
  distribution `function`s for the ringbp model, returned by
  [`offspring_opts()`](https://epiforecasts.io/ringbp/reference/offspring_opts.md).
  Contains three elements: `community`, `isolated`, and `asymptomatic`

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

- interventions:

  a `list` with class `<ringbp_intervention_opts>`: the intervention
  settings for the ringbp model, returned by
  [`intervention_opts()`](https://epiforecasts.io/ringbp/reference/intervention_opts.md).
  Contains one element: `quarantine`

- sim:

  a `list` with class `<ringbp_sim_opts>`: the simulation control
  options for the ringbp model, returned by
  [`sim_opts()`](https://epiforecasts.io/ringbp/reference/sim_opts.md)

## Value

`data.table` of cases by week, cumulative cases, and the effective
reproduction number of the outbreak. `data.table` columns are:

- `$week`: `numeric`

- `$weekly_cases`: `numeric`

- `$cumulative`: `numeric`

- `$effective_r0`: `numeric`

- `$cases_per_gen`: `list`

## Examples

``` r
set.seed(1)
offspring <- offspring_opts(
  community = \(n) rnbinom(n = n, mu = 2.5, size = 0.16),
  isolated = \(n) rnbinom(n = n, mu = 0.5, size = 1),
  asymptomatic = \(n) rnbinom(n = n, mu = 2.5, size = 0.16)
)
delays <- delay_opts(
  incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
  onset_to_isolation = \(n) rweibull(n = n, shape = 1.65, scale = 4.28)
)
event_probs <- event_prob_opts(
  asymptomatic = 0,
  presymptomatic_transmission = 0.5,
  symptomatic_ascertained = 0.2
)
interventions <- intervention_opts(quarantine = FALSE)
out <- outbreak_model(
  initial_cases = 1,
  offspring = offspring,
  delays = delays,
  event_probs = event_probs,
  interventions = interventions,
  sim = sim_opts()
)
out
#>      week weekly_cases cumulative effective_r0 cases_per_gen
#>     <num>        <num>      <num>        <num>        <list>
#>  1:     0            1          1            0             0
#>  2:     1            0          1            0             0
#>  3:     2            0          1            0             0
#>  4:     3            0          1            0             0
#>  5:     4            0          1            0             0
#>  6:     5            0          1            0             0
#>  7:     6            0          1            0             0
#>  8:     7            0          1            0             0
#>  9:     8            0          1            0             0
#> 10:     9            0          1            0             0
#> 11:    10            0          1            0             0
#> 12:    11            0          1            0             0
#> 13:    12            0          1            0             0
#> 14:    13            0          1            0             0
#> 15:    14            0          1            0             0
#> 16:    15            0          1            0             0
#> 17:    16            0          1            0             0
#> 18:    17            0          1            0             0
#> 19:    18            0          1            0             0
#> 20:    19            0          1            0             0
#> 21:    20            0          1            0             0
#> 22:    21            0          1            0             0
#> 23:    22            0          1            0             0
#> 24:    23            0          1            0             0
#> 25:    24            0          1            0             0
#> 26:    25            0          1            0             0
#> 27:    26            0          1            0             0
#> 28:    27            0          1            0             0
#> 29:    28            0          1            0             0
#> 30:    29            0          1            0             0
#> 31:    30            0          1            0             0
#> 32:    31            0          1            0             0
#> 33:    32            0          1            0             0
#> 34:    33            0          1            0             0
#> 35:    34            0          1            0             0
#> 36:    35            0          1            0             0
#> 37:    36            0          1            0             0
#> 38:    37            0          1            0             0
#> 39:    38            0          1            0             0
#> 40:    39            0          1            0             0
#> 41:    40            0          1            0             0
#> 42:    41            0          1            0             0
#> 43:    42            0          1            0             0
#> 44:    43            0          1            0             0
#> 45:    44            0          1            0             0
#> 46:    45            0          1            0             0
#> 47:    46            0          1            0             0
#> 48:    47            0          1            0             0
#> 49:    48            0          1            0             0
#> 50:    49            0          1            0             0
#> 51:    50            0          1            0             0
#>      week weekly_cases cumulative effective_r0 cases_per_gen
#>     <num>        <num>      <num>        <num>        <list>
```
