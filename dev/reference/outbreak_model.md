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
  which are all assumed to be missed by contact tracing (i.e. tracing
  ascertainment = 0).

- offspring:

  a `list` with class `<ringbp_offspring_opts>`: the offspring
  distribution `function`s for the ringbp model, returned by
  [`offspring_opts()`](https://epiforecasts.io/ringbp/dev/reference/offspring_opts.md).
  Contains three elements: `community`, `isolated`, and `asymptomatic`

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

- sim:

  a `list` with class `<ringbp_sim_opts>`: the simulation control
  options for the ringbp model, returned by
  [`sim_opts()`](https://epiforecasts.io/ringbp/dev/reference/sim_opts.md)

## Value

A `list` with 2 `data.table` elements:

1.  `$outbreak_ts`: the results for a single outbreak simulation. The
    `data.table` has columns:

    - `week`: the week in the simulation (`integer`)

    - `weekly_cases`: the number of new cases that week (`integer`)

    - `cumulative`: the cumulative cases (`integer`)

2.  `$outbreak_stats`: the summary statistics for the outbreak
    simulation. The `data.table` has columns:

    - `effective_r0`: the effective reproduction rate for the whole
      simulation (`numeric`)

    - `cases_per_gen`: the cases per generation (`list`)

The `$outbreak_ts` element also carries an `extinct` attribute: a
`logical` recording whether the outbreak went extinct. See
[extinction](https://epiforecasts.io/ringbp/dev/reference/extinction.md)
functions for the definition of extinction.

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
  symptomatic_traced = 0.2
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
#> $outbreak_ts
#>      week weekly_cases cumulative
#>     <int>        <int>      <int>
#>  1:     0            2          2
#>  2:     1            0          2
#>  3:     2            0          2
#>  4:     3            0          2
#>  5:     4            0          2
#>  6:     5            0          2
#>  7:     6            0          2
#>  8:     7            0          2
#>  9:     8            0          2
#> 10:     9            0          2
#> 11:    10            0          2
#> 12:    11            0          2
#> 13:    12            0          2
#> 14:    13            0          2
#> 15:    14            0          2
#> 16:    15            0          2
#> 17:    16            0          2
#> 18:    17            0          2
#> 19:    18            0          2
#> 20:    19            0          2
#> 21:    20            0          2
#> 22:    21            0          2
#> 23:    22            0          2
#> 24:    23            0          2
#> 25:    24            0          2
#> 26:    25            0          2
#> 27:    26            0          2
#> 28:    27            0          2
#> 29:    28            0          2
#> 30:    29            0          2
#> 31:    30            0          2
#> 32:    31            0          2
#> 33:    32            0          2
#> 34:    33            0          2
#> 35:    34            0          2
#> 36:    35            0          2
#> 37:    36            0          2
#> 38:    37            0          2
#> 39:    38            0          2
#> 40:    39            0          2
#> 41:    40            0          2
#> 42:    41            0          2
#> 43:    42            0          2
#> 44:    43            0          2
#> 45:    44            0          2
#> 46:    45            0          2
#> 47:    46            0          2
#> 48:    47            0          2
#> 49:    48            0          2
#> 50:    49            0          2
#> 51:    50            0          2
#>      week weekly_cases cumulative
#>     <int>        <int>      <int>
#> 
#> $outbreak_stats
#>    effective_r0 cases_per_gen
#>           <num>        <list>
#> 1:          0.5           1,0
#> 
```
