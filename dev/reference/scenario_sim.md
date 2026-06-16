# Run a specified number of simulations with identical parameters

Run a specified number of simulations with identical parameters

## Usage

``` r
scenario_sim(
  n,
  initial_cases,
  offspring,
  delays,
  event_probs,
  interventions,
  sim
)
```

## Arguments

- n:

  a positive `integer` scalar: number of simulations to run

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

1.  `$outbreak_ts`: the results for multiple simulations using the same
    set of parameters. The `data.table` has columns:

    - `sim`: the simulation replicate index (`integer`)

    - `week`: the week in the simulation (`integer`)

    - `weekly_cases`: the number of new cases that week (`integer`)

    - `cumulative`: the cumulative cases (`integer`)

2.  `$outbreak_stats`: the summary statistics for each outbreak
    simulation replicate using the same set of parameters. The
    `data.table` has columns:

    - `sim`: the simulation replicate index (`integer`)

    - `effective_r0`: the effective reproduction rate for the whole
      simulation (`numeric`)

    - `cases_per_gen`: the cases per generation (`list`)

The `$outbreak_ts` element also carries two attributes used by
[`extinct_prob()`](https://epiforecasts.io/ringbp/dev/reference/extinction.md)
and
[`detect_extinct()`](https://epiforecasts.io/ringbp/dev/reference/extinction.md):
`extinct`, a `logical` vector recording whether each replicate went
extinct, and `cap_cases`, the maximum number of cases used to cap each
simulation.

## Examples

``` r
offspring <- offspring_opts(
  community = \(n) rnbinom(n = n, mu = 2.5, size = 0.16),
  isolated = \(n) rnbinom(n = n, mu = 0, size = 1),
  asymptomatic = \(n) rnbinom(n = n, mu = 2.5, size = 0.16)
)
delays <- delay_opts(
  incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
  onset_to_isolation = \(n) rweibull(n = n, shape = 2.5, scale = 5)
)
event_probs <- event_prob_opts(
  asymptomatic = 0,
  presymptomatic_transmission = 0.3,
  symptomatic_traced = 0
)
interventions <- intervention_opts(quarantine = TRUE)
sim <- sim_opts(
  cap_max_days = 365,
  cap_cases = 2000
)
res <- scenario_sim(
  n = 5,
  initial_cases = 5,
  offspring = offspring,
  delays = delays,
  event_probs = event_probs,
  interventions = interventions,
  sim = sim
)
res
#> $outbreak_ts
#>        sim  week weekly_cases cumulative
#>      <int> <int>        <int>      <int>
#>   1:     1     0            4          4
#>   2:     1     1           17         21
#>   3:     1     2           36         57
#>   4:     1     3          198        255
#>   5:     1     4          488        743
#>  ---                                    
#> 261:     5    48            0       2860
#> 262:     5    49            0       2860
#> 263:     5    50            0       2860
#> 264:     5    51            0       2860
#> 265:     5    52            0       2860
#> 
#> $outbreak_stats
#>      sim effective_r0                  cases_per_gen
#>    <int>        <num>                         <list>
#> 1:     1     2.820762    18,  30, 138, 382, 832,1757
#> 2:     2     0.000000                              0
#> 3:     3     2.759802    20,  51, 124, 285, 901,1908
#> 4:     4     3.517917     6,  62, 187, 402, 921,1950
#> 5:     5     2.609450  33, 43, 96,136,332,700,...[7]
#> 
```
