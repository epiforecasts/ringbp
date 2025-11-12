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

A `data.table` object returning the results for multiple simulations
using the same set of parameters. The table has columns

- week: The week in the simulation.

- weekly_cases: The number of new cases that week.

- cumulative: The cumulative cases.

- effective_r0: The effective reproduction rate for the whole simulation

- cases_per_gen: A list column with the cases per generation. This is
  repeated each row.

- sim: Index column for which simulation.

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
  symptomatic_ascertained = 0
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
#>        sim  week weekly_cases cumulative effective_r0            cases_per_gen
#>      <int> <num>        <num>      <num>        <num>                   <list>
#>   1:     1     0            5          5     0.000000                        0
#>   2:     1     1            0          5     0.000000                        0
#>   3:     1     2            0          5     0.000000                        0
#>   4:     1     3            0          5     0.000000                        0
#>   5:     1     4            0          5     0.000000                        0
#>  ---                                                                          
#> 261:     5    48            0       3200     4.673659   67,  88, 360, 806,1874
#> 262:     5    49            0       3200     4.673659   67,  88, 360, 806,1874
#> 263:     5    50            0       3200     4.673659   67,  88, 360, 806,1874
#> 264:     5    51            0       3200     4.673659   67,  88, 360, 806,1874
#> 265:     5    52            0       3200     4.673659   67,  88, 360, 806,1874
```
