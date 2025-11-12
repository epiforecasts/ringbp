# Calculate whether outbreaks went extinct or not

Calculate whether outbreaks went extinct or not

## Usage

``` r
detect_extinct(outbreak_df_week, cap_cases, week_range = 12:16)
```

## Arguments

- outbreak_df_week:

  a `data.table`: weekly cases produced by the outbreak model

- cap_cases:

  a positive `integer` scalar: number of cumulative cases at which the
  branching process (simulation) was terminated

- week_range:

  a positive `integer` vector: giving the (zero indexed) week range to
  test for whether an extinction occurred. Default is `12:16`.

## Value

A `data.table`, with two columns `sim` and `extinct`, for a binary
classification of whether the outbreak went extinct in each simulation
replicate. `1` is an outbreak that went extinct, `0` if not.

## Details

The `cap_cases` argument should be equal to the value supplied to
[`outbreak_model()`](https://epiforecasts.io/ringbp/reference/outbreak_model.md)
(possibly passed from
[`scenario_sim()`](https://epiforecasts.io/ringbp/reference/scenario_sim.md)).

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
    symptomatic_ascertained = 0.2
  ),
  interventions = intervention_opts(quarantine = FALSE),
  sim = sim_opts(cap_max_days = 350, cap_cases = 4500)
)
detect_extinct(outbreak_df_week = res, cap_cases = 4500)
#>       sim extinct
#>     <int>   <num>
#>  1:     1       1
#>  2:     2       1
#>  3:     3       1
#>  4:     4       1
#>  5:     5       1
#>  6:     6       0
#>  7:     7       0
#>  8:     8       1
#>  9:     9       1
#> 10:    10       0
```
