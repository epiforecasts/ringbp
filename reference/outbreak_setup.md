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

- `$new_cases`: `logical`

- `$isolated_time`: `numeric`

- `$isolated`: `logical`

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
#>    exposure asymptomatic caseid infector isolated missed     onset new_cases
#>       <num>       <lgcl>  <int>    <num>   <lgcl> <lgcl>     <num>    <lgcl>
#> 1:        0        FALSE      1        0    FALSE   TRUE  5.327302        NA
#> 2:        0        FALSE      2        0    FALSE   TRUE 10.248073        NA
#> 3:        0        FALSE      3        0    FALSE   TRUE  9.878893        NA
#> 4:        0        FALSE      4        0    FALSE   TRUE  1.781222        NA
#> 5:        0        FALSE      5        0    FALSE   TRUE  5.395534        NA
#>    isolated_time
#>            <num>
#> 1:      7.479154
#> 2:     11.923502
#> 3:     15.219283
#> 4:      4.397555
#> 5:     18.466834
```
