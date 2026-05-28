# Create a list of intervention settings to run the ringbp model

Create a list of intervention settings to run the ringbp model

## Usage

``` r
intervention_opts(quarantine = FALSE, test_sensitivity = 1)
```

## Arguments

- quarantine:

  a `logical` scalar: whether quarantine is in effect. If `TRUE`, traced
  contacts are isolated when their infector is isolated, regardless of
  their own symptom status (so they may be isolated before symptom
  onset, and asymptomatic traced contacts are isolated too). If `FALSE`,
  only symptomatic traced contacts are isolated, no earlier than their
  own symptom onset. Defaults to `FALSE`

- test_sensitivity:

  a `numeric` scalar probability (between 0 and 1 inclusive): the test
  sensitivity (i.e. probability that a true positive tests positive).
  Only symptomatic individuals that do not self-isolate are tested; a
  false-negative result means the case is not isolated via the testing
  pathway (see
  [`outbreak_step()`](https://epiforecasts.io/ringbp/reference/outbreak_step.md)
  for how isolation times are assigned). Default is 1, which assumes all
  tested individuals get a positive test result.

## Value

A `list` with class `<ringbp_intervention_opts>`.

## Examples

``` r
intervention_opts(quarantine = FALSE)
#> $quarantine
#> [1] FALSE
#> 
#> $test_sensitivity
#> [1] 1
#> 
#> attr(,"class")
#> [1] "ringbp_intervention_opts"
```
