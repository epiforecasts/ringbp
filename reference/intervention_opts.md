# Create a list of intervention settings to run the ringbp model

Create a list of intervention settings to run the ringbp model

## Usage

``` r
intervention_opts(quarantine = FALSE, test_sensitivity = 1)
```

## Arguments

- quarantine:

  a `logical` scalar: whether quarantine is in effect, if `TRUE` then
  traced contacts are isolated before symptom onset; defaults to `FALSE`

- test_sensitivity:

  a `numeric` scalar probability (between 0 and 1 inclusive): the test
  sensitivity (i.e. probability that a true positive tests positive).
  Individuals that are tested and get a false negative are not isolated
  and their contacts are not traced. Only symptomatic individuals are
  tested. Default is 1, which assumes all symptomatic individuals tested
  get a positive test result.

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
