# Create a list of intervention settings to run the ringbp model

Create a list of intervention settings to run the ringbp model

## Usage

``` r
intervention_opts(quarantine = FALSE)
```

## Arguments

- quarantine:

  a `logical` scalar: whether quarantine is in effect, if `TRUE` then
  traced contacts are isolated before symptom onset; defaults to `FALSE`

## Value

A `list` with class `<ringbp_intervention_opts>`.

## Examples

``` r
intervention_opts(quarantine = FALSE)
#> $quarantine
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "ringbp_intervention_opts"
```
