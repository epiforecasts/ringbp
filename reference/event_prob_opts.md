# Create a list of event probabilities to run the ringbp model

Create a list of event probabilities to run the ringbp model

## Usage

``` r
event_prob_opts(
  asymptomatic,
  presymptomatic_transmission,
  symptomatic_ascertained
)
```

## Arguments

- asymptomatic:

  a `numeric` scalar probability (between 0 and 1 inclusive): proportion
  of cases that are completely asymptomatic (subclinical)

- presymptomatic_transmission:

  a `numeric` scalar probability (between 0 and 1 inclusive): proportion
  of transmission that occurs before symptom onset.

- symptomatic_ascertained:

  a `numeric` scalar probability (between 0 and 1 inclusive): proportion
  of infectious contacts ascertained by contact tracing

## Value

A `list` with class `<ringbp_event_prob_opts>`.

## Examples

``` r
event_prob_opts(
  asymptomatic = 0.1,
  presymptomatic_transmission = 0.5,
  symptomatic_ascertained = 0.2
)
#> $asymptomatic
#> [1] 0.1
#> 
#> $presymptomatic_transmission
#> [1] 0.5
#> 
#> $alpha
#> [1] 1.968077e-08
#> 
#> $symptomatic_ascertained
#> [1] 0.2
#> 
#> attr(,"class")
#> [1] "ringbp_event_prob_opts"
```
