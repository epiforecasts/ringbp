# Estimate skew normal alpha parameter from proportion of presymptomatic transmission

Estimate skew normal alpha parameter from proportion of presymptomatic
transmission

## Usage

``` r
presymptomatic_transmission_to_alpha(presymptomatic_transmission)
```

## Arguments

- presymptomatic_transmission:

  a `numeric` scalar probability (between 0 and 1 inclusive): proportion
  of transmission that occurs before symptom onset.

## Value

A `numeric` scalar: The `$minimum` output from
[`optimise()`](https://rdrr.io/r/stats/optimize.html) to find the best
`alpha` parameter to get the desired proportion of presymptomatic
transmission.

## Details

Since there isn't any analytical expression for linking the two, the
value of alpha that corresponds to the given proportion presymptomatic
is obtained via numeric optimisation.
