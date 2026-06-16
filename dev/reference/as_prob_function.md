# Coerce a probability input to a probability generating [function](https://rdrr.io/r/base/function.html).

Coerce a probability input to a probability generating
[function](https://rdrr.io/r/base/function.html).

## Usage

``` r
as_prob_function(x)
```

## Arguments

- x:

  An R object.

## Value

A `numeric` generating `function`

## Details

Used to create time-constant functions from `numeric` scalar inputs in
`*_opts()` functions so that users can provide simple number inputs but
the internal outbreak simulation can use time-varying functions that
generate probabilities.

This function also contains the input checking to ensure valid `numeric`
scalar probabilities or `numeric` generating functions are provided.
Functions input to `as_prob_function()` are input checked and then
returned.
