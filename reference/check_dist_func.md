# Check a distribution function returns valid output and has the correct number of arguments

Check a distribution function returns valid output and has the correct
number of arguments

## Usage

``` r
check_dist_func(func, dist_name, n_req_args = 1, func_eval_min = 0)
```

## Arguments

- func:

  a [function](https://rdrr.io/r/base/function.html).

- dist_name:

  a `character` string: the name of the distribution function being
  passed (e.g. `"incubation_period"`)

- n_req_args:

  a single `numeric`: the number of required arguments

- func_eval_min:

  a single `numeric`: the lower bound of valid numeric output by the
  `func` argument (i.e. the minimum of the acceptable value in the
  function's codomain). The default is `0` so `func` must return
  non-negative values.

## Value

`TRUE` if all the checks pass or an error is thrown if the distribution
function is invalid.
