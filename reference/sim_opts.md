# Create a list of simulation control options for the ringbp model

Create a list of simulation control options for the ringbp model

## Usage

``` r
sim_opts(cap_max_days = 350, cap_cases = 5000)
```

## Arguments

- cap_max_days:

  a positive `integer` scalar: stop the simulation when this many days
  is reached.

- cap_cases:

  a positive `integer` scalar: number of cumulative cases at which the
  branching process (simulation) was terminated

## Value

A `list` with class `<ringbp_sim_opts>`.

## Examples

``` r
# default simulation control options
sim_opts()
#> $cap_max_days
#> [1] 350
#> 
#> $cap_cases
#> [1] 5000
#> 
#> attr(,"class")
#> [1] "ringbp_sim_opts"

# specifying custom simulation control options
sim_opts(
  cap_max_days = 140,
  cap_cases = 1000
)
#> $cap_max_days
#> [1] 140
#> 
#> $cap_cases
#> [1] 1000
#> 
#> attr(,"class")
#> [1] "ringbp_sim_opts"
```
