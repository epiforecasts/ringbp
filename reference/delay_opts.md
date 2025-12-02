# Create a list of delay distributions to run the ringbp model

Create a list of delay distributions to run the ringbp model

## Usage

``` r
delay_opts(incubation_period, onset_to_isolation, latent_period = 0)
```

## Arguments

- incubation_period:

  a `function`: a random number generating `function` that samples from
  incubation period distribution, the `function` accepts a single
  `integer` argument specifying the number of times to sample the
  incubation period (i.e. length of the `function` output).

- onset_to_isolation:

  a `function`: a random number generating `function` that accepts a
  single `integer` argument specifying the length of the `function`
  output.

- latent_period:

  a non-negative `numeric` scalar: the minimum time between an
  individual being exposed and becoming infectious. It is a
  population-wide parameter, with no variability between individuals. It
  sets the minimum generation time in the model. Default is 0 (i.e. an
  individual becomes immediately infectious after being infected).

  If `latent_period` is positive then generation times are sampled
  conditional on `gt >= latent_period` (i.e. left-truncated at
  `latent_period`). This may reduce the realised proportion of
  presymptomatic transmission, depending on the `incubation_period`
  distribution and `presymptomatic_transmission` (in
  [`event_prob_opts()`](https://epiforecasts.io/ringbp/reference/event_prob_opts.md)).

## Value

A `list` with class `<ringbp_delay_opts>`.

## Examples

``` r
delay_opts(
  incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
  onset_to_isolation = \(n) rweibull(n = n, shape = 1.65, scale = 4.28)
)
#> $incubation_period
#> function (n) 
#> rweibull(n = n, shape = 2.32, scale = 6.49)
#> <environment: 0x555c4a057700>
#> 
#> $onset_to_isolation
#> function (n) 
#> rweibull(n = n, shape = 1.65, scale = 4.28)
#> <environment: 0x555c4a057700>
#> 
#> $latent_period
#> [1] 0
#> 
#> attr(,"class")
#> [1] "ringbp_delay_opts"
```
