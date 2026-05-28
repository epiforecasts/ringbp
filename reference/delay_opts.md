# Create a list of delay distributions to run the ringbp model

Create a list of delay distributions to run the ringbp model

## Usage

``` r
delay_opts(
  incubation_period,
  onset_to_isolation,
  latent_period = 0,
  onset_to_self_isolation = function(n) rep(Inf, n)
)
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

- onset_to_self_isolation:

  a `function`: a random number generating `function` that samples from
  the onset-to-self-isolation distribution, the `function` accepts a
  single `integer` argument specifying the length of the `function`
  output.

  By default `onset_to_self_isolation` is a function that generates
  `Inf` (i.e. individuals never self-isolate). A different
  onset-to-self-isolation `function` only needs to be specified if a
  non-zero value is specified to `symptomatic_self_isolate` in
  [`event_prob_opts()`](https://epiforecasts.io/ringbp/reference/event_prob_opts.md)
  (which by default is 0). If `onset_to_self_isolation` is specified but
  `symptomatic_self_isolate` is zero, a warning will be thrown and the
  `onset_to_self_isolation` will be ignored; if
  `symptomatic_self_isolate` is non-zero and `onset_to_self_isolation`
  is an `Inf` generating function,
  [`scenario_sim()`](https://epiforecasts.io/ringbp/reference/scenario_sim.md)
  will error.

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
#> <environment: 0x557e26a2ad48>
#> 
#> $onset_to_isolation
#> function (n) 
#> rweibull(n = n, shape = 1.65, scale = 4.28)
#> <environment: 0x557e26a2ad48>
#> 
#> $latent_period
#> [1] 0
#> 
#> $onset_to_self_isolation
#> function (n) 
#> rep(Inf, n)
#> <environment: 0x557e26a453d8>
#> 
#> attr(,"class")
#> [1] "ringbp_delay_opts"
```
