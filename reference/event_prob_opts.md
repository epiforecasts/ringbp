# Create a list of event probabilities to run the ringbp model

Create a list of event probabilities to run the ringbp model

## Usage

``` r
event_prob_opts(
  asymptomatic,
  presymptomatic_transmission,
  symptomatic_traced,
  symptomatic_self_isolate = 0
)
```

## Arguments

- asymptomatic:

  a `numeric` scalar probability (between 0 and 1 inclusive): proportion
  of cases that are completely asymptomatic (subclinical)

- presymptomatic_transmission:

  a `numeric` scalar probability (between 0 and 1 inclusive): proportion
  of transmission that occurs before symptom onset.

- symptomatic_traced:

  a `numeric` scalar probability (between 0 and 1 inclusive), or a
  `function` of time returning probabilities in `[0, 1]`: proportion of
  infectious contacts ascertained by contact tracing.

  A scalar is treated as a constant contact-tracing probability over the
  whole simulation. A `function` accepts a `numeric` vector of times
  (the contact's exposure time in days since the exposure of the initial
  cases on day 0) and returns a `numeric` vector of probabilities of the
  same length, allowing the contact-tracing probability to vary with
  time. For example, `\(t) ifelse(t < 30, 0, 0.5)` represents a
  contact-tracing programme that activates on day 30 and ascertains 50%
  of contacts.

  Only contacts whose infector is symptomatic are eligible for tracing
  (see
  [`outbreak_step()`](https://epiforecasts.io/ringbp/reference/outbreak_step.md)
  for how isolation times are assigned).

- symptomatic_self_isolate:

  a `numeric` scalar probability (between 0 and 1 inclusive): proportion
  of cases that self-isolate when they become symptomatic. These
  individuals do not get tested and do not require a positive test
  result to enter isolation. Default is 0 (i.e. no infectious
  individuals self-isolate).

  If `symptomatic_self_isolate` is non-zero a random number generating
  `function` needs to be specified in the `onset_to_self_isolation`
  argument in the
  [`delay_opts()`](https://epiforecasts.io/ringbp/reference/delay_opts.md)
  function, otherwise the
  [`scenario_sim()`](https://epiforecasts.io/ringbp/reference/scenario_sim.md)
  will error.

## Value

A `list` with class `<ringbp_event_prob_opts>`.

## Examples

``` r
event_prob_opts(
  asymptomatic = 0.1,
  presymptomatic_transmission = 0.5,
  symptomatic_traced = 0.2
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
#> $symptomatic_traced
#> function (t) 
#> rep(x, length(t))
#> <bytecode: 0x558feff3d538>
#> <environment: 0x558feff3d148>
#> 
#> $symptomatic_self_isolate
#> [1] 0
#> 
#> attr(,"class")
#> [1] "ringbp_event_prob_opts"

# time-varying contact tracing ascertainment: programme activates
# on day 30 and ascertains 50%
event_prob_opts(
  asymptomatic = 0.1,
  presymptomatic_transmission = 0.5,
  symptomatic_traced = \(t) ifelse(t < 30, 0, 0.5)
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
#> $symptomatic_traced
#> function (t) 
#> ifelse(t < 30, 0, 0.5)
#> <environment: 0x558feff2cb00>
#> 
#> $symptomatic_self_isolate
#> [1] 0
#> 
#> attr(,"class")
#> [1] "ringbp_event_prob_opts"
```
