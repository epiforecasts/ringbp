# Create a list of intervention settings to run the ringbp model

Create a list of intervention settings to run the ringbp model

## Usage

``` r
intervention_opts(quarantine = FALSE, test_sensitivity = 1)
```

## Arguments

- quarantine:

  a `logical` scalar: whether quarantine is in effect. If `TRUE`, traced
  contacts are isolated when their infector is isolated, regardless of
  their own symptom status (so they may be isolated before symptom
  onset, and asymptomatic traced contacts are isolated too). If `FALSE`,
  only symptomatic traced contacts are isolated, no earlier than their
  own symptom onset. Defaults to `FALSE`

- test_sensitivity:

  a `numeric` scalar probability (between 0 and 1 inclusive), or a
  `function` of time returning probabilities in `[0, 1]`: the test
  sensitivity (i.e. probability that a true positive tests positive).

  A scalar is treated as a constant test sensitivity over the whole
  simulation. A `function` accepts a `numeric` vector of times (symptom
  onset time in days since the exposure of the initial cases on day 0)
  and returns a `numeric` vector of probabilities of the same length,
  allowing the test sensitivity to vary with time. For example,
  `\(t) ifelse(t < 30, 0, 0.8)` represents a testing programme that
  activates on day 30 with sensitivity 0.8.

  Only symptomatic individuals that do not self-isolate are tested; a
  false-negative result means the case is not isolated via the testing
  pathway (see
  [`outbreak_step()`](https://epiforecasts.io/ringbp/reference/outbreak_step.md)
  for how isolation times are assigned). Default is 1, which assumes all
  tested individuals get a positive test result.

## Value

A `list` with class `<ringbp_intervention_opts>`.

## Examples

``` r
# quarantine is not active (default)
intervention_opts(quarantine = FALSE)
#> $quarantine
#> [1] FALSE
#> 
#> $test_sensitivity
#> function (t) 
#> rep(x, length(t))
#> <bytecode: 0x55ff3bcc2e98>
#> <environment: 0x55ff43fd5438>
#> 
#> attr(,"class")
#> [1] "ringbp_intervention_opts"

# quarantine is active
intervention_opts(quarantine = TRUE)
#> $quarantine
#> [1] TRUE
#> 
#> $test_sensitivity
#> function (t) 
#> rep(x, length(t))
#> <bytecode: 0x55ff3bcc2e98>
#> <environment: 0x55ff43f40100>
#> 
#> attr(,"class")
#> [1] "ringbp_intervention_opts"

# 20% of tests return a false-negative
intervention_opts(test_sensitivity = 0.8)
#> $quarantine
#> [1] FALSE
#> 
#> $test_sensitivity
#> function (t) 
#> rep(x, length(t))
#> <bytecode: 0x55ff3bcc2e98>
#> <environment: 0x55ff43e9a088>
#> 
#> attr(,"class")
#> [1] "ringbp_intervention_opts"

# time-varying test sensitivity, in the first 30 days of the outbreak
# sensitivity is 0.5, then after 30 days, sensitivity improves to 0.8
intervention_opts(
  test_sensitivity = \(t) ifelse(t > 30, yes = 0.8, no = 0.5)
)
#> $quarantine
#> [1] FALSE
#> 
#> $test_sensitivity
#> function (t) 
#> ifelse(t > 30, yes = 0.8, no = 0.5)
#> <environment: 0x55ff44038d60>
#> 
#> attr(,"class")
#> [1] "ringbp_intervention_opts"
```
