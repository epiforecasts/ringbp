# Cross-check `*_opts()` lists to run the ringbp model

Cross-check `*_opts()` lists to run the ringbp model

## Usage

``` r
cross_check_opts(delays, event_probs)
```

## Arguments

- delays:

  a `list` with class `<ringbp_delay_opts>`: the delay distribution
  `function`s for the ringbp model, returned by
  [`delay_opts()`](https://epiforecasts.io/ringbp/reference/delay_opts.md).
  Contains 4 elements: `incubation_period`, `onset_to_isolation`,
  `latent_period` and `onset_to_self_isolation`

- event_probs:

  a `list` with class `<ringbp_event_prob_opts>`: the event
  probabilities for the ringbp model, returned by
  [`event_prob_opts()`](https://epiforecasts.io/ringbp/reference/event_prob_opts.md).
  Contains 5 elements: `asymptomatic`, `presymptomatic_transmission`,
  `alpha`, `symptomatic_traced` and `symptomatic_self_isolate`

## Value

`TRUE` if all the checks pass or an error or warning is thrown if the
simulation options are incompatible.

## Details

Currently the only cross-checking is between `onset_to_self_isolation`
from
[`delay_opts()`](https://epiforecasts.io/ringbp/reference/delay_opts.md)
and `symptomatic_self_isolate` from
[`event_prob_opts()`](https://epiforecasts.io/ringbp/reference/event_prob_opts.md).

If `delays$onset_to_self_isolation` carries a `cross_checked` attribute
set to `TRUE`, the checks are skipped and `TRUE` is returned
immediately. This lets
[`scenario_sim()`](https://epiforecasts.io/ringbp/reference/scenario_sim.md)
cross-check the options once and then tag `delays` so that the
per-replicate
[`outbreak_model()`](https://epiforecasts.io/ringbp/reference/outbreak_model.md)
calls do not repeat the check (and re-emit its warning) for every
simulation replicate. The attribute is set on a local copy of `delays`
(a single
[`scenario_sim()`](https://epiforecasts.io/ringbp/reference/scenario_sim.md)
or
[`outbreak_model()`](https://epiforecasts.io/ringbp/reference/outbreak_model.md))
call), and does not persist, so the warning will be repeated in separate
[`scenario_sim()`](https://epiforecasts.io/ringbp/reference/scenario_sim.md)
or
[`outbreak_model()`](https://epiforecasts.io/ringbp/reference/outbreak_model.md))
calls.
