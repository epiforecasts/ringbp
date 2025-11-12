# Changelog

## ringbp 0.1.2.9999

- The package test suite has been improved, adding several new unit
  tests, and increasing the test coverage to 100%. The {testthat}
  edition is incremented to use the 3rd edition. Snapshot (regression)
  tests are added for
  [`outbreak_setup()`](https://epiforecasts.io/ringbp/reference/outbreak_setup.md)
  and
  [`scenario_sim()`](https://epiforecasts.io/ringbp/reference/scenario_sim.md).
  Addresses [\#100](https://github.com/epiforecasts/ringbp/issues/100)
  by [@joshwlambert](https://github.com/joshwlambert) in
  [\#160](https://github.com/epiforecasts/ringbp/issues/160) and
  reviewed by [@pearsonca](https://github.com/pearsonca).

- The lower bound for the generation time returned by
  [`incubation_to_generation_time()`](https://epiforecasts.io/ringbp/reference/incubation_to_generation_time.md)
  is now greater or equal to the latent period (default 0) instead of 1.
  A `latent_period` argument has been added to
  [`delay_opts()`](https://epiforecasts.io/ringbp/reference/delay_opts.md).
  [`incubation_to_generation_time()`](https://epiforecasts.io/ringbp/reference/incubation_to_generation_time.md)
  gains two new arguments: `exposure_time` and `latent_period` to
  prevent a bug where an infectee’s exposure precedes an infector’s
  exposure time. When `latent_period > 0`, generation times are
  left‑truncated at `latent_period`; this can reduce the realised
  presymptomatic transmission proportion, for which a warning is issued
  and the realised value is reported after simulation. Addresses
  [\#124](https://github.com/epiforecasts/ringbp/issues/124) by
  [@joshwlambert](https://github.com/joshwlambert) in
  [\#142](https://github.com/epiforecasts/ringbp/issues/142) and
  reviewed by [@sbfnk](https://github.com/sbfnk) and
  [@pearsonca](https://github.com/pearsonca).

- The outbreak simulation functions
  ([`scenario_sim()`](https://epiforecasts.io/ringbp/reference/scenario_sim.md),
  [`outbreak_model()`](https://epiforecasts.io/ringbp/reference/outbreak_model.md),
  [`outbreak_setup()`](https://epiforecasts.io/ringbp/reference/outbreak_setup.md)
  and
  [`outbreak_step()`](https://epiforecasts.io/ringbp/reference/outbreak_step.md))
  have been refactored to provide a more modular and functional
  interface. New
  [`delay_opts()`](https://epiforecasts.io/ringbp/reference/delay_opts.md),
  [`event_prob_opts()`](https://epiforecasts.io/ringbp/reference/event_prob_opts.md),
  [`intervention_opts()`](https://epiforecasts.io/ringbp/reference/intervention_opts.md),
  [`offspring_opts()`](https://epiforecasts.io/ringbp/reference/offspring_opts.md),
  and
  [`sim_opts()`](https://epiforecasts.io/ringbp/reference/sim_opts.md)
  helper functions are added.
  [`check_dist_func()`](https://epiforecasts.io/ringbp/reference/check_dist_func.md)
  is added and `check_outbreak_input()` removed. The `parameter_sweep()`
  function is removed and converted into a vignette ({purrr} is removed
  as a package dependency). `prop_presymptomatic_to_alpha()` is renamed
  to
  [`presymptomatic_transmission_to_alpha()`](https://epiforecasts.io/ringbp/reference/presymptomatic_transmission_to_alpha.md).
  Addresses [\#65](https://github.com/epiforecasts/ringbp/issues/65),
  [\#91](https://github.com/epiforecasts/ringbp/issues/91) by
  [@joshwlambert](https://github.com/joshwlambert) in
  [\#127](https://github.com/epiforecasts/ringbp/issues/127) and
  reviewed by [@pearsonca](https://github.com/pearsonca) and
  [@sbfnk](https://github.com/sbfnk).

- The `inf_fn()` function has been renamed to
  [`incubation_to_generation_time()`](https://epiforecasts.io/ringbp/reference/incubation_to_generation_time.md)
  and the `k` function argument
  ([`scenario_sim()`](https://epiforecasts.io/ringbp/reference/scenario_sim.md),
  [`outbreak_model()`](https://epiforecasts.io/ringbp/reference/outbreak_model.md),
  [`outbreak_step()`](https://epiforecasts.io/ringbp/reference/outbreak_step.md))
  has been renamed `prop_presymptomatic` (in
  [`scenario_sim()`](https://epiforecasts.io/ringbp/reference/scenario_sim.md)
  and
  [`outbreak_model()`](https://epiforecasts.io/ringbp/reference/outbreak_model.md))
  or `alpha` (in
  [`outbreak_step()`](https://epiforecasts.io/ringbp/reference/outbreak_step.md)).
  The internal `prop_presymptomatic_to_alpha()` function has been added
  to do the conversion from `prop_presymptomatic` to `alpha`. Addresses
  [\#119](https://github.com/epiforecasts/ringbp/issues/119),
  [\#120](https://github.com/epiforecasts/ringbp/issues/120) by
  [@joshwlambert](https://github.com/joshwlambert) in
  [\#123](https://github.com/epiforecasts/ringbp/issues/123) and
  reviewed by [@pearsonca](https://github.com/pearsonca) and
  [@sbfnk](https://github.com/sbfnk).

- Added input checking, including new `check_outbreak_input()` function,
  removed unused argument defaults, consistently ordered arguments in
  functions, and moved function argument documentation to functions that
  use the argument. Addresses
  [\#89](https://github.com/epiforecasts/ringbp/issues/89),
  [\#91](https://github.com/epiforecasts/ringbp/issues/91),
  [\#93](https://github.com/epiforecasts/ringbp/issues/93),
  [\#116](https://github.com/epiforecasts/ringbp/issues/116) by
  [@joshwlambert](https://github.com/joshwlambert) in
  [\#117](https://github.com/epiforecasts/ringbp/issues/117) and
  reviewed by [@pearsonca](https://github.com/pearsonca) and
  [@sbfnk](https://github.com/sbfnk).

- Function argument and variable names are consistently named and styled
  (snake case). Arguments that were abbreviated (e.g. `iso`) now use the
  full word, and arguments that had aliases (e.g. `prop_ascertain` and
  `control_effectiveness`) now use a single name. By
  [@joshwlambert](https://github.com/joshwlambert) in
  [\#112](https://github.com/epiforecasts/ringbp/issues/112) and
  reviewed by [@pearsonca](https://github.com/pearsonca) and
  [@sbfnk](https://github.com/sbfnk).

- Fixed a bug in the implementation of quarantine, where isolation could
  happen later than onset + delay, if this was earlier than the
  isolation time of the infector. By [@sbfnk](https://github.com/sbfnk)
  in [\#107](https://github.com/epiforecasts/ringbp/issues/107) and
  reviewed by [@pearsonca](https://github.com/pearsonca) and
  [@joshwlambert](https://github.com/joshwlambert).

- Changed internal sampling to vector draws in several locations; in
  some cases, this was an optimization, but in others it was a bug fix
  correcting the use of single draw where there should have been many.
  Also incorporates other vectorization changes to improve performance.
  Because of the bug fix, users should expect results to change.
  Addresses issues
  [\#90](https://github.com/epiforecasts/ringbp/issues/90),
  [\#92](https://github.com/epiforecasts/ringbp/issues/92),
  [\#94](https://github.com/epiforecasts/ringbp/issues/94), in
  [\#98](https://github.com/epiforecasts/ringbp/issues/98) by
  [@pearsonca](https://github.com/pearsonca) (now added as a
  contributor) with review by [@sbfnk](https://github.com/sbfnk) and
  [@joshwlambert](https://github.com/joshwlambert).

- The minimum R version required by the {ringbp} package is now
  `>= 4.4.0`. This is due to the dependency on {Matrix}. A GitHub
  actions workflow for R CMD check has been added to check the package
  is valid on the minimum required R version. Addressed in
  [\#85](https://github.com/epiforecasts/ringbp/issues/85) by
  [@joshwlambert](https://github.com/joshwlambert) with review by
  [@sbfnk](https://github.com/sbfnk).

- Parameterisation of delay distributions accepted by the model have
  been generalised in
  [`scenario_sim()`](https://epiforecasts.io/ringbp/reference/scenario_sim.md)
  and
  [`outbreak_model()`](https://epiforecasts.io/ringbp/reference/outbreak_model.md).
  `delay_shape` and `delay_scale` arguments have been replaced by the
  `onset_to_isolation` argument, and an `incubation_period` argument is
  added. Addresses issues
  [\#59](https://github.com/epiforecasts/ringbp/issues/59) and
  [\#63](https://github.com/epiforecasts/ringbp/issues/63) in
  [\#84](https://github.com/epiforecasts/ringbp/issues/84) by
  [@joshwlambert](https://github.com/joshwlambert) with review by
  [@sbfnk](https://github.com/sbfnk) and
  [@pearsonca](https://github.com/pearsonca).

- Function examples are added and updated, and `\dontrun{}` is removed.
  Addresses issue
  [\#27](https://github.com/epiforecasts/ringbp/issues/27) in
  [\#83](https://github.com/epiforecasts/ringbp/issues/83) by
  [@joshwlambert](https://github.com/joshwlambert) with review by
  [@pearsonca](https://github.com/pearsonca).

- Use {roxyglobals} to manage global variables. Addresses issue
  [\#66](https://github.com/epiforecasts/ringbp/issues/66) in
  [\#81](https://github.com/epiforecasts/ringbp/issues/81) by
  [@joshwlambert](https://github.com/joshwlambert) with review by
  [@sbfnk](https://github.com/sbfnk).

- Remove silent printing of returned `data.table`s from
  [`outbreak_model()`](https://epiforecasts.io/ringbp/reference/outbreak_model.md)
  and
  [`scenario_sim()`](https://epiforecasts.io/ringbp/reference/scenario_sim.md)
  by adding `[]` to [`return()`](https://rdrr.io/r/base/function.html).
  Addresses issue
  [\#64](https://github.com/epiforecasts/ringbp/issues/64) in
  [\#80](https://github.com/epiforecasts/ringbp/issues/80) by
  [@joshwlambert](https://github.com/joshwlambert) with review from
  [@sbfnk](https://github.com/sbfnk).

- Function documentation is updated, including function arguments,
  function outputs, inheriting arguments across functions and fixing
  out-of-date references. Addressed issue
  [\#58](https://github.com/epiforecasts/ringbp/issues/58) and
  [\#67](https://github.com/epiforecasts/ringbp/issues/67) in
  [\#78](https://github.com/epiforecasts/ringbp/issues/78) by
  [@joshwlambert](https://github.com/joshwlambert) with review by
  [@sbfnk](https://github.com/sbfnk) and
  [@pearsonca](https://github.com/pearsonca) and issue
  [\#79](https://github.com/epiforecasts/ringbp/issues/79) in
  [\#82](https://github.com/epiforecasts/ringbp/issues/82) by
  [@joshwlambert](https://github.com/joshwlambert) with review by
  [@sbfnk](https://github.com/sbfnk).

## ringbp 0.1.2

- Improved package infrastructure by remove the number of dependencies,
  removing deprecated function calls and reducing clutter in the repo.
  In [\#70](https://github.com/epiforecasts/ringbp/issues/70),
  [\#71](https://github.com/epiforecasts/ringbp/issues/71),
  [\#72](https://github.com/epiforecasts/ringbp/issues/72) and
  [\#73](https://github.com/epiforecasts/ringbp/issues/73) by
  [@sbfnk](https://github.com/sbfnk).

## ringbp 0.1.1

- Added unit tests. By [@timcdlucas](https://github.com/timcdlucas) in
  [\#43](https://github.com/epiforecasts/ringbp/issues/43) and reviewed
  by [@sbfnk](https://github.com/sbfnk).

- Added testing and checking infrastructure using github actions.

## ringbp 0.1.0

- Initial release alongside paper on [Feasibility of controlling
  2019-nCoV outbreaks by isolation of cases and
  contacts](https://doi.org/10.1016/S2214-109X(20)30074-7).
