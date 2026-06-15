# Changelog

## ringbp 1.0.0

- Index cases in
  [`outbreak_setup()`](https://epiforecasts.io/ringbp/reference/outbreak_setup.md)
  are now only isolated if they test positive (using the
  `test_sensitivity` parameter), as opposed to previously where all
  symptomatic index cases were isolated. The updated
  [`outbreak_setup()`](https://epiforecasts.io/ringbp/reference/outbreak_setup.md)
  matches the testing pathway in
  [`outbreak_step()`](https://epiforecasts.io/ringbp/reference/outbreak_step.md).
  [`outbreak_setup()`](https://epiforecasts.io/ringbp/reference/outbreak_setup.md)
  gains an `interventions` argument. Addresses
  [\#231](https://github.com/epiforecasts/ringbp/issues/231) by
  [@joshwlambert](https://github.com/joshwlambert) in
  [\#232](https://github.com/epiforecasts/ringbp/issues/232) and
  reviewed by [@pearsonca](https://github.com/pearsonca).

- Interventions can now activate after a delay rather than being active
  from the start of the outbreak. The `symptomatic_traced` argument of
  [`event_prob_opts()`](https://epiforecasts.io/ringbp/reference/event_prob_opts.md)
  and the `test_sensitivity` argument of
  [`intervention_opts()`](https://epiforecasts.io/ringbp/reference/intervention_opts.md)
  can now accept a `function` of time returning probabilities as well as
  a `numeric` scalar (constant over the simulation, as before). The
  function is evaluated at the contact’s exposure time
  (`symptomatic_traced`) or the case’s symptom onset time
  (`test_sensitivity`) in
  [`outbreak_step()`](https://epiforecasts.io/ringbp/reference/outbreak_step.md),
  so contact tracing and testing can vary over time. A new internal
  [`as_prob_function()`](https://epiforecasts.io/ringbp/reference/as_prob_function.md)
  coerces scalar inputs to constant functions. Addresses
  [\#229](https://github.com/epiforecasts/ringbp/issues/229) by
  [@joshwlambert](https://github.com/joshwlambert) in
  [\#230](https://github.com/epiforecasts/ringbp/issues/230) and
  reviewed by [@pearsonca](https://github.com/pearsonca).

- The time-based stopping criterion in
  [`outbreak_continue()`](https://epiforecasts.io/ringbp/reference/outbreak_continue.md)
  now uses the earliest exposure time of unprocessed cases rather than
  the latest symptom onset, so long incubation periods no longer end the
  simulation prematurely and changing `cap_max_days` no longer alters
  in-window case counts. Addresses
  [\#163](https://github.com/epiforecasts/ringbp/issues/163) and
  [\#225](https://github.com/epiforecasts/ringbp/issues/225) by
  [@joshwlambert](https://github.com/joshwlambert) in
  [\#228](https://github.com/epiforecasts/ringbp/issues/228) and
  reviewed by [@pearsonca](https://github.com/pearsonca).

- Symptomatic individuals can now self-isolate without requiring a
  positive test.
  [`event_prob_opts()`](https://epiforecasts.io/ringbp/reference/event_prob_opts.md)
  gains a `symptomatic_self_isolate` argument (the proportion of
  symptomatic cases that self-isolate, default `0`) and
  [`delay_opts()`](https://epiforecasts.io/ringbp/reference/delay_opts.md)
  gains an `onset_to_self_isolation` argument, (by default a function
  generating `Inf` so that no individual self-isolates). A new internal
  [`cross_check_opts()`](https://epiforecasts.io/ringbp/reference/cross_check_opts.md)
  checks that the
  [`delay_opts()`](https://epiforecasts.io/ringbp/reference/delay_opts.md)
  and
  [`event_prob_opts()`](https://epiforecasts.io/ringbp/reference/event_prob_opts.md)
  settings are mutually compatible.

  In
  [`outbreak_step()`](https://epiforecasts.io/ringbp/reference/outbreak_step.md)
  a case’s isolation time is now the earliest of three pathways:
  self-isolation, a positive test, and contact tracing. Tracing under
  `quarantine` is now exposure-based, so all traced contacts, including
  asymptomatic ones, are isolated when their infector is isolated
  regardless of symptom status (previously asymptomatic cases were never
  isolated);
  [`sample_offspring()`](https://epiforecasts.io/ringbp/reference/sample_offspring.md)
  reduces post-isolation transmission for asymptomatic cases
  accordingly. Addresses
  [\#212](https://github.com/epiforecasts/ringbp/issues/212) and
  [\#227](https://github.com/epiforecasts/ringbp/issues/227) by
  [@joshwlambert](https://github.com/joshwlambert) in
  [\#213](https://github.com/epiforecasts/ringbp/issues/213) and
  reviewed by [@pearsonca](https://github.com/pearsonca) and
  [@sbfnk](https://github.com/sbfnk).

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

- Updated package maintainer to
  [@joshwlambert](https://github.com/joshwlambert). By
  [@joshwlambert](https://github.com/joshwlambert) in
  [\#223](https://github.com/epiforecasts/ringbp/issues/223) and
  reviewed by [@pearsonca](https://github.com/pearsonca) and
  [@sbfnk](https://github.com/sbfnk).

- Added vignettes on:

  - Getting started vignette (`ringbp.Rmd`), containing an overview of
    the [ringbp](https://epiforecasts.io/ringbp/) model and how to
    parameterise it, with an explanation of the model parameters, as
    well as how to plot the outbreak, and summarise the results
    (e.g. [`extinct_prob()`](https://epiforecasts.io/ringbp/reference/extinction.md)).
    There is also a simplified version of the Hellewell et al. analysis.
    Addresses [\#139](https://github.com/epiforecasts/ringbp/issues/139)
    by [@joshwlambert](https://github.com/joshwlambert) in
    [\#201](https://github.com/epiforecasts/ringbp/issues/201) and
    reviewed by [@sbfnk](https://github.com/sbfnk) and
    [@pearsonca](https://github.com/pearsonca).
  - Epidemic model description vignette (`ringbp-model.Rmd`), containing
    model schematic figures which describe and illustrate how disease
    transmission and interventions (isolation, contact tracing and
    quarantine) are structured in the
    [ringbp](https://epiforecasts.io/ringbp/) model. The vignette also
    links the epidemiological parameters that control the interventions,
    disease transmissibility and delay distributions with the R code for
    parameterising a [ringbp](https://epiforecasts.io/ringbp/)
    simulation. Addresses
    [\#140](https://github.com/epiforecasts/ringbp/issues/140) by
    [@joshwlambert](https://github.com/joshwlambert) in
    [\#188](https://github.com/epiforecasts/ringbp/issues/188) and
    reviewed by [@sbfnk](https://github.com/sbfnk) and
    [@pearsonca](https://github.com/pearsonca).
  - A vignette running [ringbp](https://epiforecasts.io/ringbp/) across
    a parameter set (`parameter-sweep.Rmd`) replaces the
    `parameter_sweep()` function. Addresses
    [\#86](https://github.com/epiforecasts/ringbp/issues/86),
    [\#125](https://github.com/epiforecasts/ringbp/issues/125) by
    [@joshwlambert](https://github.com/joshwlambert) in
    [\#127](https://github.com/epiforecasts/ringbp/issues/127) and
    reviewed by [@pearsonca](https://github.com/pearsonca) and
    [@sbfnk](https://github.com/sbfnk).

- Added `CONTRIBUTING.md`. Addresses
  [\#109](https://github.com/epiforecasts/ringbp/issues/109) and
  [\#203](https://github.com/epiforecasts/ringbp/issues/203) by
  [@joshwlambert](https://github.com/joshwlambert) in
  [\#199](https://github.com/epiforecasts/ringbp/issues/199) and
  reviewed by [@pearsonca](https://github.com/pearsonca).

- Improved weekly aggregation in
  [`outbreak_model()`](https://epiforecasts.io/ringbp/reference/outbreak_model.md).
  Addresses [\#168](https://github.com/epiforecasts/ringbp/issues/168)
  by [@joshwlambert](https://github.com/joshwlambert) in
  [\#197](https://github.com/epiforecasts/ringbp/issues/197) and
  reviewed by [@sbfnk](https://github.com/sbfnk).

- Added test sensitivity to outbreak model
  ([\#176](https://github.com/epiforecasts/ringbp/issues/176)) and
  renamed `symptomatic_ascertained` to `symptomatic_traced`
  ([\#208](https://github.com/epiforecasts/ringbp/issues/208)) by
  [@joshwlambert](https://github.com/joshwlambert) in
  [\#196](https://github.com/epiforecasts/ringbp/issues/196) and
  reviewed by [@sbfnk](https://github.com/sbfnk) and
  [@pearsonca](https://github.com/pearsonca).

- The package test suite has been improved, adding several new unit
  tests, and increasing the test coverage to 100%. The
  [testthat](https://testthat.r-lib.org) edition is incremented to use
  the 3rd edition. Snapshot (regression) tests are added for
  [`outbreak_setup()`](https://epiforecasts.io/ringbp/reference/outbreak_setup.md)
  and
  [`scenario_sim()`](https://epiforecasts.io/ringbp/reference/scenario_sim.md).
  Addresses [\#100](https://github.com/epiforecasts/ringbp/issues/100),
  [\#178](https://github.com/epiforecasts/ringbp/issues/178),
  [\#181](https://github.com/epiforecasts/ringbp/issues/181) by
  [@joshwlambert](https://github.com/joshwlambert) in
  [\#160](https://github.com/epiforecasts/ringbp/issues/160) and
  [\#182](https://github.com/epiforecasts/ringbp/issues/182) and
  reviewed by [@pearsonca](https://github.com/pearsonca)
  ([\#160](https://github.com/epiforecasts/ringbp/issues/160)) and
  [@sbfnk](https://github.com/sbfnk)
  ([\#182](https://github.com/epiforecasts/ringbp/issues/182)).

- Fixed per-generation offspring distribution sampling. Previously,
  offspring counts were redrawn for every infector at every call to
  [`outbreak_step()`](https://epiforecasts.io/ringbp/reference/outbreak_step.md),
  so the same infector could generate offspring across multiple
  generations. Offspring are now drawn once per infector via a new
  internal
  [`sample_offspring()`](https://epiforecasts.io/ringbp/reference/sample_offspring.md)
  function, and the `isolated` column in `case_data` is renamed to
  `sampled` to reflect that the infector has had their chance to infect.
  `new_cases` is now an integer. Because of the bug fix, users should
  expect simulation results to change. Addresses
  [\#148](https://github.com/epiforecasts/ringbp/issues/148) by
  [@joshwlambert](https://github.com/joshwlambert) in
  [\#169](https://github.com/epiforecasts/ringbp/issues/169) and
  reviewed by [@sbfnk](https://github.com/sbfnk).

- Enhanced interaction between simulation and extinction functionality:

  - [`outbreak_model()`](https://epiforecasts.io/ringbp/reference/outbreak_model.md)
    and
    [`scenario_sim()`](https://epiforecasts.io/ringbp/reference/scenario_sim.md)
    return the extinction status as an attribute (`extinct`) of the
    `data.table`. This removes the need to calculate extinction from
    weekly case data in
    [`extinct_prob()`](https://epiforecasts.io/ringbp/reference/extinction.md)
    and
    [`detect_extinct()`](https://epiforecasts.io/ringbp/reference/extinction.md).
    Extinction calculation code still exists in
    [`extinct_prob()`](https://epiforecasts.io/ringbp/reference/extinction.md)
    and
    [`detect_extinct()`](https://epiforecasts.io/ringbp/reference/extinction.md)
    for backwards compatibility and if the attribute is dropped.
  - Additionally,
    [`scenario_sim()`](https://epiforecasts.io/ringbp/reference/scenario_sim.md)
    returns a `cap_cases` attribute with the `data.table` and the
    `cap_cases` argument is removed from
    [`extinct_prob()`](https://epiforecasts.io/ringbp/reference/extinction.md)
    and
    [`detect_extinct()`](https://epiforecasts.io/ringbp/reference/extinction.md).
  - The `outbreak_df_week` argument in
    [`extinct_prob()`](https://epiforecasts.io/ringbp/reference/extinction.md)
    and
    [`detect_extinct()`](https://epiforecasts.io/ringbp/reference/extinction.md)
    is renamed to `scenario` and the `week_range` argument is renamed to
    `extinction_week`. The `extinction_week` argument can now accept: 1)
    an integer scalar, 2) an integer vector of length 2 with the lower
    and upper bounds of the extinction window, and 3) an integer vector
    of length *n*. The integer scalar is used as a cutoff for
    determining if extinction has occurred by the cutoff. The integer
    vectors are used as extinction windows, if users want to examine if
    extinction has occurred before a time that is not until the end of
    the outbreak. The new default for `extinction_week` argument is
    `max(scenario$week) - 1` which means the last 2 weeks of the
    outbreak are used as the extinction window.
  - A check is added to
    [`detect_extinct()`](https://epiforecasts.io/ringbp/reference/extinction.md)
    to ensure the `extinction_week` is within the simulated outbreak.

  Addresses [\#132](https://github.com/epiforecasts/ringbp/issues/132)
  by [@joshwlambert](https://github.com/joshwlambert) in
  [\#161](https://github.com/epiforecasts/ringbp/issues/161) and
  reviewed by [@pearsonca](https://github.com/pearsonca) and
  [@sbfnk](https://github.com/sbfnk).

- Upgraded the pkgdown website to Bootstrap v5. Addresses
  [\#131](https://github.com/epiforecasts/ringbp/issues/131) by
  [@joshwlambert](https://github.com/joshwlambert) in
  [\#143](https://github.com/epiforecasts/ringbp/issues/143) and
  reviewed by [@pearsonca](https://github.com/pearsonca).

- Added dependabot by [@sbfnk](https://github.com/sbfnk) in
  [\#154](https://github.com/epiforecasts/ringbp/issues/154).

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

- The
  [`outbreak_continue()`](https://epiforecasts.io/ringbp/reference/outbreak_continue.md)
  function is added to house the simulation loop logic. Addresses
  [\#133](https://github.com/epiforecasts/ringbp/issues/133) by
  [@joshwlambert](https://github.com/joshwlambert) in
  [\#150](https://github.com/epiforecasts/ringbp/issues/150) and
  reviewed by [@pearsonca](https://github.com/pearsonca) and
  [@sbfnk](https://github.com/sbfnk).

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
  is added. The `parameter_sweep()` function is removed and converted
  into a vignette ([purrr](https://purrr.tidyverse.org/) is removed as a
  package dependency). `prop_presymptomatic_to_alpha()` is renamed to
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

- Added input checking, removed unused argument defaults, consistently
  ordered arguments in functions, and moved function argument
  documentation to functions that use the argument. Addresses
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

- The minimum R version required by the
  [ringbp](https://epiforecasts.io/ringbp/) package is now `>= 4.4.0`.
  This is due to the dependency on
  [Matrix](https://Matrix.R-forge.R-project.org). A GitHub actions
  workflow for R CMD check has been added to check the package is valid
  on the minimum required R version. Addressed in
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

- Use [roxyglobals](https://github.com/anthonynorth/roxyglobals) to
  manage global variables. Addresses issue
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
