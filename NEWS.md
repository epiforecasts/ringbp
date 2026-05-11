# ringbp 0.1.2.9999

* Added vignettes on:

  - Getting started vignette (`ringbp.Rmd`), containing an overview of the `{ringbp}` model and how to parameterise it, with an explanation of the model parameters, as well as how to plot the outbreak, and summarise the results (e.g. `extinct_prob()`). There is also a simplified version of the Hellewell et al. analysis. Addresses #139 by @joshwlambert in #201 and reviewed by @sbfnk and @pearsonca.
  - Epidemic model description vignette (`ringbp-model.Rmd`), containing model schematic figures which describe and illustrate how disease transmission and interventions (isolation, contact tracing and quarantine) are structured in the `{ringbp}` model. The vignette also links the epidemiological parameters that control the interventions, disease transmissibility and delay distributions with the R code for parameterising a `{ringbp}` simulation. Addresses #140 by @joshwlambert in #188 and reviewed by @sbfnk and @pearsonca.
  - A vignette running `{ringbp}` across a parameter set (`parameter-sweep.Rmd`) replaces the `parameter_sweep()` function. Addresses #86, #125 by @joshwlambert in #127 and reviewed by @pearsonca and @sbfnk.

* Added `CONTRIBUTING.md`. Addresses #109 and #203 by @joshwlambert in #199 and reviewed by @pearsonca.

* Improved weekly aggregation in `outbreak_model()`. Addresses #168 by @joshwlambert in #197 and reviewed by @sbfnk.

* Added test sensitivity to outbreak model (#176) and renamed `symptomatic_ascertained` to `symptomatic_traced` (#208) by @joshwlambert in #196 and reviewed by @sbfnk and @pearsonca.

* The package test suite has been improved, adding several new unit tests, and increasing the test coverage to 100%. The `{testthat}` edition is incremented to use the 3rd edition. Snapshot (regression) tests are added for `outbreak_setup()` and `scenario_sim()`. Addresses #100, #178, #181 by @joshwlambert in #160 and #182 and reviewed by @pearsonca (#160) and @sbfnk (#182).

* Fixed per-generation offspring distribution sampling. Previously, offspring counts were redrawn for every infector at every call to `outbreak_step()`, so the same infector could generate offspring across multiple generations. Offspring are now drawn once per infector via a new internal `sample_offspring()` function, and the `isolated` column in `case_data` is renamed to `sampled` to reflect that the infector has had their chance to infect. `new_cases` is now an integer. Because of the bug fix, users should expect simulation results to change. Addresses #148 by @joshwlambert in #169 and reviewed by @sbfnk.

* Enhanced interaction between simulation and extinction functionality:

  - `outbreak_model()` and `scenario_sim()` return the extinction status as an attribute (`extinct`) of the `data.table`. This removes the need to calculate extinction from weekly case data in `extinct_prob()` and `detect_extinct()`. Extinction calculation code still exists in `extinct_prob()` and `detect_extinct()` for backwards compatibility and if the attribute is dropped.
  - Additionally, `scenario_sim()` returns a `cap_cases` attribute with the `data.table` and the `cap_cases` argument is removed from `extinct_prob()` and `detect_extinct()`.
  - The `outbreak_df_week` argument in `extinct_prob()` and `detect_extinct()` is renamed to `scenario` and the `week_range` argument is renamed to `extinction_week`. The `extinction_week` argument can now accept: 1) an integer scalar, 2) an integer vector of length 2 with the lower and upper bounds of the extinction window, and 3) an integer vector of length _n_. The integer scalar is used as a cutoff for determining if extinction has occurred by the cutoff. The integer vectors are used as extinction windows, if users want to examine if extinction has occurred before a time that is not until the end of the outbreak. The new default for `extinction_week` argument is `max(scenario$week) - 1` which means the last 2 weeks of the outbreak are used as the extinction window.
  - A check is added to `detect_extinct()` to ensure the `extinction_week` is within the simulated outbreak.

  Addresses #132 by @joshwlambert in #161 and reviewed by @pearsonca and @sbfnk.

* Upgraded the pkgdown website to Bootstrap v5. Addresses #131 by @joshwlambert in #143 and reviewed by @pearsonca.

* Added dependabot by @sbfnk in #154.

* The lower bound for the generation time returned by `incubation_to_generation_time()` is now greater or equal to the latent period (default 0) instead of 1. A `latent_period` argument has been added to `delay_opts()`. `incubation_to_generation_time()` gains two new arguments: `exposure_time` and `latent_period` to prevent a bug where an infectee's exposure precedes an infector's exposure time. When `latent_period > 0`, generation times are left‑truncated at `latent_period`; this can reduce the realised presymptomatic transmission proportion, for which a warning is issued and the realised value is reported after simulation. Addresses #124 by @joshwlambert in #142 and reviewed by @sbfnk and @pearsonca.

* The `outbreak_continue()` function is added to house the simulation loop logic. Addresses #133 by @joshwlambert in #150 and reviewed by @pearsonca and @sbfnk.

* The outbreak simulation functions (`scenario_sim()`, `outbreak_model()`, `outbreak_setup()` and `outbreak_step()`) have been refactored to provide a more modular and functional interface. New `delay_opts()`, `event_prob_opts()`, `intervention_opts()`, `offspring_opts()`, and `sim_opts()` helper functions are added. `check_dist_func()` is added and `check_outbreak_input()` removed. The `parameter_sweep()` function is removed and converted into a vignette (`{purrr}` is removed as a package dependency). `prop_presymptomatic_to_alpha()` is renamed to `presymptomatic_transmission_to_alpha()`. Addresses #65, #91 by @joshwlambert in #127 and reviewed by @pearsonca and @sbfnk.

* The `inf_fn()` function has been renamed to `incubation_to_generation_time()` and the `k` function argument (`scenario_sim()`, `outbreak_model()`, `outbreak_step()`) has been renamed `prop_presymptomatic` (in `scenario_sim()` and `outbreak_model()`) or `alpha` (in `outbreak_step()`). The internal `prop_presymptomatic_to_alpha()` function has been added to do the conversion from `prop_presymptomatic` to `alpha`. Addresses #119, #120 by @joshwlambert in #123 and reviewed by @pearsonca and @sbfnk.

* Added input checking, including new `check_outbreak_input()` function, removed unused argument defaults, consistently ordered arguments in functions, and moved function argument documentation to functions that use the argument. Addresses #89, #91, #93, #116 by @joshwlambert in #117 and reviewed by @pearsonca and @sbfnk.

* Function argument and variable names are consistently named and styled (snake case). Arguments that were abbreviated (e.g. `iso`) now use the full word, and arguments that had aliases (e.g. `prop_ascertain` and `control_effectiveness`) now use a single name. By @joshwlambert in #112 and reviewed by @pearsonca and @sbfnk.

* Fixed a bug in the implementation of quarantine, where isolation could happen later than onset + delay, if this was earlier than the isolation time of the infector. By @sbfnk in #107 and reviewed by @pearsonca and @joshwlambert.

* Changed internal sampling to vector draws in several locations; in some cases, this was an optimization, but in others it was a bug fix correcting the use of single draw where there should have been many. Also incorporates other vectorization changes to improve performance. Because of the bug fix, users should expect results to change. Addresses issues #90, #92, #94, in #98 by @pearsonca (now added as a contributor) with review by @sbfnk and @joshwlambert.

* The minimum R version required by the `{ringbp}` package is now `>= 4.4.0`. This is due to the dependency on `{Matrix}`. A GitHub actions workflow for R CMD check has been added to check the package is valid on the minimum required R version. Addressed in #85 by @joshwlambert with review by @sbfnk.

* Parameterisation of delay distributions accepted by the model have been generalised in `scenario_sim()` and `outbreak_model()`. `delay_shape` and `delay_scale` arguments have been replaced by the `onset_to_isolation` argument, and an `incubation_period` argument is added. Addresses issues #59 and #63 in #84 by @joshwlambert with review by @sbfnk and @pearsonca.

* Function examples are added and updated, and `\dontrun{}` is removed. Addresses issue #27 in #83 by @joshwlambert with review by @pearsonca.

* Use `{roxyglobals}` to manage global variables. Addresses issue #66 in #81 by @joshwlambert with review by @sbfnk.

* Remove silent printing of returned `data.table`s from `outbreak_model()` and `scenario_sim()` by adding `[]` to `return()`. Addresses issue #64 in #80 by @joshwlambert with review from @sbfnk.

* Function documentation is updated, including function arguments, function outputs, inheriting arguments across functions and fixing out-of-date references. Addressed issue #58 and #67 in #78 by @joshwlambert with review by @sbfnk and @pearsonca and issue #79 in #82 by @joshwlambert with review by @sbfnk.

# ringbp 0.1.2

* Improved package infrastructure by remove the number of dependencies, removing deprecated function calls and reducing clutter in the repo. In #70, #71, #72 and #73 by @sbfnk.

# ringbp 0.1.1

* Added unit tests. By @timcdlucas in #43 and reviewed by @sbfnk.

* Added testing and checking infrastructure using github actions.

# ringbp 0.1.0

* Initial release alongside paper on [Feasibility of controlling 2019-nCoV outbreaks by isolation of cases and contacts](https://doi.org/10.1016/S2214-109X(20)30074-7).
