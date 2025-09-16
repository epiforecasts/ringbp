# ringbp 0.1.2.9999

* The lower bound for the generation time output from `incubation_to_generation_time()` has been changed from 1 to being conditioned to be larger than the latent period. A `latent_period` argument has been added to `delay_opts()` which defaults to 0. `incubation_to_generation_time()` gets two new arguments: `exposure_time` and `latent_period` to prevent a bug where an infectees exposure preceeds an infectors exposure time. Addresses #124 by @joshwlambert in #142 and reviewed by @sbfnk.

* The outbreak simulation functions (`scenario_sim()`, `outbreak_model()`, `outbreak_setup()` and `outbreak_step()`) have been refactored to provide a more modular and functional interface. New `delay_opts()`, `event_prob_opts()`, `intervention_opts()`, `offspring_opts()`, and `sim_opts()` helper functions are added. `check_dist_func()` is added and `check_outbreak_input()` removed. The `parameter_sweep()` function is removed and converted into a vignette ({purrr} is removed as a package dependency). `prop_presymptomatic_to_alpha()` is renamed to `presymptomatic_transmission_to_alpha()`. Addresses #65, #91 by @joshwlambert in #127 and reviewed by @pearsonca and @sbfnk.

* The `inf_fn()` function has been renamed to `incubation_to_generation_time()` and the `k` function argument (`scenario_sim()`, `outbreak_model()`, `outbreak_step()`) has been renamed `prop_presymptomatic` (in `scenario_sim()` and `outbreak_model()`) or `alpha` (in `outbreak_step()`). The internal `prop_presymptomatic_to_alpha()` function has been added to do the conversion from `prop_presymptomatic` to `alpha`. Addresses #119, #120 by @joshwlambert in #123 and reviewed by @pearsonca and @sbfnk.

* Added input checking, including new `check_outbreak_input()` function, removed unused argument defaults, consistently ordered arguments in functions, and moved function argument documentation to functions that use the argument. Addresses #89, #91, #93, #116 by @joshwlambert in #117 and reviewed by @pearsonca and @sbfnk.

* Function argument and variable names are consistently named and styled (snake case). Arguments that were abbreviated (e.g. `iso`) now use the full word, and arguments that had aliases (e.g. `prop_ascertain` and `control_effectiveness`) now use a single name. By @joshwlambert in #112 and reviewed by @pearsonca and @sbfnk. 

* Fixed a bug in the implementation of quarantine, where isolation could happen later than onset + delay, if this was earlier than the isolation time of the infector. By @sbfnk in #107 and reviewed by @pearsonca and @joshwlambert.

* Changed internal sampling to vector draws in several locations; in some cases, this was an optimization, but in others it was a bug fix correcting the use of single draw where there should have been many. Also incorporates other vectorization changes to improve performance. Because of the bug fix, users should expect results to change. Addresses issues #90, #92, #94, in #98 by @pearsonca (now added as a contributor) with review by @sbfnk and @joshwlambert.

* The minimum R version required by the {ringbp} package is now `>= 4.4.0`. This is due to the dependency on {Matrix}. A GitHub actions workflow for R CMD check has been added to check the package is valid on the minimum required R version. Addressed in #85 by @joshwlambert with review by @sbfnk.

* Parameterisation of delay distributions accepted by the model have been generalised in `scenario_sim()` and `outbreak_model()`. `delay_shape` and `delay_scale` arguments have been replaced by the `onset_to_isolation` argument, and an `incubation_period` argument is added. Addresses issues #59 and #63 in #84 by @joshwlambert with review by @sbfnk and @pearsonca.

* Function examples are added and updated, and `\dontrun{}` is removed. Addresses issue #27 in #83 by @joshwlambert with review by @pearsonca.

* Use {roxyglobals} to manage global variables. Addresses issue #66 in #81 by @joshwlambert with review by @sbfnk.

* Remove silent printing of returned `data.table`s from `outbreak_model()` and `scenario_sim()` by adding `[]` to `return()`. Addresses issue #64 in #80 by @joshwlambert with review from @sbfnk. 

* Function documentation is updated, including function arguments, function outputs, inheriting arguments across functions and fixing out-of-date references. Addressed issue #58 and #67 in #78 by @joshwlambert with review by @sbfnk and @pearsonca and issue #79 in #82 by @joshwlambert with review by @sbfnk.

# ringbp 0.1.2

* Improved package infrastructure by remove the number of dependencies, removing deprecated function calls and reducing clutter in the repo. In #70, #71, #72 and #73 by @sbfnk.

# ringbp 0.1.1

* Added unit tests. By @timcdlucas in #43 and reviewed by @sbfnk.

* Added testing and checking infrastructure using github actions.

# ringbp 0.1.0

* Initial release alongside paper on [Feasibility of controlling 2019-nCoV outbreaks by isolation of cases and contacts](https://doi.org/10.1016/S2214-109X(20)30074-7).
