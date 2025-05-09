# ringbp 0.1.2.9999

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
