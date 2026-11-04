#' Convert symptom onset times to generation times
#'
#' Samples generation times from a skew-normal distribution based on relative
#' symptom onset times (`symptom_onset_time` - `exposure_time`), ensuring all
#' generation times are at least `latent_period`. The location parameter of the
#' skew-normal distribution is set to the relative symptom onset times.
#'
#' @param symptom_onset_time a positive `numeric` vector: symptom onset time(s)
#'   of the infector(s) in the case data. The symptom onset times are generated
#'   by sampling from the incubation period.
#' @param exposure_time a non-negative `numeric` vector: time of exposure of
#'   the infector(s) in the case data. Used to convert symptom onset in absolute
#'   time to relative time for each infectee. Default is for all exposure
#'   times to be 0.
#' @param alpha a `numeric` scalar: skew parameter of the skew-normal
#'   distribution. Used to model the relationship between incubation period and
#'   generation time.
#' @inheritParams delay_opts
#'
#' @return a `numeric` vector of generation times of equal length to the vector
#'   input to `symptom_onset_time`: the i-th element of the vector contains a
#'   sample from the generation time distribution of an individual with
#'   incubation period given by the i-th element of the `symptom_onset_time`
#'   vector. The lower bound of the output generation time vector is set by the
#'   `latent_period`, to prevent transmission before becoming infectious.
#' @export
#' @importFrom sn rsn
#'
#' @examples
#' incubation_to_generation_time(
#'   symptom_onset_time = c(1, 2, 3, 4, 1),
#'   alpha = 2
#' )
incubation_to_generation_time <- function(symptom_onset_time,
                                          exposure_time = rep(0, length(symptom_onset_time)),
                                          alpha,
                                          latent_period = 0) {

  checkmate::assert_numeric(symptom_onset_time, lower = 0, finite = TRUE)
  checkmate::assert_numeric(
    exposure_time, lower = 0, finite = TRUE, len = length(symptom_onset_time)
  )
  checkmate::assert_number(alpha, finite = TRUE)
  checkmate::assert_number(latent_period, lower = 0, finite = TRUE)

  # convert absolute to relative (individual) symptom onset time using exposure
  rel_symptom_onset_time <- symptom_onset_time - exposure_time

  # initialise generation time vector to trigger sampling loop
  gt <- rep(-Inf, times = length(rel_symptom_onset_time))
  # loop counter to stop infinite loop
  counter <- 0
  limit <- 1000
  resample_idx <- gt < latent_period
  n_resample <- sum(resample_idx)
  # ensure no negative or pre-infectious generation times
  while (n_resample && counter < limit) {
    gt[resample_idx] <- sn::rsn(
      n = n_resample,
      xi = rel_symptom_onset_time[resample_idx],
      omega = 2,
      alpha = alpha
    )
    resample_idx <- gt < latent_period
    n_resample <- sum(resample_idx)
    counter <- counter + 1
  }
  if (n_resample) {
    stop(
      "Unable to sample generation times satisfying `latent_period` >= ",
      "`incubation_period`.\nConsider reducing the `latent_period` or ",
      "checking parameter compatibility with the `incubation_period` ",
      "distribution.",
      call. = FALSE
    )
  }

  # convert generation time to absolute time and return
  gt + exposure_time
}

#' Estimate skew normal alpha parameter from proportion of presymptomatic
#' transmission
#'
#' @details Since there isn't any analytical expression for linking the two,
#' the value of alpha that corresponds to the given proportion presymptomatic
#' is obtained via numeric optimisation.
#'
#' @param presymptomatic_transmission a `numeric` scalar probability
#'   (between 0 and 1 inclusive): proportion of transmission that occurs
#'   before symptom onset.
#'
#' @return A `numeric` scalar: The `$minimum` output from [optimise()] to find
#'   the best `alpha` parameter to get the desired proportion of presymptomatic
#'   transmission.
#' @keywords internal
presymptomatic_transmission_to_alpha <- function(presymptomatic_transmission) {
  objective <- function(alpha) {
    # fix x, xi and omega for optimisation
    p_current <- sn::psn(x = 0, xi = 0, omega = 2, alpha = alpha)
    (p_current - presymptomatic_transmission)^2
  }
  # alpha domain is (-Inf, Inf), approximate with large numbers
  res <- stats::optimise(f = objective, interval = c(-1e5, 1e5))
  if (res$objective > 1e-5) {
    stop(
      "Estimating the `alpha` parameter from `presymptomatic_transmission` ",
      "did not converge."
    )
  }
  res$minimum
}

#' Outbreak extinction functions
#'
#' @description
#' `extinct_prob()`: Calculate proportion of runs that have controlled outbreak
#'
#' `detect_extinct()`: Calculate whether outbreaks went extinct or not
#'
#' @details
#' The data passed to `scenario` has to be produced by [scenario_sim()].
#' It cannot be produced by [outbreak_model()] as it requires the `sim` column,
#' which is only appended in [scenario_sim()].
#'
#' ***Warning***: the `$outbreak_ts` element of the [scenario_sim()] output
#' carries an `cap_cases` attribute which is used by [extinct_prob()] and
#' [detect_extinct()], therefore if you modify the output of [scenario_sim()]
#' before passing to [extinct_prob()] be careful not to drop the attribute
#' (e.g. from subsetting the `data.table`).
#'
#' The `extinction_time` is in the time interval the `scenario` is aggregated
#' to (set by the `aggregation` argument in [sim_opts()], or by
#' [`aggregate()`][aggregate.ringbp()]), for example weeks for the default
#' `"weekly"` aggregation, or days for `"daily"` aggregation. If the `scenario`
#' is not aggregated (`aggregation = "none"`) it is aggregated to daily cases,
#' with a warning, and `extinction_time` is in days.
#'
#' @param scenario a `list` output by [scenario_sim()], containing the
#'   `$outbreak_ts` `data.table` of cases used to determine extinction
#' @param extinction_time By default `NULL` but also accepts a positive
#'   `integer` scalar or `integer` vector to test if the outbreak has gone
#'   extinct (i.e. no new cases) by this time (zero indexed), in the time
#'   interval of the aggregated `scenario` (e.g. weeks for weekly aggregated
#'   data):
#'   * By default (`NULL`) the extinction status is stored in the output of
#'     `scenario_sim()` which is supplied to the `scenario` argument. If
#'     `extinction_time` is not specified or specified as `NULL` then the
#'     pre-computed extinction status from the outbreak simulation will be used.
#'     This is defined as all infectious cases have had the opportunity to
#'     transmit but no new cases are generated.
#'   * A single `integer` to test if extinction has occurred by this time.
#'     For example, for weekly data, `extinction_time = 5` tests whether the
#'     outbreak is extinct by week 5 (inclusive) until the end of the outbreak.
#'   * An `integer` vector of length two can be supplied to provide the lower
#'     and upper bounds (inclusive) of the time range to test for whether
#'     extinction occurred by this window. For example, for weekly data,
#'     `extinction_time = c(5, 10)` will test whether the outbreak went extinct
#'     by week 5 and there we no new cases between weeks 5 and 10 (inclusive).
#'   * An `integer` vector of length _n_ can be supplied to provide the times
#'     to test for whether extinction occurred by this window. For example,
#'     for weekly data, `extinction_time = 12:16` will test that there are no
#'     new infections between 12 and 16 weeks after the initial cases
#'     (inclusive). These integer sequences will most likely be contiguous but
#'     the function does allow non-contiguous integer sequences.
#'
#'   If extinction occurs before the `extinction_time` window then the outbreak
#'   extinction is considered extinct, however, if the extinction occurs within
#'   the `extinction_time` window it is not considered extinct. Therefore,
#'   using a single `integer` for `extinction_time` and thinking of this as
#'   "_has the outbreak gone extinct by time X_".
#'
#' @importFrom data.table setDT fifelse data.table
#'
#' @return
#' `extinct_prob()`: a single `numeric` with the probability of extinction
#'
#' `detect_extinct()`: a `data.table`, with two columns `sim` and `extinct`, for a binary
#' classification of whether the outbreak went extinct in each simulation
#' replicate. `1` is an outbreak that went extinct, `0` if not.
#'
#' @examples
#' res <- scenario_sim(
#'   n = 10,
#'   initial_cases = 1,
#'   offspring = offspring_opts(
#'     community = \(n) rnbinom(n = n, mu = 2.5, size = 0.16),
#'     isolated = \(n) rnbinom(n = n, mu = 0.5, size = 1)
#'   ),
#'   delays = delay_opts(
#'     incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
#'     onset_to_isolation = \(n) rweibull(n = n, shape = 1.65, scale = 4.28)
#'   ),
#'   event_probs = event_prob_opts(
#'     asymptomatic = 0,
#'     presymptomatic_transmission = 0.5,
#'     symptomatic_traced = 0.2
#'   ),
#'   interventions = intervention_opts(quarantine = FALSE),
#'   sim = sim_opts(cap_max_days = 350, cap_cases = 4500)
#' )
#'
#' # calculate probability of extinction
#' extinct_prob(res)
#'
#' # determine if each outbreak simulation replicate has gone extinct
#' detect_extinct(res)
#'
#' # calculate extinction in the last 2 weeks of the simulated outbreak
#' # (i.e. the penultimate and last week of the outbreak)
#' extinct_prob(res, extinction_time = max(res$outbreak_ts$week) - 1)
#'
#' # calculate extinction as no new cases between weeks 12 and 16 of the outbreak
#' extinct_prob(res, extinction_time = 12:16)
#' @name extinction
NULL

#' @rdname extinction
#' @export
extinct_prob <- function(scenario,
                         extinction_time = NULL) {

  extinct_runs <- detect_extinct(
    scenario = scenario,
    extinction_time = extinction_time
  )
  sum(extinct_runs$extinct) / max(extinct_runs$sim)
}

#' @rdname extinction
#' @autoglobal
#' @export
detect_extinct <- function(scenario,
                           extinction_time = NULL) {
  outbreak <- scenario
  scenario <- scenario$outbreak_ts
  extinct <- attr(scenario, which = "extinct", exact = TRUE)
  if (is.null(extinction_time)) {
    if (!is.null(extinct)) {
      message(
        "Calculating extinction using the extinction status from ",
        "the simulation."
      )
      return(
        data.table(
          # one extinct value per replicate, including replicates without
          # cases in a case line list
          sim = seq_along(extinct),
          extinct = as.integer(extinct)
        )
      )
    } else {
      stop(
        "`extinction_time` not specified and `scenario` is missing the ",
        "`extinct` attribute.\n Use `scenario_sim()` to simulate `scenario`, ",
        "or specify `extinction_time`.",
        call. = FALSE
      )
    }
  }

  checkmate::assert_data_frame(scenario)
  checkmate::assert_integerish(extinction_time, min.len = 1)

  aggregation <- attr(scenario, which = "aggregation", exact = TRUE)
  stopifnot(
    "`scenario` is missing the `aggregation` attribute.
    Use `scenario_sim()` to simulate `scenario`" = !is.null(aggregation)
  )
  # extinction for case line list is calculated on daily cases
  if (identical(aggregation, "none")) {
    checkmate::assert_class(outbreak, "ringbp", .var.name = "scenario")
    warning(
      "`scenario` is not aggregated (`aggregation = \"none\"`), it will be ",
      "aggregated to daily cases to calculate extinction.\n",
      "`extinction_time` is in days.",
      call. = FALSE
    )
    aggregation <- "daily"
    scenario <- aggregate(outbreak, interval = aggregation)$outbreak_ts
  }
  cols <- aggregation_cols(aggregation)
  time_col <- cols[["time"]]
  cases_col <- cols[["cases"]]
  time_unit <- if (is.numeric(aggregation)) {
    paste0(aggregation, "-day periods")
  } else {
    paste0(time_col, "s")
  }

  if (length(extinction_time) == 1) {
    extinction_time <- extinction_time:max(scenario[[time_col]])
  } else if (length(extinction_time) == 2) {
    extinction_time <- min(extinction_time):max(extinction_time)
  }
  stopifnot(
    "`extinction_time` not in simulated outbreak data" =
      all(extinction_time %in% scenario[[time_col]])
  )
  message(
    "Calculating extinction as no new cases within ", time_unit, ": ",
    min(extinction_time), " to ", max(extinction_time), " (inclusive)."
  )

  cap_cases <- attr(scenario, which = "cap_cases", exact = TRUE)
  stopifnot(
    "`scenario` is missing the `cap_cases` attribute.
    Use `scenario_sim()` to simulate `scenario`" = !is.null(cap_cases)
  )

  scenario <- setDT(scenario)
  scenario <- scenario[get(time_col) %in% extinction_time]
  scenario[, list(
    extinct = fifelse(all(get(cases_col) == 0 & cumulative < cap_cases), 1, 0)
  ), by = sim][]
}

# The following function is copied from `testthat:::on_ci()` from the
# {testthat} package (version 3.2.3).
# It is licensed under the MIT license (see LICENSE.md).
# Copyright (c) [2023] [testthat auhors]
on_ci <- function() isTRUE(as.logical(Sys.getenv("CI", unset = "FALSE")))

#' Control whether outbreak simulation continues stepping
#'
#' @description
#' Used in a while loop to determine whether the [outbreak_model()]
#'   continues to call [outbreak_step()], or to end the simulation.
#'
#' @inheritParams outbreak_step
#' @inheritParams outbreak_model
#'
#' @return a `logical` scalar: whether the outbreak is still active and should
#'   continue (`TRUE`) or if the outbreak is extinct or has reached a stopping
#'   criterion (`FALSE`).
#' @keywords internal
outbreak_continue <- function(case_data, sim) {

  # if all sampled == TRUE, then use Inf to end outbreak
  earliest_exposure <- min(c(case_data[sampled == FALSE, exposure], Inf))
  total_cases <- nrow(case_data)
  extinct <- all(case_data$sampled)

  return(
    earliest_exposure < sim$cap_max_days &&
      total_cases < sim$cap_cases && !extinct
  )
}

#' Coerce a probability input to a probability generating [function].
#'
#' @details Used to create time-constant functions from `numeric` scalar inputs
#'   in `*_opts()` functions so that users can provide simple number inputs
#'   but the internal outbreak simulation can use time-varying functions that
#'   generate probabilities.
#'
#'   This function also contains the input checking to ensure valid `numeric`
#'   scalar probabilities or `numeric` generating functions are provided.
#'   Functions input to `as_prob_function()` are input checked and then returned.
#'
#' @param x An \R object.
#'
#' @return A `numeric` generating `function`
#' @keywords internal
#' @name as_prob_function
as_prob_function <- function(x) {
  if (is.numeric(x)) {
    checkmate::assert_number(x, lower = 0, upper = 1)
    return(\(t) rep(x, length(t)))
  } else if (is.function(x)) {
    checkmate::assert_function(x, nargs = 1)
    # probability generating functions are [0,1]
    checkmate::assert_numeric(
      x(seq(0, 1000, length.out = 1e5)),
      lower = 0,
      upper = 1,
      any.missing = FALSE,
      len = 1e5
    )
    return(x)
  } else {
    stop(
      "Probabilities must be `numeric` or a `numeric` generating function.\n",
      "Check probabilities supplied in `*_opts()` functions.",
      call. = FALSE
    )
  }
}
