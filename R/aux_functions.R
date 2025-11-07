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

#' Calculate proportion of runs that have controlled outbreak
#'
#' @inherit detect_extinct details
#'
#' @inheritParams detect_extinct
#'
#' @return a single `numeric` with the probability of extinction
#' @export
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
#'     symptomatic_ascertained = 0.2
#'   ),
#'   interventions = intervention_opts(quarantine = FALSE),
#'   sim = sim_opts(cap_max_days = 350, cap_cases = 4500)
#' )
#' extinct_prob(res, cap_cases = 4500)
extinct_prob <- function(outbreak_df_week, cap_cases, week_range = 12:16) {

  checkmate::assert_data_frame(outbreak_df_week)
  checkmate::assert_number(cap_cases, lower = 0)
  checkmate::assert_numeric(week_range)

  n <- max(outbreak_df_week$sim)

  extinct_runs <- detect_extinct(outbreak_df_week, cap_cases, week_range)
  sum(extinct_runs$extinct) / n
}


#' Calculate whether outbreaks went extinct or not
#'
#' @details
#' The `cap_cases` argument should be equal to the value supplied to
#' [outbreak_model()] (possibly passed from [scenario_sim()]).
#'
#' @param outbreak_df_week a `data.table`: weekly cases produced by the
#'   outbreak model
#' @inheritParams sim_opts
#' @param week_range a positive `integer` vector: giving the (zero indexed)
#'   week range to test for whether an extinction occurred. Default is `12:16`.
#' @importFrom data.table as.data.table fifelse
#'
#' @return A `data.table`, with two columns `sim` and `extinct`, for a binary
#' classification of whether the outbreak went extinct in each simulation
#' replicate. `1` is an outbreak that went extinct, `0` if not.
#' @autoglobal
#' @export
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
#'     symptomatic_ascertained = 0.2
#'   ),
#'   interventions = intervention_opts(quarantine = FALSE),
#'   sim = sim_opts(cap_max_days = 350, cap_cases = 4500)
#' )
#' detect_extinct(outbreak_df_week = res, cap_cases = 4500)
detect_extinct <- function(outbreak_df_week, cap_cases, week_range = 12:16) {

  checkmate::assert_data_frame(outbreak_df_week)
  checkmate::assert_number(cap_cases, lower = 0)
  checkmate::assert_integerish(week_range)

  outbreak_df_week <- as.data.table(outbreak_df_week)
  outbreak_df_week <- outbreak_df_week[week %in% week_range]
  outbreak_df_week[, list(
    extinct = fifelse(all(weekly_cases == 0 & cumulative < cap_cases), 1, 0)
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

  latest_onset <- max(case_data$onset)
  total_cases <- nrow(case_data)
  extinct <- all(case_data$isolated)

  return(
    latest_onset < sim$cap_max_days &&
      total_cases < sim$cap_cases && !extinct
  )
}

