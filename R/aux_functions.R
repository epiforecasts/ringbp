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
