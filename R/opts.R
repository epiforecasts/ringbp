#' Create a list of offspring distributions to run the \pkg{ringbp} model
#'
#' @details
#' If `asymptomatic` is not provided it will be specified as the same as
#'   `community` meaning transmission of subclinical cases to be equal to
#'   clinical cases unless specified otherwise.
#'
#' @param community a `function`: a random number generating `function`
#'   that samples from the community (non-isolated) offspring distribution,
#'   the `function` accepts a single `integer` argument specifying the number
#'   of times to sample the offspring distribution (i.e. the length of the
#'   `function` output)
#' @param isolated a `function`: a random number generating `function` that
#'   samples from the isolated cases offspring distribution, the `function`
#'   accepts a single `integer` argument specifying the number of times to
#'   sample the offspring distribution (i.e. the length of the `function`
#'   output)
#' @param asymptomatic a `function`: a random number generating `function`
#'   that samples from the sub-clinical non-isolated cases offspring
#'   distribution, the `function` accepts a single `integer` argument
#'   specifying the number of times to sample the offspring distribution (i.e.
#'   the length of the `function` output). Will be specified as the same as
#'   the `community` offspring distribution if left unspecified
#'
#' @return A `list` with class `<ringbp_offspring_opts>`.
#' @export
#'
#' @examples
#' # Negative binomial offspring distributions with:
#' # Community R0 of 2.5 and dispersion of 0.16
#' # Isolated R0 of 0.5 and dispersion of 1
#' # Asymptomatic R0 of 2.5 and dispersion of 0.16
#' offspring_opts(
#'   community = \(n) rnbinom(n = n, mu = 2.5, size = 0.16),
#'   isolated = \(n) rnbinom(n = n, mu = 0.5, size = 1),
#'   asymptomatic = \(n) rnbinom(n = n, mu = 2.5, size = 0.16)
#' )
offspring_opts <- function(community, isolated, asymptomatic = community) {

  check_dist_func(community, dist_name = "community")
  check_dist_func(isolated, dist_name = "isolated")
  check_dist_func(asymptomatic, dist_name = "asymptomatic")

  opts <- list(
    community = community,
    isolated = isolated,
    asymptomatic = asymptomatic
  )

  class(opts) <- "ringbp_offspring_opts"
  opts
}

#' Create a list of delay distributions to run the \pkg{ringbp} model
#'
#' @param incubation_period a `function`: a random number generating
#'   `function` that samples from incubation period distribution, the
#'   `function` accepts a single `integer` argument specifying the number of
#'   times to sample the incubation period (i.e. length of the `function`
#'   output).
#' @param onset_to_isolation a `function`: a random number generating
#'   `function` that accepts a single `integer` argument specifying the
#'   length of the `function` output.
#' @param latent_period a non-negative `numeric` scalar: the minimum time
#'   between an individual being exposed and becoming infectious. It is a
#'   population-wide parameter, with no variability between individuals. It
#'   sets the minimum generation time in the model. Default is 0 (i.e.
#'   an individual becomes immediately infectious after being infected).
#'
#'   If `latent_period` is positive then generation times are sampled
#'   conditional on `gt >= latent_period` (i.e. left-truncated at
#'   `latent_period`). This may reduce the realised proportion of
#'   presymptomatic transmission, depending on the `incubation_period`
#'   distribution and `presymptomatic_transmission` (in [event_prob_opts()]).
#'
#' @param onset_to_self_isolation a `function`: a random number generating
#'   `function` that samples from the onset-to-self-isolation distribution,
#'   the `function` accepts a single `integer` argument specifying the length
#'   of the `function` output.
#'
#'   By default `onset_to_self_isolation` is `NULL`. An onset-to-self-isolation
#'   `function` only needs to be specified if a non-zero value is specified to
#'   `symptomatic_self_isolate` in [event_prob_opts()] (which by default is 0).
#'   If `onset_to_self_isolation` is specified but `symptomatic_self_isolate`
#'   is zero, a warning will be thrown and the `onset_to_self_isolation`
#'   will be ignored; if `symptomatic_self_isolate` is non-zero and
#'   `onset_to_self_isolation` is `NULL`, [scenario_sim()] will error.
#'
#' @return A `list` with class `<ringbp_delay_opts>`.
#' @export
#'
#' @examples
#' delay_opts(
#'   incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
#'   onset_to_isolation = \(n) rweibull(n = n, shape = 1.65, scale = 4.28)
#' )
delay_opts <- function(incubation_period,
                       onset_to_isolation,
                       latent_period = 0,
                       onset_to_self_isolation = NULL) {

  check_dist_func(incubation_period, dist_name = "incubation_period")
  check_dist_func(onset_to_isolation, dist_name = "onset_to_isolation")
  checkmate::assert_number(latent_period, lower = 0, finite = TRUE)
  if (!is.null(onset_to_self_isolation)) {
    check_dist_func(
      onset_to_self_isolation,
      dist_name = "onset_to_self_isolation"
    )
  }

  if (latent_period > 0) {
    warning(
      "A `latent_period` > 0 may cause the realised proportion of ",
      "presymptomatic transmission to be less than specified.\n",
      "(`presymptomatic_transmission` in `event_prob_opts()`)\n",
      "The realised proportion of presymptomatic transmission is printed ",
      "after the simulation.",
      call. = FALSE
    )
  }

  opts <- list(
    incubation_period = incubation_period,
    onset_to_isolation = onset_to_isolation,
    latent_period = latent_period,
    onset_to_self_isolation = onset_to_self_isolation
  )

  class(opts) <- "ringbp_delay_opts"
  opts
}

#' Create a list of event probabilities to run the \pkg{ringbp} model
#'
#' @param asymptomatic a `numeric` scalar probability (between 0 and 1
#'   inclusive): proportion of cases that are completely asymptomatic
#'   (subclinical)
#' @inheritParams presymptomatic_transmission_to_alpha
#' @param symptomatic_traced a `numeric` scalar probability (between 0
#'   and 1 inclusive): proportion of infectious contacts ascertained by contact
#'   tracing
#' @param symptomatic_self_isolate a `numeric` scalar probability (between 0
#'   and 1 inclusive): proportion of cases that self-isolate when they become
#'   symptomatic. These individuals do not get tested and do not require a
#'   positive test result to enter isolation. Default is 0 (i.e. no infectious
#'   individuals self-isolate).
#'
#'   If `symptomatic_self_isolate` is non-zero a random number generating
#'   `function` needs to be specified in the `onset_to_self_isolation` argument
#'   in the [delay_opts()] function, otherwise the [scenario_sim()] will error.
#'
#' @return A `list` with class `<ringbp_event_prob_opts>`.
#' @export
#'
#' @examples
#' event_prob_opts(
#'   asymptomatic = 0.1,
#'   presymptomatic_transmission = 0.5,
#'   symptomatic_traced = 0.2
#' )
event_prob_opts <- function(asymptomatic,
                            presymptomatic_transmission,
                            symptomatic_traced,
                            symptomatic_self_isolate = 0) {

  checkmate::assert_number(asymptomatic, lower = 0, upper = 1)
  checkmate::assert_number(presymptomatic_transmission, lower = 0, upper = 1)
  checkmate::assert_number(symptomatic_traced, lower = 0, upper = 1)
  checkmate::assert_number(symptomatic_self_isolate, lower = 0, upper = 1)

  # calculate alpha parameter from presymptomatic_transmission
  alpha <- presymptomatic_transmission_to_alpha(
    presymptomatic_transmission = presymptomatic_transmission
  )

  opts <- list(
    asymptomatic = asymptomatic,
    presymptomatic_transmission = presymptomatic_transmission,
    alpha = alpha,
    symptomatic_traced = symptomatic_traced,
    symptomatic_self_isolate = symptomatic_self_isolate
  )

  class(opts) <- "ringbp_event_prob_opts"
  opts
}

#' Create a list of intervention settings to run the \pkg{ringbp} model
#'
#' @param quarantine a `logical` scalar: whether quarantine is in effect, if
#'   `TRUE` then traced contacts are isolated before symptom onset; defaults to
#'   `FALSE`
#' @param test_sensitivity a `numeric` scalar probability (between 0
#'   and 1 inclusive): the test sensitivity (i.e. probability that a true positive
#'   tests positive). Individuals that are tested and get a false negative
#'   are not isolated and their contacts are not traced. Only symptomatic
#'   individuals are tested. Default is 1, which assumes all symptomatic
#'   individuals tested get a positive test result.
#'
#' @return A `list` with class `<ringbp_intervention_opts>`.
#' @export
#'
#' @examples
#' intervention_opts(quarantine = FALSE)
intervention_opts <- function(quarantine = FALSE, test_sensitivity = 1) {
  checkmate::assert_logical(quarantine, any.missing = FALSE, len = 1)
  checkmate::assert_number(test_sensitivity, lower = 0, upper = 1)
  opts <- list(quarantine = quarantine, test_sensitivity = test_sensitivity)
  class(opts) <- "ringbp_intervention_opts"
  opts
}

#' Create a list of simulation control options for the \pkg{ringbp} model
#'
#' @param cap_max_days a positive `integer` scalar: stop the simulation when
#'   this many days is reached.
#' @param cap_cases a positive `integer` scalar: number of cumulative cases at
#'   which the branching process (simulation) was terminated
#'
#' @return A `list` with class `<ringbp_sim_opts>`.
#' @export
#'
#' @examples
#' # default simulation control options
#' sim_opts()
#'
#' # specifying custom simulation control options
#' sim_opts(
#'   cap_max_days = 140,
#'   cap_cases = 1000
#' )
sim_opts <- function(cap_max_days = 350, cap_cases  = 5000) {

  checkmate::assert_int(cap_max_days, lower = 1)
  checkmate::assert_int(cap_cases, lower = 1)

  opts <- list(
    cap_max_days = cap_max_days,
    cap_cases = cap_cases
  )

  class(opts) <- "ringbp_sim_opts"
  opts
}
