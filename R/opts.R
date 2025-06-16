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
  return(opts)
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
#'
#' @return A `list` with class `<ringbp_delay_opts>`.
#' @export
#'
#' @examples
#' delay_opts(
#'   incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
#'   onset_to_isolation = \(n) rweibull(n = n, shape = 1.65, scale = 4.28)
#' )
delay_opts <- function(incubation_period, onset_to_isolation) {

  check_dist_func(incubation_period, dist_name = "incubation_period")
  check_dist_func(onset_to_isolation, dist_name = "onset_to_isolation")

  opts <- list(
    incubation_period = incubation_period,
    onset_to_isolation = onset_to_isolation
  )

  class(opts) <- "ringbp_delay_opts"
  return(opts)
}

#' Create a list of event probabilities to run the \pkg{ringbp} model
#'
#' @param asymptomatic a `numeric` scalar probability (between 0 and 1
#'   inclusive): proportion of cases that are completely asymptomatic
#'   (subclinical)
#' @inheritParams presymptomatic_transmission_to_alpha
#' @param symptomatic_ascertained a `numeric` scalar probability (between 0
#'   and 1 inclusive): proportion of infectious contacts ascertained by contact
#'   tracing
#'
#' @return A `list` with class `<ringbp_event_prob_opts>`.
#' @export
#'
#' @examples
#' event_prob_opts(
#'   asymptomatic = 0.1,
#'   presymptomatic_transmission = 0.5,
#'   symptomatic_ascertained = 0.2
#' )
event_prob_opts <- function(asymptomatic,
                            presymptomatic_transmission,
                            symptomatic_ascertained) {

  checkmate::assert_number(asymptomatic, lower = 0, upper = 1)
  checkmate::assert_number(presymptomatic_transmission, lower = 0, upper = 1)
  checkmate::assert_number(symptomatic_ascertained, lower = 0, upper = 1)

  # calculate alpha parameter from presymptomatic_transmission
  alpha <- presymptomatic_transmission_to_alpha(
    presymptomatic_transmission = presymptomatic_transmission
  )

  opts <- list(
    asymptomatic = asymptomatic,
    alpha = alpha,
    symptomatic_ascertained = symptomatic_ascertained
  )

  class(opts) <- "ringbp_event_prob_opts"
  return(opts)
}

#' Create a list of intervention settings to run the \pkg{ringbp} model
#'
#' @param quarantine a `logical` scalar: whether quarantine is in effect, if
#'   `TRUE` then traced contacts are isolated before symptom onset; defaults to
#'   `FALSE`
#'
#' @return A `list` with class `<ringbp_intervention_opts>`.
#' @export
#'
#' @examples
#' intervention_opts(quarantine = FALSE)
intervention_opts <- function(quarantine = FALSE) {
  checkmate::assert_logical(quarantine, any.missing = FALSE, len = 1)
  opts <- list(quarantine = quarantine)
  class(opts) <- "ringbp_intervention_opts"
  return(opts)
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
  return(opts)
}
