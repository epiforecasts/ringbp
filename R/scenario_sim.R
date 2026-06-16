#' Run a specified number of simulations with identical parameters
#'
#' @param n a positive `integer` scalar: number of simulations to run
#' @inheritParams outbreak_setup
#' @inheritParams outbreak_step
#' @inheritParams outbreak_model
#'
#' @importFrom data.table rbindlist setattr
#' @return A `list` with 2 `data.table` elements:
#' 1. `$outbreak_ts`: the results for multiple simulations using the same
#'    set of parameters. The `data.table` has columns:
#'    * `sim`: the simulation replicate index (`integer`)
#'    * `week`: the week in the simulation (`integer`)
#'    * `weekly_cases`: the number of new cases that week (`integer`)
#'    * `cumulative`: the cumulative cases (`integer`)
#' 2. `$outbreak_stats`: the summary statistics for each outbreak simulation
#'    replicate using the same set of parameters. The `data.table` has columns:
#'    * `sim`: the simulation replicate index (`integer`)
#'    * `effective_r0`: the effective reproduction rate for the whole
#'      simulation (`numeric`)
#'    * `cases_per_gen`: the cases per generation (`list`)
#'
#' The `$outbreak_ts` element also carries two attributes used by
#' [extinct_prob()] and [detect_extinct()]: `extinct`, a `logical` vector
#' recording whether each replicate went extinct, and `cap_cases`, the maximum
#' number of cases used to cap each simulation.
#'
#' @autoglobal
#' @export
#'
#' @examples
#' offspring <- offspring_opts(
#'   community = \(n) rnbinom(n = n, mu = 2.5, size = 0.16),
#'   isolated = \(n) rnbinom(n = n, mu = 0, size = 1),
#'   asymptomatic = \(n) rnbinom(n = n, mu = 2.5, size = 0.16)
#' )
#' delays <- delay_opts(
#'   incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
#'   onset_to_isolation = \(n) rweibull(n = n, shape = 2.5, scale = 5)
#' )
#' event_probs <- event_prob_opts(
#'   asymptomatic = 0,
#'   presymptomatic_transmission = 0.3,
#'   symptomatic_traced = 0
#' )
#' interventions <- intervention_opts(quarantine = TRUE)
#' sim <- sim_opts(
#'   cap_max_days = 365,
#'   cap_cases = 2000
#' )
#' res <- scenario_sim(
#'   n = 5,
#'   initial_cases = 5,
#'   offspring = offspring,
#'   delays = delays,
#'   event_probs = event_probs,
#'   interventions = interventions,
#'   sim = sim
#' )
#' res
scenario_sim <- function(n,
                         initial_cases,
                         offspring,
                         delays,
                         event_probs,
                         interventions,
                         sim) {

  checkmate::assert_number(n, lower = 1, finite = TRUE)
  checkmate::assert_number(initial_cases, lower = 1, finite = TRUE)
  checkmate::assert_class(offspring, "ringbp_offspring_opts")
  checkmate::assert_class(delays, "ringbp_delay_opts")
  checkmate::assert_class(event_probs, "ringbp_event_prob_opts")
  checkmate::assert_class(interventions, "ringbp_intervention_opts")
  checkmate::assert_class(sim, "ringbp_sim_opts")
  cross_check_opts(delays, event_probs)

  # cross-check once per scenario not per replicate
  attr(x = delays$onset_to_self_isolation, which = "cross_checked") <- TRUE

  # Run n number of model runs and put them all together in a big data.frame
  res <- replicate(
    n, outbreak_model(
      initial_cases = initial_cases,
      offspring = offspring,
      delays = delays,
      event_probs = event_probs,
      interventions = interventions,
      sim = sim
    ),
    simplify = FALSE
  )

  outbreak_ts <- lapply(res, `[[`, "outbreak_ts")
  outbreak_stats <- lapply(res, `[[`, "outbreak_stats")

  extinct <- vapply(
    outbreak_ts, attr, FUN.VALUE = logical(1), which = "extinct", exact = TRUE
  )

  # bind output together and add simulation index
  outbreak_ts <- data.table::rbindlist(outbreak_ts, idcol = "sim")
  outbreak_stats <- data.table::rbindlist(outbreak_stats, idcol = "sim")

  setattr(outbreak_ts, name = "cap_cases", value = sim$cap_cases)
  setattr(outbreak_ts, name = "extinct", value = extinct)

  list(
    outbreak_ts = outbreak_ts,
    outbreak_stats = outbreak_stats
  )
}
