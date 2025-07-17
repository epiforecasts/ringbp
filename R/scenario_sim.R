#' Run a specified number of simulations with identical parameters
#'
#' @param n a positive `integer` scalar: number of simulations to run
#' @inheritParams outbreak_setup
#' @inheritParams outbreak_step
#' @inheritParams outbreak_model
#'
#' @importFrom data.table rbindlist
#' @return A `data.table` object returning the results for multiple simulations
#'   using the same set of parameters. The table has columns
#' * week: The week in the simulation.
#' * weekly_cases: The number of new cases that week.
#' * cumulative: The cumulative cases.
#' * effective_r0: The effective reproduction rate for the whole simulation
#' * cases_per_gen: A list column with the cases per generation. This is
#'   repeated each row.
#' * sim: Index column for which simulation.
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
#'   symptomatic_ascertained = 0
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

  # bind output together and add simulation index
  res <- data.table::rbindlist(res, idcol = "sim")

  return(res[])
}
