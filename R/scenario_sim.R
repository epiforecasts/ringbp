#' Run a specified number of simulations with identical parameters
#' @author Joel Hellewell
#'
#' @param n a positive `integer` scalar: number of simulations to run
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
#' parameters <- parameters(
#'   initial_cases = 5,
#'   r0_community = 2.5,
#'   r0_isolated = 0,
#'   disp_community = 0.16,
#'   disp_isolated = 1,
#'   incubation_period = \(x) rweibull(n = x, shape = 2.32, scale = 6.49),
#'   prop_presymptomatic = 0.3,
#'   onset_to_isolation = \(x) rweibull(n = x, shape = 2.5, scale = 5),
#'   prop_ascertain = 0,
#'   prop_asymptomatic = 0,
#'   quarantine = TRUE
#' )
#' control <- control(
#'   cap_max_days = 365,
#'   cap_cases = 2000
#' )
#' res <- scenario_sim(
#'   n = 5,
#'   parameters = parameters,
#'   control = control
#' )
#' res
scenario_sim <- function(n,
                         parameters,
                         control) {

  checkmate::assert_number(n, lower = 1, finite = TRUE)
  checkmate::assert_class(parameters, "ringbp_parameters")
  checkmate::assert_class(control, "ringbp_control")

  # Run n number of model runs and put them all together in a big data.frame
  res <- replicate(
    n, outbreak_model(
      parameters = parameters,
      control = control
    ),
    simplify = FALSE
  )

  # bind output together and add simulation index
  res <- data.table::rbindlist(res, idcol = "sim")

  return(res[])
}
