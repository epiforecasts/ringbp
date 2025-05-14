#' Run a specified number of simulations with identical parameters
#' @author Joel Hellewell
#' @param n_sim a positive `integer` scalar: number of simulations to run
#'
#' @inheritParams outbreak_model
#' @inheritParams outbreak_step
#' @importFrom data.table rbindlist
#' @return A `data.table` object returning the results for multiple simulations using
#' the same set of parameters. The table has columns
#' * week: The week in the simulation.
#' * weekly_cases: The number of new cases that week.
#' * cumulative: The cumulative cases.
#' * effective_r0: The effective reproduction rate for the whole simulation
#' * cases_per_gen: A list column with the cases per generation. This is repeated each row.
#' * sim: Index column for which simulation.
#' @autoglobal
#' @export
#'
#' @examples
#' res <- scenario_sim(
#'   n_sim = 5,
#'   num_initial_cases = 5,
#'   cap_max_days = 365,
#'   cap_cases = 2000,
#'   r0isolated = 0,
#'   r0community = 2.5,
#'   disp_isolated = 1,
#'   disp_community = 0.16,
#'   k = 0.7,
#'   onset_to_isolation = \(x) rweibull(n = x, shape = 2.5, scale = 5),
#'   incubation_period = \(x) rweibull(n = x, shape = 2.32, scale = 6.49),
#'   prop_asymptomatic = 0,
#'   prop_ascertain = 0,
#'   quarantine = TRUE
#' )
#' res
scenario_sim <- function(n_sim, prop_ascertain, cap_max_days, cap_cases,
                         r0isolated, r0community, disp_isolated, disp_community, k,
                         onset_to_isolation, incubation_period,
                         num_initial_cases, prop_asymptomatic, quarantine = FALSE,
                         r0asymptomatic = NULL, disp_asymptomatic = NULL) {

  # Set infectiousness of subclinical cases to be equal to clinical cases unless specified otherwise
  if(is.null(r0asymptomatic)) {
    r0asymptomatic <- r0community
  }

  if(is.null(disp_asymptomatic)) {
    disp_asymptomatic <- disp_community
  }
  # Run n_sim number of model runs and put them all together in a big data.frame
  res <- replicate(
    n_sim, outbreak_model(
      num_initial_cases = num_initial_cases,
      prop_ascertain = prop_ascertain,
      cap_max_days = cap_max_days,
      cap_cases = cap_cases,
      r0isolated = r0isolated,
      r0community = r0community,
      r0asymptomatic = r0asymptomatic,
      disp_asymptomatic = disp_asymptomatic,
      disp_isolated = disp_isolated,
      disp_community = disp_community,
      onset_to_isolation = onset_to_isolation,
      incubation_period = incubation_period,
      k = k,
      prop_asymptomatic = prop_asymptomatic,
      quarantine = quarantine
    ), simplify = FALSE)

  # bind output together and add simulation index
  res <- data.table::rbindlist(res, idcol = "sim")

  return(res[])
}
