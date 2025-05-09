#' Run a specified number of simulations with identical parameters
#' @author Joel Hellewell
#' @param n.sim a positive `integer` scalar: number of simulations to run
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
#'   n.sim = 5,
#'   num.initial.cases = 5,
#'   cap_max_days = 365,
#'   cap_cases = 2000,
#'   r0isolated = 0,
#'   r0community = 2.5,
#'   disp.iso = 1,
#'   disp.com = 0.16,
#'   k = 0.7,
#'   onset_to_isolation = \(x) rweibull(n = x, shape = 2.5, scale = 5),
#'   incubation_period = \(x) rweibull(n = x, shape = 2.32, scale = 6.49),
#'   prop.asym = 0,
#'   prop.ascertain = 0,
#'   quarantine = TRUE
#' )
#' res
scenario_sim <- function(n.sim, prop.ascertain, cap_max_days, cap_cases,
                         r0isolated, r0community, disp.iso, disp.com, k,
                         onset_to_isolation, incubation_period,
                         num.initial.cases, prop.asym, quarantine = FALSE,
                         r0subclin = NULL, disp.subclin = NULL) {

  # Set infectiousness of subclinical cases to be equal to clinical cases unless specified otherwise
  if(is.null(r0subclin)) {
    r0subclin <- r0community
  }

  if(is.null(disp.subclin)) {
    disp.subclin <- disp.com
  }
  # Run n.sim number of model runs and put them all together in a big data.frame
  res <- replicate(
    n.sim, outbreak_model(
      num.initial.cases = num.initial.cases,
      prop.ascertain = prop.ascertain,
      cap_max_days = cap_max_days,
      cap_cases = cap_cases,
      r0isolated = r0isolated,
      r0community = r0community,
      r0subclin = r0subclin,
      disp.subclin = disp.subclin,
      disp.iso = disp.iso,
      disp.com = disp.com,
      onset_to_isolation = onset_to_isolation,
      incubation_period = incubation_period,
      k = k,
      prop.asym = prop.asym,
      quarantine = quarantine
    ), simplify = FALSE)

  # bind output together and add simulation index
  res <- data.table::rbindlist(res, idcol = "sim")

  return(res[])
}
