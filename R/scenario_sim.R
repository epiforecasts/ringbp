#' Run a specified number of simulations with identical parameters
#' @author Joel Hellewell
#' @param n.sim number of simulations to run
#' @param num.initial.cases Initial number of cases in each initial cluster
#' @param num.initial.clusters Number of initial clusters
#' @param prop.ascertain Probability that cases are ascertained by contact tracing
#' @param cap_max_days Maximum number of days to run process for
#' @param cap_cases Maximum number of cases to run process for
#' @param r0isolated basic reproduction number for isolated cases
#' @param r0community basic reproduction number for non-isolated cases
#' @param disp.iso dispersion parameter for negative binomial distribution for isolated cases
#' @param disp.com dispersion parameter for negative binomial distribution for non-isolated cases
#' @param delay_shape shape of distribution for delay between symptom onset and isolation
#' @param delay_scale scale of distribution for delay between symptom onset and isolation
#' @param k numeric skew parameter for sampling the serial interval from the incubation period
#' @param quarantine logical whether quarantine is in effect, if TRUE then traced contacts are isolated before symptom onset
#' @param prop.asym proportion of cases that are completely asymptomatic.
#'
#' @importFrom purrr safely
#' @importFrom stats as.formula
#' @importFrom stats dist
#' @importFrom stats dweibull
#' @importFrom stats dnbinom
#' @importFrom stats median
#' @importFrom stats quantile
#' @importFrom stats rweibull
#' @importFrom utils data
#' @return A data.table object returning the results for multiple simulations using
#' the same set of parameters. The table has columns
#' * week: The week in the simulation.
#' * weekly_cases: The number of new cases that week.
#' * cumulative: The cumulative cases.
#' * effective_r0: The effective reproduction rate for the whole simulation
#' * cases_per_gen: A list column with the cases per generation. This is repeated each row.
#' * sim: Index column for which simulation.
#' @export
#'
#' @examples
#' \dontrun{
#' res <- scenario_sim(n.sim = 5,
#' num.initial.cases = 5,
#' cap_max_days = 365,
#' cap_cases = 2000,
#' r0isolated = 0,
#' r0community = 2.5,
#' disp.iso = 1,
#' disp.com = 0.16,
#' k = 0.7,
#' delay_shape = 2.5,
#' delay_scale = 5,
#' prop.asym = 0,
#' prop.ascertain = 0)
#' }
#'
scenario_sim <- function(n.sim, prop.ascertain, cap_max_days, cap_cases,
                         r0isolated, r0community, disp.iso, disp.com, k,
                         delay_shape, delay_scale, num.initial.cases, prop.asym,
                         quarantine) {

  # Run n.sim number of model runs and put them all together in a big data.frame
  res <- purrr::map(.x = 1:n.sim, ~ outbreak_model(num.initial.cases = num.initial.cases,
                                             prop.ascertain = prop.ascertain,
                                             cap_max_days = cap_max_days,
                                             cap_cases = cap_cases,
                                             r0isolated = r0isolated,
                                             r0community = r0community,
                                             disp.iso = disp.iso,
                                             disp.com = disp.com,
                                             delay_shape = delay_shape,
                                             delay_scale = delay_scale,
                                             k = k,
                                             prop.asym = prop.asym,
                                             quarantine = quarantine))


  # bind output together and add simulation index
  res <- data.table::rbindlist(res)
  res[, sim := rep(1:n.sim, rep(floor(cap_max_days / 7) + 1, n.sim)), ]
  return(res)
}


utils::globalVariables(c(".", ".N", ":=", "asym", "control_effectiveness", "cumulative", "data", "delay",
                         "disp",  "exposure", "extinct", "full_join", "incubfn_sample",
                         "index_R0", "infector_iso_time", "iqr_lower", "iqr_upper", "isolated", "isolated_time",
                         "k", "label_extinct", "lower", "max_weekly_cases", "median_eff_r0", "missed",
                         "new_cases", "num.initial.cases", "onset", "pext", "prob_extinct", "prop.asym",
                         "r0", "rweibull", "samp", "samples", "scenario", "sim", "sims", "theta", "upper", "value",
                         "week", "weekly_cases", "x", "y", "y0", "y100", "y25", "y50", "y75"))
                         


