#' Set up initial cases for branching process
#' @author Joel Hellewell
#'
#' @inheritParams outbreak_model
#' @inheritParams outbreak_step
#'
#' @return `data.table` of cases in outbreak so far. `data.table` columns are:
#' * `$exposure`: `numeric`
#' * `$asym`: `logical`
#' * `$caseid`: `integer`
#' * `$infector`: `numeric`
#' * `$missed`: `logical`
#' * `$onset`: `numeric`
#' * `$new_cases`: `logical`
#' * `$isolated_time`: `numeric`
#' * `$isolated`: `logical`
#' @autoglobal
#' @export
#' @importFrom data.table data.table
#' @importFrom stats rbinom
#'
#' @examples
#' # incubation period sampling function
#' incubation_period <- \(x) rweibull(n = x, shape = 2.32, scale = 6.49)
#' # delay distribution sampling function
#' onset_to_isolation <- \(x) rweibull(n = x, shape = 1.65, scale = 4.28)
#' out <- outbreak_setup(
#'   num.initial.cases = 1,
#'   incubation_period = incubation_period,
#'   onset_to_isolation = onset_to_isolation,
#'   k = 1.95,
#'   prop.asym = 0
#' )
#' out
outbreak_setup <- function(num.initial.cases, incubation_period, onset_to_isolation, k, prop.asym) {
  # Set up table of initial cases
  inc_samples <- incubation_period(num.initial.cases)

  case_data <- data.table(exposure = rep(0, num.initial.cases), # Exposure time of 0 for all initial cases
                          asym = as.logical(rbinom(num.initial.cases, 1, prop.asym)),
                          caseid = 1:(num.initial.cases), # set case id
                          infector = 0,
                          missed = TRUE,
                          onset = inc_samples,
                          new_cases = NA)

  # set isolation time for cluster to minimum time of onset of symptoms + draw from delay distribution
  case_data <- case_data[, isolated_time := onset + onset_to_isolation(1)
                         ][, isolated := FALSE]

  case_data$isolated_time[case_data$asym] <- Inf

  # return
  return(case_data)
}
