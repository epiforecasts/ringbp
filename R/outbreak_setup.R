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
#' @importFrom stats runif
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
  case_data <- data.table(
    exposure = 0, # Exposure time of 0 for all initial cases
    asym = runif(num.initial.cases) < prop.asym,
    caseid = seq_len(num.initial.cases), # set case id
    infector = 0,
    isolated = FALSE,
    missed = TRUE,
    onset = incubation_period(num.initial.cases),
    new_cases = NA,
    isolated_time = Inf
  )

  # set isolation time for cluster to minimum time of onset of symptoms + draw from delay distribution
  case_data <- case_data[
    asym == FALSE,
    isolated_time := onset + onset_to_isolation(.N)
  ]

  # return
  return(case_data)
}
