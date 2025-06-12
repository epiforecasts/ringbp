#' Set up initial cases for branching process
#' @author Joel Hellewell
#'
#' @inheritParams outbreak_step
#'
#' @return `data.table` of cases in outbreak so far. `data.table` columns are:
#' * `$exposure`: `numeric`
#' * `$asymptomatic`: `logical`
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
#' parameters <- parameters(
#'   initial_cases = 5,
#'   r0_community = 2.5,
#'   r0_isolated = 0,
#'   r0_asymptomatic = 1.25,
#'   disp_community = 0.16,
#'   disp_isolated = 1,
#'   disp_asymptomatic = 0.16,
#'   incubation_period = \(x) rweibull(n = x, shape = 2.32, scale = 6.49),
#'   prop_presymptomatic = 0.15,
#'   onset_to_isolation = \(x) rweibull(n = x, shape = 1.65, scale = 4.28),
#'   prop_ascertain = 0,
#'   prop_asymptomatic = 0,
#'   quarantine = FALSE
#' )
#'
#' # generate initial cases
#' case_data <- outbreak_setup(parameters = parameters)
#' case_data
outbreak_setup <- function(parameters) {

  checkmate::assert_class(parameters, "ringbp_parameters")

  # Set up table of initial cases
  case_data <- data.table(
    exposure = 0, # Exposure time of 0 for all initial cases
    asymptomatic = runif(parameters$initial_cases) < parameters$prop_asymptomatic,
    caseid = seq_len(parameters$initial_cases), # set case id
    infector = 0,
    isolated = FALSE,
    missed = TRUE,
    onset = parameters$incubation_period(parameters$initial_cases),
    new_cases = NA,
    isolated_time = Inf
  )

  # set isolation time for cluster to minimum time of onset of symptoms + draw
  # from delay distribution
  case_data <- case_data[
    asymptomatic == FALSE,
    isolated_time := onset + parameters$onset_to_isolation(.N)
  ]

  return(case_data)
}
