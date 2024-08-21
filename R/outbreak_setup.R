#' Set up initial cases for branching process
#' @author Joel Hellewell
#'
#' @inheritParams outbreak_model
#' @inheritParams outbreak_step
#' @param k Numeric skew parameter for sampling the serial interval from the
#' incubation period
#'
#' @return data.table of cases in outbreak so far
#' @keywords internal
#' @importFrom data.table data.table
#' @importFrom stats rbinom
#'
#' @examples
#'
#'\dontrun{
#' # incubation period sampling function
#' incfn <- dist_setup(dist_shape = 2.322737,dist_scale = 6.492272)
#' # delay distribution sampling function
#' onset_to_isolation <- function(x) {
#'   rweibull(n = x, shape = 1.651524, scale = 4.287786)
#' }
#' outbreak_setup(
#'   num_initial_cases = 5,
#'   incfn,
#'   onset_to_isolation,
#'   k = 1.95,
#'   prop_asym = 0
#' )
#'}
outbreak_setup <- function(num_initial_cases,
                           incfn,
                           onset_to_isolation,
                           k,
                           prop_asym) {
  # Set up table of initial cases
  inc_samples <- incfn(num_initial_cases)

  case_data <- data.table(
    exposure = rep(0, num_initial_cases), # Exposure time of 0 for all initial cases
    asym = as.logical(rbinom(num_initial_cases, 1, prop_asym)),
    caseid = 1:(num_initial_cases), # set case id
    infector = 0,
    missed = TRUE,
    onset = inc_samples,
    new_cases = NA
  )

  # set isolation time for cluster to minimum time of onset of symptoms + draw
  # from delay distribution
  case_data <- case_data[, isolated_time := onset + onset_to_isolation(1)
                         ][, isolated := FALSE]

  case_data$isolated_time[case_data$asym] <- Inf

  # return
  return(case_data)
}
