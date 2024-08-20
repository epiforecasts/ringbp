#' Set up initial cases for branching process
#' @author Joel Hellewell
#'
#' @inheritParams outbreak_model
#' @inheritParams outbreak_step
#' @param k Numeric skew parameter for sampling the serial interval from the
#' incubation period
#'
#' @return data.table of cases in outbreak so far
#' @export
#' @importFrom data.table data.table
#' @importFrom stats rbinom
#'
#' @examples
#'
#'\dontrun{
#' # incubation period sampling function
#' incfn <- dist_setup(dist_shape = 2.322737,dist_scale = 6.492272)
#' # delay distribution sampling function
#' delayfn <- dist_setup(delay_shape, delay_scale)
#' outbreak_setup(num_initial_cases = 5,incfn,delayfn,k=1.95,prop_asym=0)
#'}
outbreak_setup <- function(num_initial_cases, incfn, delayfn, k, prop_asym) {
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
  case_data <- case_data[, isolated_time := onset + delayfn(1)
                         ][, isolated := FALSE]

  case_data$isolated_time[case_data$asym] <- Inf

  # return
  return(case_data)
}
