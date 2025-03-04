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
#'
#'\dontrun{
#' # incubation period sampling function
#' incfn <- dist_setup(dist_shape = 2.322737,dist_scale = 6.492272)
#' # delay distribution sampling function
#' delayfn <- dist_setup(delay_shape, delay_scale)
#' outbreak_setup(num.initial.cases = 5,incfn,delayfn,k=1.95,prop.asym=0)
#'}
outbreak_setup <- function(num.initial.cases, incfn, delayfn, k, prop.asym) {
  # Set up table of initial cases
  inc_samples <- incfn(num.initial.cases)

  case_data <- data.table(exposure = rep(0, num.initial.cases), # Exposure time of 0 for all initial cases
                          asym = as.logical(rbinom(num.initial.cases, 1, prop.asym)),
                          caseid = 1:(num.initial.cases), # set case id
                          infector = 0,
                          missed = TRUE,
                          onset = inc_samples,
                          new_cases = NA)

  # set isolation time for cluster to minimum time of onset of symptoms + draw from delay distribution
  case_data <- case_data[, isolated_time := onset + delayfn(1)
                         ][, isolated := FALSE]

  case_data$isolated_time[case_data$asym] <- Inf

  # return
  return(case_data)
}
