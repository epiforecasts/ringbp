#' Set up initial cases for branching process
#' @author Emma Davis and Tim Lucas (from Joel Hellewell)
#'
#' @param num.initial.cases Integer number of initial cases
#' @param incfn function that samples from incubation period Weibull distribution; generated using dist_setup
#' @param delayfn function generated using dist_setup = 1 or Inf (adherence to isolation)
#' @param prop.asym Numeric proportion of cases that are sublinical (between 0 and 1)
#'
#' @return data.table of cases in outbreak so far
#' @export
#' @importFrom data.table data.table
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
outbreak_setup <- function(num.initial.cases, incfn, delayfn, prop.asym, sensitivity, precaution, test_delay, self_report) {
  # Set up table of initial cases
  inc_samples <- incfn(num.initial.cases)

  case_data <- data.table(exposure = rep(0, num.initial.cases), # Exposure time of 0 for all initial cases
                          asym = purrr::rbernoulli(num.initial.cases, prop.asym),
                          caseid = 1:(num.initial.cases), # set case id
                          infector = 0,
                          missed = TRUE,
                          onset = inc_samples,
                          new_cases = NA,
                          test_result = NA)

  case_data <- case_data[asym==FALSE, test_result := purrr::rbernoulli(sum(1-asym), sensitivity)]
  case_data$missed[which(!is.na(case_data$test_result))] <- FALSE

  # set isolation time for cluster to minimum time of onset of symptoms + draw from delay distribution
  case_data <- case_data[, isolated_time := onset + delayfn(1)
                         ][, isolated_end := isolated_time+test_delay+ifelse(test_result==1 | asym==T,Inf,precaution)
                           ][, isolated := FALSE]

  case_data$isolated_time[case_data$asym] <- Inf

  # return
  return(case_data)
}
