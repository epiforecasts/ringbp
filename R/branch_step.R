#' Move forward one generation in the branching process
#'
#' @author Joel Hellewell
#'
#' @param case_data A tibble of cases so far, can be created initially with branch_setup
#' @param total.cases Number of cases in case_data
#' @param extinct Whether the outbreak is extinct
#' @param disp.iso The dispersion parameter for isolated cases
#' @param disp.com The dispersion parameter for non-isolated cases
#' @param r0isolated The reproduction number for isolated cases
#' @param r0community The reproduction number for non-isolated cases
#' @param incfn A function that samples from the incubation period
#' @param delayfn A function that samples from the onset-to-hospitalisation delay
#' @param prop.ascertain The proportion of infectious contacts ascertained by contact tracing
#' @param k The skew parameter for sampling the serial interval from the incubation period
#' @param quarantine Whether quarantine is in effect, if TRUE then traced contacts are isolated before symptom onset
#'
#' @importFrom data.table data.table rbindlist
#' @importFrom purrr map2 map2_dbl map_lgl
#'
#' @return
#' @export
#'
#' @examples
#'
branch_step_single <- function(case_data,total.cases,extinct,
                        disp.iso,disp.com,r0isolated,r0community,prop.asym,
                        incfn,delayfn,prop.ascertain,k,quarantine){

  # A vectorised version of isTRUE
  vect_isTRUE <- function(x) {
    purrr::map_lgl(x, isTRUE)
  }

  vect_max <- function(x,y) {
    purrr::map2_dbl(x,y,max)
  }

  vect_min <- function(x,y) {
    purrr::map2_dbl(x,y,min)
  }

  # For each case in case_data, draw new_cases from a negative binomial distribution
  # with an R0 and dispersion dependent on if isolated=TRUE
  case_data[,new_cases := purrr::map2_dbl(
    ifelse(vect_isTRUE(isolated), disp.iso, disp.com),
    ifelse(vect_isTRUE(isolated), r0isolated, r0community), ~ rnbinom(1, size = .x, mu = .y))
    ]

  # Select cases that have generated any new cases
  new_case_data <- case_data[new_cases>0]
  # The total new cases generated
  total_new_cases <- case_data[,sum(new_cases),]

  # If no new cases drawn, outbreak is over so return case_data
  if(total_new_cases==0){
    # If everyone is isolated it means that either control has worked or everyone has had a chance to infect but didn't
    case_data$isolated <- TRUE

    effective_r0 <- 0
    cases_in_gen <- 0
    out <- list(case_data, effective_r0, cases_in_gen)
    names(out) <- c("cases", "effective_r0", "cases_in_gen")

    return(out)
  }

  # Compile a data.table for all new cases, new_cases is the amount of people that each infector has infected
  inc_samples <- incfn(total_new_cases)

  prob_samples <- data.table(
    # time when new cases were exposed, a draw from serial interval based on infector's onset
    exposure = unlist(purrr::map2(new_case_data$new_cases, new_case_data$onset,  function(x,y) {inf_fn(rep(y,x),k)})),
    # records the infector of each new person
    infector = unlist(purrr::map2(new_case_data$caseid, new_case_data$new_cases,  function(x,y) {rep(as.integer(x), as.integer(y))})),
    # records when infector was isolated
    infector_iso_time = unlist(purrr::map2(new_case_data$isolated_time,new_case_data$new_cases, function(x,y) {rep(x, as.integer(y))})),
    # records if infector asymptomatic
    infector_asym = unlist(purrr::map2(new_case_data$asym,new_case_data$new_cases, function(x,y){rep(x,y)})),
    # draws a sample to see if this person is asymptomatic
    asym = purrr::rbernoulli(n = total_new_cases, p = prop.asym),
    # draws a sample to see if this person is traced
    missed = purrr::rbernoulli(n = total_new_cases, p = 1-prop.ascertain),
    # sample from the incubation period for each new person
    incubfn_sample = inc_samples,
    isolated = FALSE,
    new_cases = NA
  )


  prob_samples <- prob_samples[exposure < infector_iso_time][, # filter out new cases prevented by isolation
                                             `:=`(# onset of new case is exposure + incubation period sample
                                               onset = exposure + incubfn_sample)]


  # cases whose parents are asymptomatic are automatically missed
  prob_samples$missed[vect_isTRUE(prob_samples$infector_asym)] <- TRUE

  # If you are asymptomatic, your isolation time is Inf
  prob_samples[,isolated_time := ifelse(vect_isTRUE(asym),Inf,
                                        # If you are not asymptomatic, but you are missed,
                                        # you are isolated at your symptom onset
                                        ifelse(vect_isTRUE(missed),onset + delayfn(1),
                                               # If you are not asymptomatic and you are traced,
                                               # you are isolated at max(onset,infector isolation time) # max(onset,infector_iso_time)
                                               ifelse(!vect_isTRUE(rep(quarantine,total_new_cases)),vect_min(onset + delayfn(1),
                                                                                                             vect_max(onset,infector_iso_time)),
                                                      infector_iso_time)))]


  # Chop out unneeded sample columns
  prob_samples[,c("incubfn_sample","infector_iso_time","infector_asym"):=NULL]
  # Set new case ids for new people
  prob_samples$caseid <- (nrow(case_data)+1):(nrow(case_data)+nrow(prob_samples))

  ## Number of new cases
  cases_in_gen <- nrow(prob_samples)

  ## Estimate the effective r0
  effective_r0 <- nrow(prob_samples) / nrow(case_data[!vect_isTRUE(case_data$isolated)])

  # Everyone in case_data so far has had their chance to infect and are therefore considered isolated
  case_data$isolated <- TRUE

  # bind original cases + new secondary cases
  case_data <- rbindlist(list(case_data,prob_samples),use.names=TRUE)

  # Return
  out <- list(case_data, effective_r0, cases_in_gen)
  names(out) <- c("cases", "effective_r0", "cases_in_gen")

  return(out)
}
