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
#' @param incub_shape Mean of incubation period distribution
#' @param incub_scale scale of incubation period distribution
#' @param inf_shape shape of time until infectious distribution
#' @param inf_scale scale of time until infectious distribution
#' @param delay_shape shape of distribution for delay between symptom onset and isolation
#' @param delay_scale scale of distribution for delay between symptom onset and isolation
#'
#' @importFrom purrr safely
#' @return
#' @export
#'
#' @examples
#'
wuhan_sim <- function(n.sim,prop.ascertain,cap_max_days,cap_cases,r0isolated,
                      r0community,disp.iso,disp.com,mu_ip,sd_ip,k,
                      mu_si,sd_si,delay_shape,delay_scale,num.initial.cases,
                      num.initial.clusters){

  # Run n.sim number of model runs and put them all together in a big data.frame
  res <- purrr::map(.x = 1:n.sim, ~ outbreak_model(num.initial.cases = num.initial.cases,
                                             num.initial.clusters = num.initial.clusters,
                                             prop.ascertain = prop.ascertain,
                                             cap_max_days = cap_max_days,
                                             cap_cases = cap_cases,
                                             r0isolated = r0isolated,
                                             r0community = r0community,
                                             disp.iso = disp.iso,
                                             disp.com = disp.com,
                                             delay_shape = delay_shape,
                                             delay_scale = delay_scale,
                                             k = k))


  # bind output together and add simulation index
  res <- data.table::rbindlist(res)
  res[,sim := rep(1:n.sim,rep(floor(cap_max_days/7),n.sim)),]

}





