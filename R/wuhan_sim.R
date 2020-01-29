#' Run a specified number of simulations with identical parameters
#' @author Joel Hellewell
#' @param n.sim
#' @param prop.ascertain
#' @param cap_max_days
#' @param cap_cases
#' @param r0isolated
#' @param r0community
#' @param disp.iso
#' @param disp.com
#' @param incub_mean
#' @param incub_var
#' @param inf_mean
#' @param inf_var
#' @param delay_mean
#' @param delay_var
#' @param num.initial.cases
#' @param num.initial.clusters
#'
#' @return
#' @export
#'
#' @examples
#'
wuhan_sim <- function(n.sim,prop.ascertain,cap_max_days,cap_cases,r0isolated,
                      r0community,disp.iso,disp.com,incub_mean,incub_var,
                      inf_mean,inf_var,delay_mean,delay_var,num.initial.cases,
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
                                             incub_mean = incub_mean,
                                             incub_var = incub_var,
                                             inf_mean = inf_mean,
                                             inf_var = inf_var,
                                             delay_mean = delay_mean,
                                             delay_var = delay_var))


  # bind output together and add simulation index
  res <- data.table::rbindlist(res)
  res[,sim := rep(1:n.sim,rep(floor(cap_max_days/7),n.sim)),]

}





