#' Set up initial cases for branching process
#' @author Joel Hellewell
#'
#' @param num.initial.cases Number of initial cases
#' @param num.initial.clusters Number of initial clusters
#' @param incfn A function that samples from the incubation period
#' @param delayfn A function that samples from the onset-to-hospitalisation delay
#' @param k The skew parameter for sampling the serial interval from the incubation period
#'
#' @return
#' @export
#' @importFrom data.table data.table
#'
#' @examples
#'
branch_setup <- function(num.initial.cases,num.initial.clusters,incfn,delayfn,k){
  # Set up table of initial cases
  inc_samples = incfn(num.initial.cases * num.initial.clusters)
  # lat_samples = inf_fn(inc_samples,k)

  case_data <- data.table(exposure = rep(0, num.initial.cases * num.initial.clusters), # Exposure time of 0 for all initial cases
                          onset = inc_samples, # Draw symptom onset for all initial
                          # latent = lat_samples, # Draw when initial cases infect people
                          cluster = rep(1:num.initial.clusters, rep(num.initial.cases, num.initial.clusters)), # Set cluster number
                          missed = rep(FALSE, num.initial.cases * num.initial.clusters), # All initial cases are known
                          caseid = 1:(num.initial.cases * num.initial.clusters), # set case id
                          infector = 0)

  # set isolation time for cluster to minimum time of onset of symptoms + draw from delay distribution
  case_data <- case_data[,isolated_time := onset + delayfn(1)
                         ][,isolated := FALSE]
  # return
  return(case_data)
}
