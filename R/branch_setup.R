
#' Title
#'
#' @param num.initial.cases
#' @param num.initial.clusters
#' @param incubfn
#' @param infecfn
#' @param delayfn
#'
#' @return
#' @export
#' @importFrom data.table data.table
#' @examples
branch_setup <- function(num.initial.cases,num.initial.clusters,incubfn,infecfn,delayfn){
  # Set up table of initial cases
  case_data <- data.table(exposure = rep(0, num.initial.cases * num.initial.clusters), # Exposure time of 0 for all initial cases
                          onset = incubfn(num.initial.cases * num.initial.clusters), # Draw symptom onset for all initial
                          latent = infecfn(num.initial.cases * num.initial.clusters), # Draw when initial cases infect people
                          cluster = rep(1:num.initial.clusters, rep(num.initial.cases, num.initial.clusters)), # Set cluster number
                          missed = rep(FALSE, num.initial.cases * num.initial.clusters), # All initial cases are known
                          caseid = 1:(num.initial.cases * num.initial.clusters), # set case id
                          infector = 0)

  # set isolation time for cluster to minimum time of onset of symptoms + draw from delay distribution
  case_data <- case_data[,isolated_time := min(onset) + delayfn(1),cluster
                         # isolated if time when infecting < isolation time
                         ][,isolated := ifelse(latent > isolated_time, TRUE, FALSE)]
  # return
  return(case_data)
}
