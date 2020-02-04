#' Step forward a generation in the branching process
#' @author Joel Hellewell
#' @param case_data
#' @param total.clusters
#' @param total.cases
#' @param extinct
#'
#' @return
#' @export
#' @importFrom data.table data.table rbindlist
#' @importFrom purrr map2 map2_dbl
#' @examples
#'
branch_step <- function(case_data,total.clusters,total.cases,extinct,
                        disp.iso,disp.com,r0isolated,r0community,
                        incfn,delayfn,prop.ascertain,k){

  # A vectorised version of isTRUE
  vect_isTRUE <- function(x) {
    purrr::map_lgl(x, isTRUE)
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
  if(nrow(new_case_data)==0){
    # new_cases removed so it can be sewn back on to other data.tables
    case_data[,new_cases:=NULL]
    # If everyone is isolated it means that either control has worked or everyone has had a chance to infect but didn't
    case_data$isolated <- TRUE
    return(case_data)
  }

  # Compile a data.table for all new cases, new_cases is the amount of people that each infector has infected
  inc_samples <- incfn(total_new_cases)
  lat_samples <- inf_fn(inc_samples,k)

  prob_samples <- data.table(
    # new people are given the exposure time equal to their infector's draw from the latent period
    exposure = unlist(map2(new_case_data$exposure, new_case_data$new_cases,  function(x,y) {x + inf_fn(y))})),
    # records the infector of each new person
    infector = unlist(map2(new_case_data$caseid, new_case_data$new_cases,  function(x,y) {rep(as.integer(x), as.integer(y))})),
    # assigns the cluster of the infector to the new person
    cluster = unlist(map2(new_case_data$cluster, new_case_data$new_cases,  function(x,y) {rep(as.integer(x), as.integer(y))})),
    # draws a sample to see if this person remains in the cluster
    bernoulli_sample = purrr::rbernoulli(n = total_new_cases, p = 1-prop.ascertain),
    # sample from the incubation period for each new person
    incubfn_sample = inc_samples,
    # sample from the latent period for each new person
    infecfn_sample = lat_samples
  )

  # new person's time when they infect is their exposure + latent period sample
  prob_samples[,`:=`(latent = exposure + infecfn_sample,
                     # their onset is exposure + incubation period sample
                     onset = exposure + incubfn_sample,
                     # They are missed with probability 1-prob(detected by contact tracing)
                     missed = bernoulli_sample)]

  # Isolation time for each cluster is the minimum onset in the cluster + a draw from delay distribution
  prob_samples[,isolated_time := min(onset) + delayfn(1),infector]

  # For people who remain in cluster, they are isolated if their time when they infect is before the cluster's isolation time
  prob_samples[,isolated := ifelse(vect_isTRUE(missed),FALSE,ifelse(latent > isolated_time, TRUE, FALSE))]
  # Chop out unneeded sample columns
  prob_samples[,c("incubfn_sample","infecfn_sample","bernoulli_sample"):=NULL]
  # Set new case ids for new people
  prob_samples$caseid <- (nrow(case_data)+1):(nrow(case_data)+nrow(prob_samples))
  # Sets the cluster number for new people that are missed to NA
  prob_samples$cluster <- ifelse(vect_isTRUE(prob_samples$missed),NA,prob_samples$cluster)

  ## get number of new clusters
  num.new.clusters <- sum(is.na(prob_samples$cluster))
  ## assign new cluster numbers to missed cases
  prob_samples$cluster[is.na(prob_samples$cluster) == TRUE] <- (total.clusters + 1):(num.new.clusters + total.clusters)

  # Everyone is case_data so far has had their chance to infect and are therefore considered isolated
  case_data$isolated <- TRUE

  # new table of missed people, each will start their new cluster
  prob_cpy <- prob_samples[vect_isTRUE(missed)]

  # draw numbers of cases in each cluster (they should all be not isolated since they were missed)
  prob_cpy[,new_cases := purrr::map2_dbl(ifelse(vect_isTRUE(isolated), disp.iso, disp.com),
                                        ifelse(vect_isTRUE(isolated), r0isolated, r0community), ~ rnbinom(1, size = .x, mu = .y)),]

  # Filter to clusters with new cases
  new_case_data <- prob_cpy[new_cases>0]

  # If there are no new cases, outbreak is over so clean up tables and return
  if(nrow(new_case_data)==0){
    case_data[,new_cases:=NULL]
    case_data <- rbindlist(list(case_data,prob_samples),use.names=TRUE)
    case_data$isolated <- TRUE
    return(case_data)
  }

  # Get number of new clusters
  num.new.clusters <- new_case_data[,sum(new_cases),]
  inc_samples <- incfn(num.new.clusters)
  lat_samples <- inf_fn(inc_samples,k)

  # Set exposure time, infector, cluster id, and get incubation + latent samples for each new person in each new cluster
  new_clusters <- data.table(
    exposure = unlist(map2(new_case_data$exposure, new_case_data$new_cases,  function(x,y) {x + inf_fn(y)})),
    infector = unlist(map2(new_case_data$caseid, new_case_data$new_cases,  function(x,y) {rep(as.integer(x), as.integer(y))})),
    cluster = unlist(map2(new_case_data$cluster, new_case_data$new_cases,  function(x,y) {rep(as.integer(x), as.integer(y))})),
    # sample from the incubation period for each new person
    incubfn_sample = inc_samples,
    # sample from the latent period for each new person
    infecfn_sample = lat_samples,
    missed = FALSE # everyone here is missed by default since they are in a new cluster
  )

  # Set latent and onset times for each new person in each new cluster
  new_clusters[,`:=`(latent = exposure + infecfn_sample,
                     onset = exposure + incubfn_sample)]

  # Set isolation time for cluster
  new_clusters[,isolated_time := min(onset + delayfn(1)),infector]
  # Isolated if time when infecting < isolation time
  new_clusters[,isolated := ifelse(latent > isolated_time, TRUE, FALSE)]
  # Tidy up columns
  new_clusters[,c("incubfn_sample","infecfn_sample"):=NULL]
  # Set new case ids
  new_clusters$caseid <- (nrow(case_data)+nrow(prob_samples)+1):(nrow(case_data)+nrow(prob_samples)+nrow(new_clusters))

  # Tidy up columns of original data passed in
  case_data[,new_cases:=NULL]
  # all secondary case are isolated because they have had their chance to infect
  # now the only non-isolated people are those who infected before cluster isolation time just above
  prob_samples$isolated <- TRUE
  # bind original cases + new secondary cases + new clusters from secondary cases
  case_data <- rbindlist(list(case_data,prob_samples,new_clusters),use.names=TRUE)
  # return
  return(case_data)

}
