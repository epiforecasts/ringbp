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
#' @importFrom purrr map2 map2_dbl pmap map_lgl
#' @examples
#'
branch_step_single <- function(case_data,total.clusters,total.cases,extinct,
                        disp.iso,disp.com,r0isolated,r0community,
                        incfn,delayfn,prop.ascertain,k,quarantine){

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

  prob_samples <- data.table(
    # time when new cases were exposed, a draw from serial interval based on infector's onset
    exposure = unlist(map2(new_case_data$new_cases, new_case_data$onset,  function(x,y) {inf_fn(rep(y,x),k)})),
    # records the infector of each new person
    infector = unlist(map2(new_case_data$caseid, new_case_data$new_cases,  function(x,y) {rep(as.integer(x), as.integer(y))})),
    # records when infector was isolated
    infector_iso_time = unlist(map2(new_case_data$isolated_time,new_case_data$new_cases, function(x,y) {rep(x, as.integer(y))})),
    # assigns the cluster of the infector to the new person
    cluster = unlist(map2(new_case_data$cluster, new_case_data$new_cases,  function(x,y) {rep(as.integer(x), as.integer(y))})),
    # draws a sample to see if this person remains in the cluster
    bernoulli_sample = purrr::rbernoulli(n = total_new_cases, p = 1-prop.ascertain),
    # sample from the incubation period for each new person
    incubfn_sample = inc_samples,
    isolated = FALSE
  )


  prob_samples <- prob_samples[exposure < infector_iso_time][, # filter out new cases prevented by isolation
                                             `:=`(# onset of new case is exposure + incubation period sample
                                               onset = exposure + incubfn_sample,
                                               # They are missed with probability 1-prob(detected by contact tracing)
                                               missed = bernoulli_sample)]


  if(isFALSE(quarantine)){
    # SCENARIO 1: If you are contact traced, you are isolated when symptomatic
    prob_samples[,isolated_time := ifelse(vect_isTRUE(missed), onset + delayfn(1), onset)]
  }else{
    # SCENARIO 2: If you are contact traced, you are isolated regardless of symptoms
    prob_samples[,isolated_time := ifelse(vect_isTRUE(missed), onset + delayfn(1), infector_iso_time)]
  }

  # Chop out unneeded sample columns
  prob_samples[,c("incubfn_sample","bernoulli_sample","infector_iso_time"):=NULL]
  # Set new case ids for new people
  prob_samples$caseid <- (nrow(case_data)+1):(nrow(case_data)+nrow(prob_samples))
  # Sets the cluster number for new people that are missed to NA
  prob_samples$cluster <- ifelse(vect_isTRUE(prob_samples$missed),NA,prob_samples$cluster)

  ## get number of new clusters
  num.new.clusters <- sum(is.na(prob_samples$cluster))
  ## assign new cluster numbers to missed cases
  prob_samples$cluster[is.na(prob_samples$cluster) == TRUE] <- (total.clusters + 1):(num.new.clusters + total.clusters)

  # Everyone in case_data so far has had their chance to infect and are therefore considered isolated
  case_data$isolated <- TRUE

  # Tidy up columns of original data passed in
  case_data[,new_cases:=NULL]

  # print("case_data")
  # print(case_data)
  # print("prob_samples")
  # print(prob_samples)
  # bind original cases + new secondary cases
  case_data <- rbindlist(list(case_data,prob_samples),use.names=TRUE)
  # return
  return(case_data)

}
