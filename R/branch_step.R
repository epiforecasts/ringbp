#' Title
#'
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
                        incubfn,infecfn,delayfn,prop.ascertain){

  vect_isTRUE <- function(x) {
    purrr::map_lgl(x, isTRUE)
  }

  case_data[,new_cases := purrr::map2_dbl(
    ifelse(vect_isTRUE(isolated), disp.iso, disp.com),
    ifelse(vect_isTRUE(isolated), r0isolated, r0community), ~ rnbinom(1, size = .x, mu = .y))
    ]

  new_case_data <- case_data[new_cases>0]
  total_new_cases <- case_data[,sum(new_cases),]

  if(nrow(new_case_data)==0){
    case_data[,new_cases:=NULL]
    case_data$isolated <- TRUE
    return(case_data)
  }

  prob_samples <- data.table(
    exposure = unlist(map2(new_case_data$latent, new_case_data$new_cases,  function(x,y) {rep(x, as.integer(y))})),
    infector = unlist(map2(new_case_data$caseid, new_case_data$new_cases,  function(x,y) {rep(as.integer(x), as.integer(y))})),
    cluster = unlist(map2(new_case_data$cluster, new_case_data$new_cases,  function(x,y) {rep(as.integer(x), as.integer(y))})),
    bernoulli_sample = 1-purrr::rbernoulli(n = total_new_cases, p = prop.ascertain),
    incubfn_sample = incubfn(total_new_cases),
    infecfn_sample = infecfn(total_new_cases)
  )


  prob_samples[,`:=`(latent = exposure + infecfn_sample,
                     onset = exposure + incubfn_sample,
                     missed = bernoulli_sample)]

  prob_samples[,isolated_time := min(onset + delayfn(1)),infector]

  prob_samples[,isolated := ifelse(vect_isTRUE(missed),FALSE,ifelse(latent > isolated_time, TRUE, FALSE))]
  prob_samples[,c("incubfn_sample","infecfn_sample","bernoulli_sample"):=NULL]
  prob_samples$caseid <- (nrow(case_data)+1):(nrow(case_data)+nrow(prob_samples))
  prob_samples$cluster <- ifelse(vect_isTRUE(prob_samples$missed),NA,prob_samples$cluster)

  ## get number of new clusters
  num.new.clusters <- sum(is.na(prob_samples$cluster))
  ## assign new cluster numbers to missed cases
  prob_samples$cluster[is.na(prob_samples$cluster) == TRUE] <- (total.clusters + 1):(num.new.clusters + total.clusters)


  case_data$isolated <- TRUE

  prob_cpy <- prob_samples[vect_isTRUE(missed)]

  prob_cpy[,
           new_cases := purrr::map2_dbl(ifelse(vect_isTRUE(isolated), disp.iso, disp.com),
                                        ifelse(vect_isTRUE(isolated), r0isolated, r0community), ~ rnbinom(1, size = .x, mu = .y)),]

  new_case_data <- prob_cpy[new_cases>0]

  if(nrow(new_case_data)==0){
    case_data[,new_cases:=NULL]
    case_data <- rbindlist(list(case_data,prob_samples),use.names=TRUE)
    case_data$isolated <- TRUE
    return(case_data)
  }

  num.new.clusters <- new_case_data[,sum(new_cases),]

  new_clusters <- data.table(
    exposure = unlist(map2(new_case_data$latent, new_case_data$new_cases,  function(x,y) {rep(x, as.integer(y))})),
    infector = unlist(map2(new_case_data$caseid, new_case_data$new_cases,  function(x,y) {rep(as.integer(x), as.integer(y))})),
    cluster = unlist(map2(new_case_data$cluster, new_case_data$new_cases,  function(x,y) {rep(as.integer(x), as.integer(y))})),
    incubfn_sample = incubfn(num.new.clusters),
    infecfn_sample = infecfn(num.new.clusters),
    missed = FALSE
  )

  new_clusters[,`:=`(latent = exposure + infecfn_sample,
                     onset = exposure + incubfn_sample)]

  new_clusters[,isolated_time := min(onset + delayfn(1)),infector]
  new_clusters[,isolated := ifelse(latent > isolated_time, TRUE, FALSE)]
  new_clusters[,c("incubfn_sample","infecfn_sample"):=NULL]
  new_clusters$caseid <- (nrow(case_data)+nrow(prob_samples)+1):(nrow(case_data)+nrow(prob_samples)+nrow(new_clusters))


  case_data[,new_cases:=NULL]
  prob_samples$isolated <- TRUE
  case_data <- rbindlist(list(case_data,prob_samples,new_clusters),use.names=TRUE)

  return(case_data)

}
