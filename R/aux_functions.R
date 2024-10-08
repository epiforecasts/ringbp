#' Create partial function to sample from gamma distributions
#' @author Joel Hellewell
#' @param dist_shape numeric shape parameter of Weibull distribution
#' @param dist_scale numeric scale parameter of Weibull distribution
#'
#' @return partial function that takes a numeric argument for number of samples
#' @export
#' @importFrom purrr partial
#'
dist_setup <- function(dist_type, dist_shape = NULL, dist_scale = NULL) {
  out <- purrr::partial(
    dist_type,shape = dist_shape, scale = dist_scale)
  
  return(out)
}


#' Samples the serial interval for given incubation period samples
#'
#' @param exp_samp vector of the exposure time of the infector
#' @param k numeric skew parameter for sampling the serial interval from the incubation period
#'
#' @export

inf_fn <- function(exp_samp = NULL, si_mean = NULL, si_sd = NULL) {

  out <- exp_samp + rlnorm(n = length(exp_samp), meanlog = si_mean, sdlog = si_sd)

  return(out)
}

#' Calculate proportion of runs that have controlled outbreak
#'
#' @author Joel Hellewell
#' @export
#' @inheritParams detect_extinct
extinct_prob <- function(outbreak_df_week = NULL, cap_cases  = NULL, week_range = 12:16) {

  n_sim <- max(outbreak_df_week$sim)

  extinct_runs <- detect_extinct(outbreak_df_week, cap_cases, week_range)
  out <-  sum(extinct_runs$extinct) / n_sim

  return(out)
}


#' Calculate whether outbreaks went extinct or not
#' @author Joel Hellewell
#' @param outbreak_df_week data.table  weekly cases produced by the outbreak model
#' @param cap_cases integer number of cumulative cases at which the branching process was terminated
#' @param week_range integer vector giving the (zero indexed) week range to test for whether an extinction occurred.
#' @importFrom data.table as.data.table fifelse
#'
#' @export
#'
detect_extinct <- function(outbreak_df_week  = NULL, cap_cases  = NULL, week_range = 12:16) {

  outbreak_df_week <- as.data.table(outbreak_df_week)
  outbreak_df_week <- outbreak_df_week[week %in% week_range]
  outbreak_df_week[, list(
    extinct = fifelse(all(weekly_cases == 0 & cumulative < cap_cases), 1, 0)
  ), by = sim]
}
