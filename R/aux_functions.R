#' Create partial function to sample from gamma distributions
#' @author Joel Hellewell
#' @param dist_shape numeric shape parameter of Weibull distribution
#' @param dist_scale numeric scale parameter of Weibull distribution
#'
#' @return partial function that takes a numeric argument for number of samples
#' @export
#' @importFrom purrr partial
#'
dist_setup <- function(dist_shape = NULL, dist_scale = NULL) {
  out <- purrr::partial(rweibull,
                 shape = dist_shape,
                 scale = dist_scale)
  return(out)
}


#' Samples the serial interval for given incubation period samples
#'
#' @param inc_samp vector of samples from the incubation period distribution
#' @param k numeric skew parameter for sampling the serial interval from the incubation period
#'
#' @export
#' @importFrom sn rsn

inf_fn <- function(inc_samp = NULL, k = NULL) {

  out <- sn::rsn(n = length(inc_samp),
                 xi = inc_samp,
                 omega = 2,
                 alpha = k)

  out <- ifelse(out < 1, 1, out)

  return(out)
}

#' Calculate proportion of runs that have controlled outbreak
#'
#' @author Joel Hellewell
#' @export
#' @inheritParams detect_extinct
extinct_prob <- function(outbreak_df_week = NULL, cap_cases  = NULL, week_range = 12:16) {

  n_sim <- max(outbreak_df_week$sim)

  out <- outbreak_df_week %>%
    # new variable extinct = 1 if cases in weeks 10-12 all 0, 0 if not
    detect_extinct(cap_cases, week_range) %>%
    # number of runs where extinct = TRUE / number of runs
    .$extinct %>%
    sum(.) / n_sim

  return(out)
}


#' Calculate whether outbreaks went extinct or not
#' @author Joel Hellewell
#' @param outbreak_df_week data.table  weekly cases produced by the outbreak model
#' @param cap_cases integer number of cumulative cases at which the branching process was terminated
#' @param week_range integer vector giving the (zero indexed) week range to test for whether an extinction occurred.
#'
#' @export
#' @importFrom dplyr group_by filter summarise ungroup
#'
detect_extinct <- function(outbreak_df_week  = NULL, cap_cases  = NULL, week_range = 12:16) {

  outbreak_df_week %>%
    dplyr::group_by(sim) %>% # group by simulation run
    dplyr::filter(week %in% week_range) %>%
    dplyr::summarise(extinct =
                       ifelse(all(weekly_cases == 0 &
                                    cumulative < cap_cases),
                              1, 0)) %>%
    dplyr::ungroup()

}
