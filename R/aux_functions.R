#' Create partial function to sample from gamma distributions
#' @author Joel Hellewell
#' @param dist_param1 numeric parameter of specified distribution
#' @param dist_param2 numeric parameter of specified distribution
#' @param dist_type type of distribution from: 'weibull', 'gamma', 'lognormal'
#'
#' @return partial function that takes a numeric argument for number of samples
#' @export
#' @importFrom purrr partial
#' @examples
#'
dist_setup <- function(dist_param1 = NULL, dist_param2 = NULL, dist_type = NULL) {
  if(dist_type == "weibull"){
    out <- purrr::partial(rweibull,
                          shape = dist_param1,
                          scale = dist_param2)
  }
  if(dist_type == "gamma"){
    out <- purrr::partial(rgamma,
                          shape = dist_param1,
                          rate = dist_param2)
  }
  if(dist_type == "lognormal"){
    out <- purrr::partial(rlnorm,
                          meanlog = dist_param1,
                          sdlog = dist_param2)
  }
  return(out)
}


#' Samples the serial interval for given incubation period samples
#'
#' @param inc_samp vector of samples from the incubation period distribution
#' @param inf_shape shape parameter for sampling the serial interval from the incubation period
#' @param inf_rate rate parameter for sampling the serial interval from the incubation period
#' @param inf_shift shift parameter, describing number of days pre-symptoms can be infectious
#'
#' @return
#' @export
#' @importFrom sn rsn
#' @examples
#'
inf_fn <- function(inc_samp = NULL, inf_shape = NULL, inf_rate = NULL, inf_shift = NULL) {

  out <- inc_samp - inf_shift + rgamma(n = length(inc_samp),
                                shape = inf_shape,
                                rate = inf_rate)

  out <- ifelse(out < 1, 1, out)

  return(out)
}

#' Calculate proportion of runs that have controlled outbreak
#'
#' @author Joel Hellewell
#' @return
#' @export
#' @inheritParams detect_extinct
#' @examples
#'
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


#' Calculate proportion of outbreaks that went extinct
#' @author Joel Hellewell
#' @param outbreak_df_week data.table  weekly cases producted by the outbreak model
#' @param cap_cases integer number of cumulative cases at which the branching process was terminated
#'
#' @return
#' @export
#' @importFrom dplyr group_by filter summarise ungroup
#' @examples
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


#' Create sub-plot for supplementary figures
#'
#' @param theta.in character filtering value for theta
#' @param delay.in character filtering value for delay
#' @param prop.asym.in numeric filtering value for proportion of asymptomatic cases
#' @param num.initial.cases.in integer filtering value for number of initial cases
#' @param index_R0.in numeric filtering value for community R0 value
#' @param res.in data.table of results from parameter sweep
#'
#' @return
#' @export
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 ggplot aes geom_line geom_point facet_wrap ylab xlab scale_x_continuous scale_y_continuous coord_cartesian
#' @importFrom cowplot panel_border
#'
#' @examples
#'
sub_plot <- function(delay.in = "SARS",
                     prop.asym.in = 0.4,
                     num.initial.cases.in = 20,
                     index_R0.in = 1.1,
                     res.in = NULL,
                     facet.by = NULL,
                     col.by = NULL) {

  col.by <- ggplot2::ensym(col.by)

  res.in %>%
    dplyr::filter(delay %in% delay.in,
                  prop.asym %in% prop.asym.in,
                  num.initial.cases %in% num.initial.cases.in,
                  index_R0 %in% index_R0.in) %>%
    # Ugly factor re-naming
    dplyr::mutate(num.initial.cases = factor(num.initial.cases,
                                             levels = c(5, 20, 40),
                                             labels = c("5 cases",
                                                        "20 cases",
                                                        "40 cases"))) %>%
    dplyr::mutate(delay = factor(delay,
                                 levels = c("SARS", "Wuhan"),
                                 labels = c("Short isolation delay",
                                            "Long isolation delay"))) %>%
    dplyr::mutate(prop.asym = factor(prop.asym,
                                     levels = c(0.2, 0.4, 0.5, 0.7),
                                     labels = c("20% cases asymptomatic",
                                                "40%","50%","70%"))) %>%
    # dplyr::mutate(theta = factor(theta,
    #                              levels = c(3),
    #                              labels = c("Trans up to 3 days pre-onset"))) %>%
    # Put plot together
    ggplot2::ggplot(ggplot2::aes(x = control_effectiveness,
                                 y = pext,
                                 color = as.factor(!!col.by))) +
    ggplot2::geom_line(size = 0.75) +
    ggplot2::geom_point(shape = 21,
                        col = "black",
                        ggplot2::aes(fill = as.factor(!!col.by)), size = 3) +
    ggplot2::facet_wrap(as.formula(paste(". ~", facet.by))) +
    ggplot2::ylab("Simulated outbreaks controlled (%)") +
    ggplot2::xlab("Contacts traced (%)") +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2),
                                labels = seq(0, 100, 20)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2),
                                labels = seq(0, 100, 20)) +
    cowplot::panel_border() +
    ggplot2::coord_cartesian(ylim = c(0, 1))

}
