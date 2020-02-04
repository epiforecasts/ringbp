#' Plot boxplots of weekly cases during outbreak
#' @author Amy Gimma
#' @param results Results of parameter sweep
#' @param cap_cases The cumulative number of cases at which the branching process was terminated
#' @param extinct_thresold Threshold to show boxplots when extinction probability is above
#' @param filt_control_effectiveness Cuts off minimum control effectiveness
#' @param flip_coords Binary variable to flip coordinates
#'
#' @return
#' @export
#'
#' @examples
#'
box_plot_max_weekly_cases <- function(results = NULL,
                                      cap_cases = 5000,
                                      extinct_thresold = 0.5,
                                      filt_control_effectiveness = 0.4,
                                      flip_coords = FALSE) {

  filt_results <- results %>%
    dplyr::group_by(scenario) %>%
    dplyr::mutate(prob_extinct = extinct_prob(sims[[1]],cap_cases = cap_cases)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(prob_extinct >  extinct_thresold) %>%
    dplyr::group_by(scenario) %>%
    dplyr::mutate(
      extinct = list(sims[[1]] %>%
                       detect_extinct(cap_cases = cap_cases) %>%
                       dplyr::filter(extinct == 1))) %>%

    dplyr::mutate(
      sum_meas = list(dplyr::left_join(extinct[[1]],
                                       sims[[1]],
                                       by = ("sim")) %>%
                        dplyr::group_by(sim) %>%
                        dplyr::summarise(max_weekly_cases = max(weekly_cases),
                                         total_cases = max(cumulative)))
    ) %>%
    dplyr::select(-extinct) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(c("sum_meas")) %>%
    dplyr::select(-sims)


  ## make plot
  plot <- filt_results %>%
    dplyr::filter(control_effectiveness >= filt_control_effectiveness) %>%
    dplyr::filter(theta %in% c("15%")) %>%
    dplyr::group_by(scenario) %>%
    dplyr::ungroup() %>%
    # dplyr::filter(delay %in% "short") %>%
    rename_variables_for_plotting()  %>%
    ggplot2::ggplot(ggplot2::aes(y = max_weekly_cases,
                                 x = factor(control_effectiveness),
                                 color = factor(control_effectiveness))) +
    # ggplot2::geom_jitter(
    #                      color = "grey34",
    #                      size = 0.005,
    #                      alpha = 0.3,
    #                      width = 0.4,
    #                      height = 0.005) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_grid(rows = ggplot2::vars(delay),
                        cols = ggplot2::vars(index_R0),
                        labeller = ggplot2::label_parsed) +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "Maximum weekly cases",
                  x = "Proportion of infected contacts ascertained by contact tracing") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_y_continuous(breaks = seq(0,1000,50)) +
    # CHOOSE COLOR SCALE MANUALLY
    ggthemes::scale_color_colorblind() + # colorblind including black
    # ggthemes::scale_color_economist() +
    # ggplot2::scale_color_discrete(colorblind_pal()(8)[-c(1)]) + # no black
    # ggplot2::scale_color_discrete(economist_pal()(8)[-c(1)]) + # no grey
    ggplot2::labs(fill = "Proportion of infected contacts ascertained by contact tracing",
      color = "Control Effectiveness")

  if (flip_coords) { plot <- plot + ggplot2::coord_flip() }

  return(plot)
}

