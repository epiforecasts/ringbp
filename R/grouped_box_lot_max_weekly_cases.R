#' @export
grouped_box_plot_max_weekly_cases <- function(results = NULL,
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
    dplyr::filter(latent %in% c("short", "medium")) %>%
    dplyr::group_by(scenario) %>%
    dplyr::ungroup() %>%
    # dplyr::filter(delay %in% "short") %>%
    rename_variables_for_plotting()  %>%
    ggplot2::ggplot(ggplot2::aes(y = max_weekly_cases,
                                 x = factor(control_effectiveness),
                                 color = as.factor(delay)
                                 )) +
    ggplot2::geom_boxplot(alpha = 0.4) +
    ggplot2::facet_grid(rows = ggplot2::vars(latent),
                        cols = ggplot2::vars(index_R0),
                        labeller = ggplot2::label_parsed) +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "Maximum weekly cases",
                  x = "Proportion of infected contacts ascertained by contact tracing") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_y_continuous(breaks = seq(0,1000,50)) +
    ggthemes::scale_color_economist() +
    ggplot2::labs(fill = "Proportion of infected contacts ascertained by contact tracing",
                  color = "Delay to isolation") +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = 1, nrow = 3), title.position = "top")

  if(flip_coords) { plot <- plot + ggplot2::coord_flip() }
plot
  return(plot)
}

