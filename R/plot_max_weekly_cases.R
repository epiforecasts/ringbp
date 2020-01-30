#' Plot maximum weekly cases in extinct outbreaks
#'
#' @param results
#' @param cap_cases
#' @param extinct_thresold
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr group_by mutate ungroup filter left_join summarise select
#' @importFrom tidyr unnest
#' @importFrom ggplot2 ggplot aes guide_colorbar guides theme labs theme_bw
#'
plot_max_weekly_cases <- function(results = NULL, cap_cases = 5000, extinct_thresold = 0.5) {


# Pull out max weekly cases and total_cases from
# each outbreak that was not declared extinct
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
  dplyr::filter(control_effectiveness >= 0.8) %>%
  dplyr::filter(latent %in% c("short", "medium")) %>%
  dplyr::group_by(scenario) %>%
  dplyr::filter(max_weekly_cases > quantile(max_weekly_cases, 0.025),
                max_weekly_cases < quantile(max_weekly_cases, 0.975)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(delay %in% "short") %>%
  rename_variables_for_plotting() %>%
  dplyr::mutate(control_effectiveness =
                  round(control_effectiveness* 100, 1) %>%
                  factor(levels = c("60", "80", "100"),
                         labels = c(
                           expression(""~"60%"),
                           expression(""~"80%"),
                           expression(""~"100%")
                         ))
                  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = max_weekly_cases, y = factor(index_R0), fill = stat(x))) +
  ggridges::geom_density_ridges_gradient(quantile_lines = TRUE, quantiles = c(0.5)) +
  ggplot2::facet_grid(rows = ggplot2::vars(latent),
                      cols = ggplot2::vars(control_effectiveness),
                      labeller = ggplot2::label_parsed) +
  ggplot2::scale_fill_continuous(type = "viridis", direction = -1, option = "plasma") +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "Maximum weekly cases",
                y = "Proportion of infected contacts ascertained by contact tracing") +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::guides(fill = ggplot2::guide_colorbar(title = ""))

return(plot)
}
