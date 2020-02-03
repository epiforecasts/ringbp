transmission_figure <- function(){

  ## Set up gamma with correct shape and cale
  make_gamma <- function(x, shape_scale_params = NULL, gamma = NULL) {
    gamma(x, shape = shape_scale_params[1] / (shape_scale_params[2]^2 / shape_scale_params[1]),
          scale = (shape_scale_params[2]^2 / shape_scale_params[1]))
  }

  ## Make distributions
  dists <- tibble::tibble(days_since_infection = seq(0,15,0.1)) %>%
    dplyr::mutate(
      incubation = make_gamma(days_since_infection,  c(6.7,2), dgamma),
      infectious = make_gamma(days_since_infection,c(9,2), dgamma),
      # delay = make_gamma(days_since_infection,  c(8,2), pgamma)
    )

  scale_colors <- viridis_pal(option = "plasma")(10)[c(3,7)]
  dists %>%
    tidyr::gather(key = "type", value = "value", -days_since_infection) %>%
    dplyr::filter(type %in% c("incub", "infect")) %>%
    # dplyr::mutate(type = factor(type, levels = c("infect", "all_trans", "pre_symp_trans")) %>%
    #                 forcats::fct_recode(`Prevented by isolation` = "infect",
    #                                     `Post symptom onset but pre isolation` = "all_trans",
    #                                     `Pre symptom onset` = "pre_symp_trans")) %>%
    ggplot2::ggplot(aes(x = days_since_infection, y = value, fill = type)) +
    ggplot2::geom_density(alpha = 0.8, stat = "identity") +
    ggplot2::theme_minimal() +
    ggplot2::scale_colour_manual(scale_colors) +
    ggplot2::theme(legend.position = "top") +
    ggplot2::labs(x = "Days since infection",
                  y = "Infectiousness") +
    ggplot2::guides(
      fill = ggplot2::guide_legend(title = "Distribution",
                                   ncol = 2,
                                   nrow = 2))
}

