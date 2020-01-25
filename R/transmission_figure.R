#' Produces first figure from summary
#'
#' @return
#' @export
#' @importFrom dplyr mutate filter
#' @importFromm tibble tibble
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot theme_minimal aes geom_density scale_fill_viridis_d theme labs guides guide_legend
#' @examples
#'
#' ## Generate figure
#'transmission_figure()
transmission_figure <- function(){

  ## Set up gamma with correct shape and cale
  make_gamma <- function(x, shape_scale_params = NULL, gamma = NULL) {
    gamma(x, shape = shape_scale_params[1] / (shape_scale_params[2]^2 / shape_scale_params[1]),
           scale = (shape_scale_params[2]^2 / shape_scale_params[1]))
  }

  ## Make distributions
  dists <- tibble::tibble(days_since_infection = seq(0,15,0.1)) %>%
    dplyr::mutate(
      incub = make_gamma(days_since_infection,  c(6,2), pgamma),
      infect = make_gamma(days_since_infection,c(6,2), dgamma),
      delay = make_gamma(days_since_infection,  c(8,2), pgamma)
    ) %>%
    dplyr::mutate(pre_symp_trans = (1-incub) * infect,
                  post_symp_trans = infect-pre_symp_trans,
                  all_trans = pre_symp_trans + post_symp_trans * (1-delay))


  dists %>%
    tidyr::gather(key = "type", value = "value", -days_since_infection) %>%
    dplyr::filter(!type %in% c("delay", "incub", "post_symp_trans")) %>%
    dplyr::mutate(type = factor(type, levels = c("infect", "all_trans", "pre_symp_trans"))) %>%
    ggplot2::ggplot(aes(x = days_since_infection, y = value, fill = type)) +
    ggplot2::geom_density(alpha = 0.8, stat = "identity") +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_viridis_d("plasma") +
    ggplot2::theme(legend.position = "top") +
    ggplot2::labs(x = "Days since infection",
         y = "Infectiousness") +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Rate"))
}

