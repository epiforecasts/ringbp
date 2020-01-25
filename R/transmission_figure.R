#' Produces first figure from summary
#'
#' @return
#' @export
#' @importFrom dplyr mutate
#' @importFromm tibble tibble
#' @importFrom ggplot2 ggplot geom_ribbon xlab theme_bw aes
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
                  post_symp_trans = infect-pre_symp_trans)

  dists %>%# full_join(trans_prevented_by_iso,by="x") %>%
    ggplot(aes(x=days_since_infection)) +
    xlab("days since infection") +
    geom_ribbon(aes(ymax=infect, ymin = pre_symp_trans + post_symp_trans*(1-delay)), fill="green4") +
    geom_ribbon(aes(ymax=pre_symp_trans,ymin=0),fill="blue") +
    geom_ribbon(aes(ymax=pre_symp_trans+post_symp_trans*(1-delay),ymin=pre_symp_trans),fill="red") +
    theme_bw()
}

