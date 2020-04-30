#' Construct Figure S1 from the supplementary material
#' @author Joel Hellewell
#' @param res data.frame of results from scenarios from generate_analysis.R
#'
#' @return ggplot object
#' @export
#' @importFrom patchwork plot_spacer plot_layout
#' @importFrom cowplot theme_cowplot
#' @importFrom ggplot2 theme scale_fill_manual scale_color_manual element_blank
#'
#' @examples
#' \dontrun{
#'
#' sweep_results <- readRDS("data-raw/res.rds")
#' res <- sweep_results %>%
#'  dplyr::group_by(scenario) %>%
#'  dplyr::mutate(pext=extinct_prob(sims[[1]],cap_cases = 5000)) %>%
#'  dplyr::ungroup(scenario)
#'
#' make_figure_S1(results)
#' }
make_figure_S1 <- function(res = NULL) {

  sub_plotS1 <- purrr::partial(ringbp::sub_plot,
                               res.in = res,
                               index_R0.in = unique(res$index_R0),
                               col.by = "index_R0")

  # Initial cases
  index_ic <- sub_plotS1(facet.by = "num.initial.cases")
  ic1 <- sub_plotS1(num.initial.cases = 5,
                    facet.by = "num.initial.cases")
  ic2 <- sub_plotS1(num.initial.cases = 40,
                    facet.by = "num.initial.cases")

  # Delay distribution
  index_dl <- sub_plotS1(facet.by = "delay")
  dl1 <- sub_plotS1(delay = "Wuhan",
                    facet.by = "delay")


  # asmyptomatic
  index_as <- sub_plotS1(facet.by = "prop.asym")
  as1 <- sub_plotS1(prop.asym = 0.2,
                    facet.by = "prop.asym")
  as2 <- sub_plotS1(prop.asym = 0.5,
                    facet.by = "prop.asym")
  as3 <- sub_plotS1(prop.asym = 0.7,
                    facet.by = "prop.asym")

  # Compose plot
  (index_as + as2 + as3) /
    (ic1 + index_ic + ic2) /
    (index_dl + patchwork::plot_spacer() + dl1) +
    patchwork::plot_layout(guides = "collect") &
    cowplot::theme_cowplot() &
    cowplot::panel_border() &
    ggplot2::theme(axis.title = ggplot2::element_blank()) &
    ggplot2::scale_fill_manual(guide = "none",
                               values = c("red", "black")) &
    ggplot2::scale_color_manual(values = c("red", "black"),
                                name = "Reproduction\nnumber")

}

#' Construct Figure S2 from the supplementary material
#' @author Joel Hellewell
#' @param res Data frame of results from scenarios from generate_analysis.R
#'
#' @return ggplot object
#' @export
#' @importFrom patchwork plot_spacer plot_layout
#' @importFrom cowplot theme_cowplot
#' @importFrom ggplot2 theme scale_fill_manual scale_color_manual element_blank
#'
#' @examples
#' \dontrun{
#'
#' sweep_results <- readRDS("data-raw/res.rds")
#' res <- sweep_results %>%
#'  dplyr::group_by(scenario) %>%
#'  dplyr::mutate(pext=extinct_prob(sims[[1]],cap_cases = 5000)) %>%
#'  dplyr::ungroup(scenario)
#'
#' make_figure_S1(results)
#' }
make_figure_S2 <- function(res = NULL) {

  sub_plotS2 <- purrr::partial(ringbp::sub_plot,
                               res.in = res,
                               num.initial.cases = unique(res$num.initial.cases),
                               col.by = "num.initial.cases")

  # delay
  index_dl <- sub_plotS2(facet.by = "delay")
  dl1 <- sub_plotS2(delay = "Wuhan",
                    facet.by = "delay")

  # R0
  ind_R0 <- sub_plotS2(facet.by = "index_R0")

  R01 <- sub_plotS2(index_R0=2.0,
                    facet.by = "index_R0")

  # asymptomatic
  index_as <- sub_plotS1(facet.by = "prop.asym")
  as1 <- sub_plotS1(prop.asym = 0.2,
                    facet.by = "prop.asym")
  as2 <- sub_plotS1(prop.asym = 0.5,
                    facet.by = "prop.asym")
  as3 <- sub_plotS1(prop.asym = 0.7,
                    facet.by = "prop.asym")

  # compose plot

  pl <- ((patchwork::plot_spacer() + index_dl + dl1) /
           (patchwork::plot_spacer() + ind_R0 + R01) /
           (index_as + as2 + as3)) +
    patchwork::plot_layout(guide = "collect")



  pl & cowplot::theme_cowplot() &
    cowplot::panel_border() &
    ggplot2::theme(axis.title = ggplot2::element_blank()) &
    ggplot2::scale_fill_manual(values = c("dodgerblue", "black", "dodgerblue4"),
                               name = "Number of\ninitial cases") &
    ggplot2::scale_colour_manual(guide = "none",
                                 values = c("dodgerblue", "black", "dodgerblue4"))

}

#' Construct Figure S3 from the supplementary material
#' @author Joel Hellewell
#' @param res Data frame of results from scenarios from generate_analysis.R
#'
#' @return ggplot object
#' @export
#' @importFrom patchwork plot_spacer plot_layout
#' @importFrom cowplot theme_cowplot
#' @importFrom ggplot2 theme scale_fill_manual scale_color_manual element_blank
#'
#' @examples
#' \dontrun{
#'
#' sweep_results <- readRDS("data-raw/res.rds")
#' res <- sweep_results %>%
#'  dplyr::group_by(scenario) %>%
#'  dplyr::mutate(pext=extinct_prob(sims[[1]],cap_cases = 5000)) %>%
#'  dplyr::ungroup(scenario)
#'
#' make_figure_S1(results)
#' }
make_figure_S3 <- function(res = NULL) {

  sub_plotS3 <- purrr::partial(ringbp::sub_plot,
                               res.in = res,
                               delay = c("SARS", "Wuhan"),
                               col.by = "delay")

  # theta
  ind_R0 <- sub_plotS3(facet.by = "index_R0")

  R01 <- sub_plotS3(index_R0 = 2,
                    facet.by = "index_R0")

  # asym
  index_as <- sub_plotS3(facet.by = "prop.asym")
  as1 <- sub_plotS3(prop.asym = 0.2,
                    facet.by = "prop.asym")
  as2 <- sub_plotS3(prop.asym = 0.5,
                    facet.by = "prop.asym")
  as3 <- sub_plotS3(prop.asym = 0.7,
                    facet.by = "prop.asym")

  pl <- (index_as + as2 + as3) / (patchwork::plot_spacer() + ind_R0 + R01) +
    patchwork::plot_layout(guides = "collect")


  pl &
    cowplot::theme_cowplot() &
    cowplot::panel_border() &
    ggplot2::theme(axis.title = ggplot2::element_blank()) &
    ggplot2::scale_fill_manual(guide = "none",
                               values = c("black", "forestgreen")) &
    ggplot2::scale_color_manual(values = c("black", "forestgreen"),
                                name = "Onset to\nisolation delay",
                                labels = c("Short", "Long"))

}

#' Construct Figure S4 from the supplementary material
#' @author Joel Hellewell
#' @param res Data frame of results from scenarios from generate_analysis.R
#'
#' @return ggplot object
#' @export
#' @importFrom patchwork plot_spacer plot_layout
#' @importFrom cowplot theme_cowplot
#' @importFrom ggplot2 theme scale_fill_manual scale_color_manual element_blank
#'
#' @examples
#' \dontrun{
#'
#' sweep_results <- readRDS("data-raw/res.rds")
#' res <- sweep_results %>%
#'  dplyr::group_by(scenario) %>%
#'  dplyr::mutate(pext=extinct_prob(sims[[1]],cap_cases = 5000)) %>%
#'  dplyr::ungroup(scenario)
#'
#' make_figure_S1(results)
#' }
#'
#' No longer relevant because theta isn't a parameter and we aren't varying the proportion of transmission that happens before symptoms
make_figure_S4 <- function(res = NULL) {
#
#   sub_plotS4 <- purrr::partial(ringbp::sub_plot,
#                                res.in = res,
#                                theta.in = c("<1%", "15%", "30%"),
#                                col.by = "theta")
#
#   index_as <- sub_plotS4(facet.by = "prop.asym")
#   as1 <- sub_plotS4(facet.by = "prop.asym",
#                     prop.asym.in = 0.1)
#
#   pl <- index_as + as1 +
#     patchwork::plot_layout(guides = "collect")
#
#   pl &
#     cowplot::theme_cowplot() &
#     cowplot::panel_border() &
#     ggplot2::theme(axis.title = ggplot2::element_blank()) &
#     ggplot2::scale_fill_manual(guide = "none",
#                                values = c("mediumpurple2", "black", "mediumpurple4")) &
#     ggplot2::scale_color_manual(values = c("mediumpurple2", "black", "mediumpurple4"),
#                                 name = "Percentage of\ntransmission\nbefore symptoms",
#                                 labels = c("<1%", "15%", "30%"))
#
}
