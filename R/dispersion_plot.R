#' Plots the impact of dispersion on the distribution of new cases
#' @author Sam Abbott
#'
#' @return
#' @export
#' @importFrom tidyr gather unnest
#' @importFrom dplyr mutate filter
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot geom_ribbon theme_bw theme xlab ylab geom_line geom_vline scale_colour_brewer scale_fill_brewer
#' @examples
#'
dispersion_plot <- function(){

  disp_sars <- function(x, r0){dnbinom(x, size = 0.16, mu =r0)}
  disp_diff <- function(x, r0){dnbinom(x,size=2, mu = r0)}

  out <- tibble::tibble(r0 = c(1.5, 2.5, 3.5), x = list(seq(0, 60, 1))) %>%
    dplyr::mutate(sars_samples = purrr::map2(r0, x, ~ disp_sars(x[[1]], .)),
                  diff_samples = purrr::map2(r0, x, ~ disp_diff(x[[1]], .))) %>%
    tidyr::gather(key = "disp", value = "samples", -r0, -x) %>%
    tidyr::unnest(c("samples", "x")) %>%
    dplyr::mutate(disp = disp %>%
                    factor(levels = c("sars_samples", "diff_samples"),
                           labels = c("SARS-like (Dispersion = 0.16)", "Flu-like (Dispersion = 2)")))



  out %>%
    dplyr::filter(x < 30) %>%
    ggplot2::ggplot(ggplot2::aes(x= x, y = samples, ymin = 0,
                                 ymax = samples, fill = disp)) +
    ggplot2::facet_wrap(~r0) +
    ggplot2::geom_ribbon(alpha = 0.4) +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="bottom",
                   axis.text = ggplot2::element_text(size=10),
                   axis.title = ggplot2::element_text(size=12),
                   legend.text = ggplot2::element_text(size=10)) +
    ggplot2::xlab("Secondary cases per infectious case") +
    ggplot2::scale_fill_brewer(name="",palette="Set1") +
    ggplot2::ylab("Probability density") +
    ggplot2::scale_colour_brewer(guide="none",palette="Set1")

}


