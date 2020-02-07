#' Generate a figure comparing the effective reproduction no with contacts traced.
#'
#' @param df A dataframe of results as produced by `parameter_sweep`
#'
#' @return A ggplot2 plot of the effective reproduction no vs contacts traced.
#' @export
#'
#' @examples
#'
#'
#'
make_figure3b <- function(df){
 df_extracted <-  df %>%
    dplyr::mutate(effective_r0 = purrr::map(
      sims,
      ~ dplyr::group_by(., sim) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::summarise(median_eff_r0 = median(effective_r0, na.rm = TRUE),
                  lower = quantile(effective_r0, 0.025, na.rm = TRUE),
                  iqr_lower = quantile(effective_r0, 0.25, na.rm = TRUE),
                  iqr_upper = quantile(effective_r0, 0.75, na.rm = TRUE),
                  upper = quantile(effective_r0, 0.975, na.rm = TRUE))
    )) %>%
    tidyr::unnest("effective_r0")

 df_extracted %>%
    dplyr::filter(prop.asym==0,
                  theta=="15%",
                  num.initial.cases==20,
                  delay=="SARS") %>%
    ggplot2::ggplot(ggplot2::aes(x=control_effectiveness,
                                 y=median_eff_r0,
                                 col=as.factor(index_R0),
                                 fill = as.factor(index_R0))) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, col = NULL), alpha = 0.4) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = iqr_lower, ymax = iqr_upper, col = NULL), alpha = 0.4) +
    ggplot2::geom_line() +
    ggplot2::xlab("Contacts traced (%)") +
    ggplot2::ylab("Effective reproduction number") +
    ggplot2::scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    ggplot2::scale_y_continuous(breaks=seq(0,3.5,0.5)) +
    ggplot2::geom_hline(yintercept=1,lty=2,size=0.75) +
    ggplot2::geom_point(shape=21,col="black",
                        ggplot2::aes(fill=as.factor(index_R0)), size=3) +
    ggplot2::scale_fill_manual(values = c("red","black","firebrick4")) +
    ggplot2::scale_color_manual(values = c("red","black","firebrick4"),name="Reproduction\nnumber") +
    cowplot::theme_cowplot() +
    ggplot2::theme(legend.position = "none")
}

