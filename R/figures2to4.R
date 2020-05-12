#' Makes Figure 2a from manuscript
#' @author Joel Hellewell
#'
#' @return A ggplot2 plot object
#' @export
#' @importFrom data.table data.table
#' @importFrom tidyr gather
#' @importFrom magrittr %<>%
#' @importFrom dplyr mutate
#' @importFrom data.table data.table
#' @importFrom ggplot2 ggplot geom_ribbon theme_bw theme xlab ylab geom_line geom_vline scale_colour_brewer scale_fill_brewer element_text aes
#'
make_figure_2a <- function() {

  # A colour-blind-friendly palette
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  hist(pmax(1,rlnorm((0:20000000)/100000,meanlog=1.434,sdlog=0.6612)+rgamma((0:20000000)/1000000,shape=2.116,rate=0.6899)-3),plot=F,breaks=c(0,0.5,(10:20000)/10)) -> h
  GI <- data.frame(gamma_sing = dgamma((0:2000)/100,shape=1.819,scale=2.821),
                   gamma_china = dgamma((0:2000)/100,shape=2.1217,scale=1.028))

  GI2 <- data.frame(num=c(h$density,GI$gamma_sing,GI$gamma_china),x = c(h$breaks[1:length(h$density)],(0:2000)/100,(0:2000)/100),Source=c(rep('He et al.',length(h$density)),rep('Ganyani et al. (1)',2001),rep('Ganyani et al. (2)',2001)))

  GI2 %>%
    ggplot2::ggplot(ggplot2::aes(x = x,
                                 y = num,
                                 ymin = 0,
                                 ymax = num,
                                 fill = Source)) +
    ggplot2::geom_ribbon(alpha = 0.4) +
    cowplot::theme_cowplot() +
    ggplot2::geom_line(aes(x, num,colour=Source),size=0.5) +
    ggplot2::theme(legend.position = "bottom",
                   axis.text = ggplot2::element_text(size = 10),
                   axis.title = ggplot2::element_text(size = 12),
                   legend.text = ggplot2::element_text(size = 10)) +
    ggplot2::scale_fill_manual(values = cbPalette[c(2,3,4)]) +
    ggplot2::scale_colour_manual(guide="none",values = cbPalette[c(2,3,4)]) +
    # ggplot2::geom_vline(data = medians,
    #                     ggplot2::aes(xintercept = x,
    #                                  col = as.factor(dist)),
    #                     lty = 2,
    #                     size = 0.8) +
    ggplot2::labs(tag = "A",
                  x = "generation interval (days)",
                  y = "probability density") +
    xlim(c(0,20))

}


#' Constructs figure 2 from the manuscript
#' @author Joel Hellewell
#' @return A patchwork object containing a number of ggplot2 plots
#' @export
#' @importFrom sn dsn
#' @importFrom ggplot2 ggplot aes geom_line geom_vline coord_cartesian labs geom_ribbon scale_fill_manual theme element_text
#' @importFrom cowplot theme_cowplot
#' @examples
#'\dontrun{
#'make_figure_2()
#'}
make_figure_2 <- function() {

  # A colour-blind-friendly palette
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  p2 <- data.frame(x = seq(0, 15, 0.1),
                   y = dlnorm(x = seq(0, 15, 0.1),
                                meanlog = 1.434065,
                                sdlog = 0.6612),
                   theta = rep(c("mean =\n4.2 days"),rep(151,1))) %>%
    ggplot2::ggplot(aes(x = x, y = y, fill=theta,colour=theta)) +
    ggplot2::geom_line() +
    cowplot::theme_cowplot() +
    ggplot2::geom_vline(xintercept = exp(1.43), lty = 2) +
    ggplot2::coord_cartesian(xlim = c(0, 13)) +
    ggplot2::labs(tag = "B", x = "time since infection (days)", y = "probability density") +
    ggplot2::geom_ribbon(aes(ymax = y, ymin = 0),
                         alpha = 0.4) +
    ggplot2::theme(legend.position = "right") +
    ggplot2::scale_fill_manual(values = cbPalette[1],
                               name = c("Incubation\nperiod"))  +
    ggplot2::scale_colour_manual(guide="none",values = cbPalette[1])


  p3 <- data.frame(y = c(dgamma(seq(0, 13, 0.1), 2.115779, 0.6898583)),
                   x = seq(0, 13, 0.1)-3,
                   theta = rep(c("42% before\n symptoms"), rep(131, 1))) %>%
    ggplot2::ggplot(aes(x, y, fill = theta, colour=theta)) +
    ggplot2::geom_ribbon(aes(ymin = 0, ymax = y), alpha = 0.4) +
    ggplot2::geom_line(aes(x, y)) +
    cowplot::theme_cowplot() +
    ggplot2::coord_cartesian(xlim = c(-3, 10)) +
    ggplot2::geom_vline(xintercept = -1.4,lty=2) +
    ggplot2::theme(legend.position = "right") +
    ggplot2::scale_fill_manual(values = cbPalette[5],
                               name = c("Transmission\nprofile")) +
    ggplot2::scale_colour_manual(guide="none",values = cbPalette[5]) +
    ggplot2::labs(tag = "C",
                  x = "time since symptom onset (days)",
                  y = "probability density")


  (make_figure_2a() | (p2 / p3)) & theme(axis.text = element_text(size = 10),
                                         legend.title = element_text(size = 12),
                                         legend.text = element_text(size = 10),
                                         axis.title = element_text(size = 10))

}


#' Construct Figure 3a from manuscript
#'
#' @param df A dataframe of results as produced by `parameter_sweep`
#'
#' @return A ggplot2 plot object
#' @export
#' @importFrom ggplot2 ggplot geom_line geom_point scale_fill_manual scale_color_manual scale_x_continuous scale_y_continuous theme labs
#' @importFrom cowplot theme_cowplot
#' @importFrom dplyr filter select
#' @examples
#'\dontrun{
#'make_figure_3a()
#'}
#'
make_figure_3a <- function(df = NULL, num.initial.cases_val = 5,
                           delay_val = "SARS",
                           prop.asym_val = 0.4) {
  pl <- df %>%
    dplyr::filter(num.initial.cases == num.initial.cases_val,
                  delay == delay_val,
                  prop.asym == prop.asym_val) %>%
    dplyr::select(control_effectiveness, index_R0, pext) %>%
    ggplot2::ggplot(ggplot2::aes(x = control_effectiveness,
                                 y = pext,
                                 color = as.factor(index_R0))) +
    ggplot2::geom_line(size = 0.75) +
    ggplot2::geom_point(shape = 21,
                        col = "black",
                        ggplot2::aes(fill = as.factor(index_R0)),
                        size = 3) +
    ggplot2::scale_fill_manual(guide = "none",
                               values = c("red", "black", "firebrick4")) +
    ggplot2::scale_color_manual(values = c("red", "black", "firebrick4"),
                                name = "Reproduction\nnumber")  +
    cowplot::theme_cowplot() +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2),
                               labels = seq(0, 100, 20)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2),
                               labels = seq(0, 100, 20)) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(tag = "A",
                  x = "Contacts traced (%)",
                  y = "Simulated outbreaks controlled (%)")
  return(pl)
}


#' Generate a figure comparing the effective reproduction no with contacts traced.
#' @author Sam Abbott
#' @param df A dataframe of results as produced by `parameter_sweep`
#'
#' @return A ggplot2 plot of the effective reproduction no vs contacts traced.
#' @export
#'
#' @examples
#'\dontrun{
#'make_figure_3b()
#'}
make_figure3b <- function(df = NULL, num.initial.cases_val = 5,
                          delay_val = "SARS",
                          prop.asym_val = 0.4) {
  df_extracted <-  df %>%
    dplyr::mutate(effective_r0 = purrr::map(
      sims,
      ~ dplyr::group_by(., sim) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::summarise(median_eff_r0 = median(effective_r0,
                                                na.rm = TRUE),
                         lower = quantile(effective_r0, 0.025,
                                          na.rm = TRUE),
                         iqr_lower = quantile(effective_r0,
                                              0.25,
                                              na.rm = TRUE),
                         iqr_upper = quantile(effective_r0,
                                              0.75,
                                              na.rm = TRUE),
                         upper = quantile(effective_r0,
                                          0.975,
                                          na.rm = TRUE))
    )) %>%
    tidyr::unnest("effective_r0")

  df_extracted %>%
    dplyr::filter(prop.asym == prop.asym_val,
                  num.initial.cases == num.initial.cases_val,
                  delay == delay_val) %>%
    ggplot2::ggplot(ggplot2::aes(x = control_effectiveness,
                                 y = median_eff_r0,
                                 col = as.factor(index_R0),
                                 fill = as.factor(index_R0))) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower,
                                      ymax = upper,
                                      col = NULL),
                         alpha = 0.1) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = iqr_lower,
                                      ymax = iqr_upper,
                                      col = NULL),
                         alpha = 0.3) +
    ggplot2::geom_line() +
    ggplot2::xlab("Contacts traced (%)") +
    ggplot2::ylab("Effective reproduction number") +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2),
                               labels = seq(0, 100, 20)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 3.5, 0.5)) +
    ggplot2::geom_hline(yintercept = 1, lty = 2, size = 0.75) +
    ggplot2::geom_point(shape = 21, col = "black",
                        ggplot2::aes(fill = as.factor(index_R0)),
                        size = 3) +
    ggplot2::scale_fill_manual(values = c("red", "black", "firebrick4")) +
    ggplot2::scale_color_manual(values = c("red", "black", "firebrick4"),
                                name = "Reproduction\nnumber") +
    cowplot::theme_cowplot() +
    ggplot2::theme(legend.position = "none")
}



#' Construct Figure 4 from the manuscript
#'
#' @param res The results dataframe todo
#'
#' @return A ggplot2 plot object
#' @export
#' @importFrom ggplot2 ggplot geom_line geom_point scale_fill_manual scale_color_manual aes theme xlab ylab element_text
#' @importFrom dplyr filter mutate
#' @importFrom cowplot theme_cowplot
#' @importFrom patchwork plot_annotation
#'
#' @examples
#'\dontrun{
#'make_figure_4()
#'}
make_figure_4 <- function(res = NULL, num.initial.cases_val = 5,
                          delay_val = "SARS", prop.asym_val = 0.4,
                          index_R0_val = 1.1) {

  f4p1 <- res %>%
    dplyr::filter(delay == delay_val,
                  index_R0 == index_R0_val,
                  prop.asym == prop.asym_val) %>%
    ggplot2::ggplot(ggplot2::aes(x = control_effectiveness,
                                 y = pext,
                                 color = as.factor(num.initial.cases))) +
    ggplot2::geom_line(size = 0.75) +
    ggplot2::geom_point(shape = 21,
                        col = "black",
                        aes(fill = as.factor(num.initial.cases)),
                        size = 3) +
    ggplot2::scale_fill_manual(guide = "none",
                               values = c("dodgerblue", "black", "dodgerblue3")) +
    ggplot2::scale_color_manual(values = c("dodgerblue", "black", "dodgerblue3"),
                                name = "Number of\ninitial cases")  +
    cowplot::theme_cowplot()

  f4p2 <- res %>%
    dplyr::filter(num.initial.cases == num.initial.cases_val,
                  index_R0 == index_R0_val,
                  prop.asym == prop.asym_val) %>%
    dplyr::mutate(delay = factor(delay,
                                 levels = c("SARS", "Wuhan", "Post lockdown"),
                                 labels = c("Short", "Long", "V. short"))) %>%
    ggplot2::ggplot(ggplot2::aes(x = control_effectiveness,
                                 y = pext,
                                 color = as.factor(delay))) +
    ggplot2::geom_line(size = 0.75) +
    ggplot2::geom_point(shape = 21,
                        col = "black",
                        aes(fill = as.factor(delay)),
                        size = 3) +
    ggplot2::scale_fill_manual(guide = "none",
                               values = c("black", "forestgreen")) +
    ggplot2::scale_color_manual(values = c("black", "forestgreen"),
                                name = "Onset to\nisolation\ndelay") +
    cowplot::theme_cowplot()

  f4p3 <- res %>%
    dplyr::filter(num.initial.cases == num.initial.cases_val,
                  delay == delay_val,
                  prop.asym == prop.asym_val) %>%
    ggplot2::ggplot(ggplot2::aes(x = control_effectiveness,
                                 y = pext,
                                 color = as.factor(index_R0))) +
    ggplot2::geom_line(size = 0.75) +
    ggplot2::geom_point(shape = 21,
                        col = "black",
                        aes(fill = as.factor(index_R0)),
                        size = 3) +
    ggplot2::scale_fill_manual(guide = "none",
                               values = c("mediumpurple2", "black", "mediumpurple4")) +
    ggplot2::scale_color_manual(values = c("mediumpurple2", "black", "mediumpurple4"),
                                name = "Reproduction\nnumber") +
    cowplot::theme_cowplot()

  f4p4 <- res %>%
    dplyr::filter(num.initial.cases == num.initial.cases_val,
                  index_R0 == index_R0_val,
                  delay == delay_val) %>%
    dplyr::mutate(prop.asym = factor(prop.asym,
                                     levels = sort(unique(prop.asym)),
                                     labels = paste0(sort(unique(prop.asym)) * 100,'%'))) %>%
    ggplot2::ggplot(aes(x = control_effectiveness,
                        y = pext,
                        color = as.factor(prop.asym))) +
    ggplot2::geom_line(size = 0.75) +
    ggplot2::geom_point(shape = 21,
                        col = "black",
                        aes(fill = as.factor(prop.asym)),
                        size = 3) +
    ggplot2::scale_fill_manual(guide = "none",
                               values = c("black", "chocolate4","mediumpurple2", "mediumpurple4")) +
    ggplot2::scale_color_manual(values = c("black", "chocolate4","mediumpurple2", "mediumpurple4"),
                                name = "Proportion\nof cases\nwithout\nsymptoms")  +
    guides(colour=guide_legend(title="Proportion\nof cases\nwithout\nsymptoms"), fill = FALSE) +
    cowplot::theme_cowplot()

  fig4 <- (f4p1 + f4p2) / (f4p3 + f4p4) + patchwork::plot_annotation(tag_levels = "A") &
    ggplot2::theme(legend.position = "left",
          plot.tag.position = "topleft",
          plot.tag = element_text(size = 20, face = "bold"),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 11),
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 11)) &
    ggplot2::ylab("Simulated outbreaks controlled (%)") &
    ggplot2::xlab("Contacts traced (%)")

  return(fig4)
}

