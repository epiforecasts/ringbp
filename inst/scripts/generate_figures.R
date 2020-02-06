
# Packages ----------------------------------------------------------------

library(ringbp)

# Figure 1 ----------------------------------------------------------------

## Generated in inkscape

# Figure 2 ----------------------------------------------------------------

ringbp::mvt_plot()

ggplot2::ggsave("inst/plots/fig_2.png", height = 8, width = 16)

# Load in results  -------------------------------------------------------

sweep_results <- readRDS("data-raw/res.rds")

res <- sweep_results %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(pext=extinct_prob(sims[[1]],cap_cases = 5000)) %>%
  dplyr::ungroup(scenario)


# Figure 3 ----------------------------------------------------------------

make_figure_3 <- function(theta_value = "15%"){
  res %>%
    dplyr::filter(theta==theta_value) %>%
    dplyr::mutate(delay = factor(delay,levels=c("SARS", "Wuhan"),
                                 labels = c("Short delay", "Long delay"))) %>%
    dplyr::mutate(num.initial.clusters = factor(num.initial.clusters,levels=c(5,20,40),
                                                labels = c("5 cases","20 cases","40 cases"))) %>%
    dplyr::mutate(index_R0 = factor(index_R0,levels = c(1.5,2.5,3.5),
                                    labels = c("R0 = 1.5","R0 = 2.5","R0 = 3.5"))) %>%
    # dplyr::mutate(delay = factor(delay,levels=c("SARS","Wuhan"),labels = c("Short delay","Long delay"))) %>%
    ggplot2::ggplot(ggplot2::aes(x=control_effectiveness,y=pext,col=as.factor(delay))) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_grid(num.initial.clusters~ index_R0) +
    ggplot2::scale_color_brewer(palette = "Set1",name="Delay from onset\n to hospitalisation") +
    ggplot2::theme_bw() +
    ggplot2::ylab("Percentage of simulated outbreaks controlled") +
    ggplot2::xlab("Percentage of contacts traced") +
    ggplot2::scale_x_continuous(breaks=seq(0,1,0.2),labels=paste0(seq(0,100,20))) +
    ggplot2::scale_y_continuous(breaks=seq(0,1,0.2),labels=paste0(seq(0,100,20)))


}

make_figure_3()


ggplot2::ggsave("inst/plots/fig_3.png", height = 5, width = 8)



# Figure 4 ----------------------------------------------------------------

make_figure_4 <- function(initial_cases = 20) {
  res %>%
    dplyr::filter(num.initial.clusters == initial_cases) %>%
    dplyr::mutate(delay = factor(delay,levels=c("SARS", "Wuhan"),
                                 labels = c("Short delay", "Long delay"))) %>%
    dplyr::mutate(theta = factor(theta,levels = c("<1%","15%","30%"),
                                 labels = c("<1% transmission \n  before symptoms",
                                            "15% transmission \n  before symptoms",
                                            "30% transmission \n  before symptoms"))) %>%
    dplyr::mutate(delay = factor(delay,levels=c("SARS","Wuhan"),labels = c("Short delay","Long delay"))) %>%
    ggplot2::ggplot(ggplot2::aes(x=control_effectiveness,y=pext,col=as.factor(index_R0))) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_grid(delay ~ theta) +
    ggplot2::scale_color_brewer(palette = "Dark2",name="Reproduction number") +
    ggplot2::theme_bw() +
    ggplot2::ylab("Percentage of simulated outbreaks controlled") +
    ggplot2::xlab("Percentage of contacts traced") +
    ggplot2::scale_x_continuous(breaks=seq(0,1,0.2),labels=paste0(seq(0,100,20))) +
    ggplot2::scale_y_continuous(breaks=seq(0,1,0.2),labels=paste0(seq(0,100,20)))
}


make_figure_4()

ggplot2::ggsave("inst/plots/fig_4.png", height = 8, width = 12)



# Figure 5 ----------------------------------------------------------------

ringbp::box_plot_max_weekly_cases(results = sweep_results, cap_cases = 5000, extinct_thresold = 0.05,
                                  filt_control_effectiveness = 0.4, num_initial_clusters = 20, flip_coords = T,
                                  facet_scales = "fixed", record_params = F)

ggplot2::ggsave("inst/plots/fig_5.png", height = 5, width = 10)


# Supplementary figures ---------------------------------------------------

## S1 A and B

make_figure_3(theta_value = "<1%")

ggplot2::ggsave("inst/plots/S_fig_1_A.png", height = 8, width = 12)

make_figure_3(theta_value = "30%")

ggplot2::ggsave("inst/plots/S_fig_1_B.png", height = 8, width = 12)


## S2 A and B

make_figure_4(initial_cases = 5)

ggplot2::ggsave("inst/plots/S_fig_2_A.png", height = 8, width = 12)

make_figure_4(initial_cases = 40)

ggplot2::ggsave("inst/plots/S_fig_2_B.png", height = 8, width = 12)

## S3 A and B

ringbp::box_plot_max_weekly_cases(results = sweep_results, cap_cases = 5000, extinct_thresold = 0.05,
                                  filt_control_effectiveness = 0.4, num_initial_clusters = 5, flip_coords = T,
                                  facet_scales = "fixed", record_params = F)

ggplot2::ggsave("inst/plots/S_fig_3_A.png", height = 5, width = 10)


ringbp::box_plot_max_weekly_cases(results = sweep_results, cap_cases = 5000, extinct_thresold = 0.05,
                                  filt_control_effectiveness = 0.4, num_initial_clusters = 40, flip_coords = T,
                                  facet_scales = "fixed", record_params = F)

ggplot2::ggsave("inst/plots/S_fig_3_B.png", height = 5, width = 10)

## S4

ringbp::serial_interval_plot()

ggplot2::ggsave("inst/plots/S_fig_4.png", height = 8, width = 12)
