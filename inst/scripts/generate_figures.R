
# Packages ----------------------------------------------------------------

library(ringbp)

# Figure 1 ----------------------------------------------------------------

## Generated in inkscape

# Figure 2 ----------------------------------------------------------------

ringbp::mvt_plot()

ggplot2::ggsave("inst/plots/fig_2.pdf", height = 5.5, width = 10)

# Load in results  -------------------------------------------------------

sweep_results <- readRDS("data-raw/res.rds")

res <- sweep_results %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(pext=extinct_prob(sims[[1]],cap_cases = 5000)) %>%
  dplyr::ungroup(scenario)


# Figure 3 ----------------------------------------------------------------

make_figure_3 <- function(df){
  df %>%
    dplyr::filter(num.initial.cases==20,
                  theta == "15%",
                  delay == "SARS",
                  prop.asym==0) %>%
    dplyr::select(control_effectiveness,index_R0,pext) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(index_R0))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(index_R0)),size=3) +
    scale_fill_manual(guide="none",values = c("red","black","firebrick4")) +
    scale_color_manual(values = c("red","black","firebrick4"),name="Reproduction\nnumber")  + theme_cowplot() +
    ylab("Simulated outbreaks controlled (%)") +
    xlab("Contacts traced (%)") + scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20))
}


make_figure_3(df = res)

ggplot2::ggsave("inst/plots/fig_3.pdf", height = 5, width = 8)

make_figure3b(df = res)

ggplot2::ggsave("inst/plots/fig_3b.pdf", height = 5, width = 8)

# Figure 4 ----------------------------------------------------------------




make_figure_4(df = res)

ggplot2::ggsave("inst/plots/fig_4.pdf", height = 5, width = 12)



# Figure 5 ----------------------------------------------------------------

ringbp::box_plot_max_weekly_cases(results = sweep_results, cap_cases = 5000, extinct_thresold = 0.1,
                                  filt_control_effectiveness = 0.4, num_initial_cases = 20, flip_coords = FALSE,
                                  facet_scales = "fixed", record_params = F, prop.asym = 0)

ggplot2::ggsave("inst/plots/fig_5.pdf", height = 7, width = 12)


# Supplementary figures ---------------------------------------------------

## S1

make_figure_S1(res)

ggplot2::ggsave("inst/plots/S_fig_1.pdf", height = 7.5, width = 9)

## S2

make_figure_S2(res)

ggplot2::ggsave("inst/plots/S_fig_2.pdf", height = 7, width = 9)

## S3

make_figure_S3(res)

ggplot2::ggsave("inst/plots/S_fig_3.pdf", height = 5.5, width = 9)

## S4

make_figure_S4(res)

ggplot2::ggsave("inst/plots/S_fig_4.pdf", height = 3, width = 6.5)


## S5

ringbp::box_plot_max_weekly_cases(results = sweep_results, cap_cases = 5000, extinct_thresold = 0.05, prop_asym = 0,
                                  filt_control_effectiveness = 0.4, num_initial_cases = 5,
                                  facet_scales = "fixed", record_params = F)

ggplot2::ggsave("inst/plots/S_fig_3_A.pdf", height = 5, width = 10)


ringbp::box_plot_max_weekly_cases(results = sweep_results, cap_cases = 5000, extinct_thresold = 0.05, prop_asym = 0,
                                  filt_control_effectiveness = 0.4, num_initial_cases = 40,
                                  facet_scales = "fixed", record_params = F)

ggplot2::ggsave("inst/plots/S_fig_3_B.pdf", height = 5, width = 10)

ringbp::box_plot_max_weekly_cases(results = sweep_results, cap_cases = 5000, extinct_thresold = 0.05, prop_asym = 0.1,
                                  filt_control_effectiveness = 0.4, num_initial_cases = 20,
                                  facet_scales = "fixed", record_params = F)

ggplot2::ggsave("inst/plots/S_fig_3_C.pdf", height = 5, width = 10)

## S6

ringbp::serial_interval_plot()

ggplot2::ggsave("inst/plots/S_fig_4.pdf", height = 8, width = 12)

## Get data for supplement looking at flu like dispersion

results_dispersion_flu <- readRDS("data-raw/res_dispersion_flu.rds")

res_flu <- results_dispersion_flu  %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(pext=extinct_prob(sims[[1]],cap_cases = 5000)) %>%
  dplyr::ungroup(scenario)

## S7

dispersion_plot()

# remaking fig 3 with flu dispersion
make_figure_3(df = res_flu)
ggplot2::ggsave("inst/plots/S_fig_7b.png", height = 4, width = 6)



# remaking fig 4 with flu dispersion
make_figure_4(res = res_flu)
ggplot2::ggsave("inst/plots/S_fig_7c.png", height = 5, width = 7)


## S6

ringbp::box_plot_max_weekly_cases(results = results_dispersion_flu, cap_cases = 5000, extinct_thresold = 0.05,
                                  filt_control_effectiveness = 0.4, num_initial_cases = 40,
                                  flip_coords = T, prop_asym = 0, facet_scales = "fixed", record_params = F)

ggplot2::ggsave("inst/plots/S_fig_6.pdf", height = 8, width = 12)

