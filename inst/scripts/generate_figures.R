
# Packages ----------------------------------------------------------------

library(ringbp)
library(ggplot2)
library(dplyr)
library(patchwork)
library(cowplot)

# Figure 1 ----------------------------------------------------------------

## Generated in inkscape

# Figure 2 ----------------------------------------------------------------

ringbp::mvt_plot()

ggplot2::ggsave("inst/plots/fig_2.pdf", height = 5.5, width = 10, useDingbats=FALSE)

# Load in results  -------------------------------------------------------

sweep_results <- readRDS("data-raw/res.rds")

res <- sweep_results %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(pext=extinct_prob(sims[[1]],cap_cases = 5000)) %>%
  dplyr::ungroup(scenario)


# Figure 3 ----------------------------------------------------------------

make_figure_3 <- function(df){
  res %>%
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
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    theme(legend.position = "bottom") +
    labs(tag = "A")
}


fig_3_a <- make_figure_3(df = res)
fig_3_b <- make_figure3b(df = res) + labs(tag = "B")

fig_3_a + fig_3_b + plot_layout(guides = "keep")

ggplot2::ggsave("inst/plots/fig_3.pdf", height = 5, width = 8, useDingbats=FALSE)


# Figure 4 ----------------------------------------------------------------




make_figure_4(df = res)

ggplot2::ggsave("inst/plots/fig_4.pdf", height = 5, width = 12)



# Figure 5 ----------------------------------------------------------------

ringbp::box_plot_max_weekly_cases(results = sweep_results, cap_cases = 5000, extinct_thresold = 0.1,
                                  filt_control_effectiveness = 0.4, num_initial_cases = 20, flip_coords = FALSE,
                                  facet_scales = "fixed", record_params = F, prop_asym = 0)

ggplot2::ggsave("inst/plots/fig_5.pdf", height = 7, width = 12, useDingbats=FALSE)


# Supplementary figures ---------------------------------------------------

## S1

make_figure_S1(res)

ggplot2::ggsave("inst/plots/S_fig_1.pdf", height = 7.5, width = 9, useDingbats=FALSE)

## S2

make_figure_S2(res)

ggplot2::ggsave("inst/plots/S_fig_2.pdf", height = 7, width = 9, useDingbats=FALSE)

## S3

make_figure_S3(res)

ggplot2::ggsave("inst/plots/S_fig_3.pdf", height = 7, width = 9, useDingbats=FALSE)

## S4

make_figure_S4(res)

ggplot2::ggsave("inst/plots/S_fig_4.pdf", height = 3, width = 6.5, useDingbats=FALSE)


## S5

ringbp::box_plot_max_weekly_cases(results = sweep_results, cap_cases = 5000, extinct_thresold = 0.05, prop_asym = 0,
                                  filt_control_effectiveness = 0.4, num_initial_cases = 5,
                                  facet_scales = "fixed", record_params = F)

ggplot2::ggsave("inst/plots/S_fig_3_A.pdf", height = 5, width = 10, useDingbats=FALSE)


ringbp::box_plot_max_weekly_cases(results = sweep_results, cap_cases = 5000, extinct_thresold = 0.05, prop_asym = 0,
                                  filt_control_effectiveness = 0.4, num_initial_cases = 40,
                                  facet_scales = "fixed", record_params = F)

ggplot2::ggsave("inst/plots/S_fig_3_B.pdf", height = 5, width = 10, useDingbats=FALSE)

ringbp::box_plot_max_weekly_cases(results = sweep_results, cap_cases = 5000, extinct_thresold = 0.05, prop_asym = 0.1,
                                  filt_control_effectiveness = 0.4, num_initial_cases = 20,
                                  facet_scales = "fixed", record_params = F)

ggplot2::ggsave("inst/plots/S_fig_3_C.pdf", height = 5, width = 10, useDingbats=FALSE)

## S4

ringbp::serial_interval_plot()

ggplot2::ggsave("inst/plots/S_fig_4.pdf", height = 8, width = 12, useDingbats=FALSE)

## Get data for supplement looking at flu like dispersion

results_dispersion_flu <- readRDS("data-raw/sweep_results_disp2.RDS.rds")

res_flu <- results_dispersion_flu  %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(pext=extinct_prob(sims[[1]],cap_cases = 5000)) %>%
  dplyr::ungroup(scenario)

## S5

dispersion_plot()

ggplot2::ggsave("inst/plots/S_fig_5a.pdf", height = 4, width = 12, useDingbats=FALSE)

make_figure_3(df = res_flu)
make_figure_4(df = res_flu)

ggplot2::ggsave("inst/plots/S_fig_5.pdf", height = 8, width = 12, useDingbats=FALSE)


## S6

ringbp::box_plot_max_weekly_cases(results = results_dispersion_flu, cap_cases = 5000, extinct_thresold = 0.05,
                                  filt_control_effectiveness = 0.4, num_initial_cases = 40,
                                  flip_coords = T, prop_asym = 0, facet_scales = "fixed", record_params = F)

ggplot2::ggsave("inst/plots/S_fig_6.pdf", height = 8, width = 12, useDingbats=FALSE)

