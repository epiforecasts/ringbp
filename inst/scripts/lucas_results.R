#'---
#'output:
#'  pdf_document:
#'    number_sections: true
#'    toc: true
#'    toc_depth: 2
#'title: "Further analysis of COVID branching process"
#'author: Tim Lucas and Emma Davis
#'fontsize: 8pt
#'geometry: margin=0.5in
#'---

#+setup, echo = TRUE, cache = FALSE

knitr::opts_chunk$set(cache = TRUE, fig.width = 8, fig.height = 5, cache.lazy = FALSE)


library(tidyverse)
library(git2r)
library(tictoc)
library(ggplot2)
library(patchwork)
library(cowplot)

devtools::load_all()

git2r::revparse_single('.',"HEAD")$sha

# Make the log file
#logs <- file.path("log_lucas.txt")
#con <- file(logs, open = "wt")
# # Send Output to log
#sink(con)
#sink(con, type = "message")


#+ create_parameters

scenarios <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("SARS", "Wuhan"),
    delay_shape = c(1.651524, 2.305172),
    delay_scale = c(4.287786, 9.483875)
  )),
  k_group = list(tibble::tibble(
    theta = c("<1%", "15%", "30%"),
    k = c(30, 1.95, 0.7)
  )),
  index_R0 = c(1.5, 2.5, 3.5),
  prop.asym = c(0, 0.1),
  control_effectiveness = seq(0, 1, 0.2),
  num.initial.cases = c(5, 20, 40)) %>%
  tidyr::unnest("k_group") %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

cap_cases <- 200
max_days <- 200
## Parameterise fixed paramters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.16,
                                  quarantine = FALSE)


#+ setup_multicore, cache = FALSE
## Set up multicore if using see ?future::plan for details
## Use the workers argument to control the number of cores used.
#future::plan("multiprocess")


#+ full_run

tic()
## Run paramter sweep
sweep_results <- ringbp::parameter_sweep(scenarios,
                                         sim_fn = sim_with_params,
                                         samples = 20,
                                         show_progress = TRUE)

toc()


#+ writeout

saveRDS(sweep_results, file = "../../data-raw/lucas_res.rds")

#sink(type = "message")
#sink()




#+ plots1


# Figure 2 ----------------------------------------------------------------

ringbp::make_figure_2()

ggplot2::ggsave("../plots/fig_2.pdf", height = 5.5, width = 10,
                useDingbats = FALSE)
ggplot2::ggsave("../plots/fig_2.png", height = 5.5, width = 10)


#+ plots2

# Load in results  -------------------------------------------------------


res <- sweep_results %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(pext = extinct_prob(sims[[1]], cap_cases = cap_cases)) %>%
  dplyr::ungroup(scenario)

#+ plots3

# Figure 3 ----------------------------------------------------------------

fig_3_a <- make_figure_3a(df = res)
fig_3_b <- make_figure3b(df = res) + ggplot2::labs(tag = "B")

fig_3_a + fig_3_b + patchwork::plot_layout(guides = "keep")

ggplot2::ggsave("../plots/fig_3.pdf", height = 5, width = 8,
                useDingbats = FALSE)
ggplot2::ggsave("../plots/fig_3.png", height = 5, width = 8)


#+ plots4

# Figure 4 ----------------------------------------------------------------


make_figure_4(res = res)

ggplot2::ggsave("../plots/fig_4.pdf", height = 5, width = 9,
                useDingbats = FALSE)
ggplot2::ggsave("../plots/fig_4.png", height = 5, width = 9)

#+ plots5

# Figure 5 ----------------------------------------------------------------

ringbp::box_plot_max_weekly_cases(results = sweep_results, cap_cases = cap_cases,
                                  extinct_thresold = 0.1,
                                  filt_control_effectiveness = 0.4,
                                  num_initial_cases = 20, flip_coords = FALSE,
                                  facet_scales = "fixed", record_params = F,
                                  prop_asym = 0, y_lim = 125)

ggplot2::ggsave("../plots/fig_5.pdf", height = 7, width = 12,
                useDingbats = FALSE)
ggplot2::ggsave("../plots/fig_5.png", height = 7, width = 12)


#+ plotsS

# Supplementary figures ---------------------------------------------------

## S1

make_figure_S1(res)

ggplot2::ggsave("../plots/S_fig_1.pdf", height = 7.5, width = 9,
                useDingbats = FALSE)
ggplot2::ggsave("../plots/S_fig_1.png", height = 7.5, width = 9)
## S2

make_figure_S2(res)

ggplot2::ggsave("../plots/S_fig_2.pdf", height = 7, width = 9,
                useDingbats = FALSE)
ggplot2::ggsave("../plots/S_fig_2.png", height = 7, width = 9)
## S3

make_figure_S3(res)


ggplot2::ggsave("../plots/S_fig_3.pdf", height = 5.5, width = 9,
                useDingbats = FALSE)
ggplot2::ggsave("../plots/S_fig_3.png", height = 5.5, width = 9)

## S4

make_figure_S4(res)

ggplot2::ggsave("../plots/S_fig_4.pdf", height = 3, width = 6.5,
                useDingbats = FALSE)
ggplot2::ggsave("../plots/S_fig_4.png", height = 3, width = 6.5)

## S5

ringbp::box_plot_max_weekly_cases(results = sweep_results,
                                  cap_cases = cap_cases,
                                  extinct_thresold = 0.05,
                                  prop_asym = 0,
                                  filt_control_effectiveness = 0.4,
                                  num_initial_cases = 5,
                                  facet_scales = "fixed",
                                  record_params = F,
                                  y_lim = 100)

ggplot2::ggsave("../plots/S_fig_5_A.pdf", height = 5, width = 10,
                useDingbats = FALSE)
ggplot2::ggsave("../plots/S_fig_5_A.png", height = 5, width = 10)

ringbp::box_plot_max_weekly_cases(results = sweep_results,
                                  cap_cases = cap_cases,
                                  extinct_thresold = 0.05,
                                  prop_asym = 0,
                                  filt_control_effectiveness = 0.4,
                                  num_initial_cases = 40,
                                  facet_scales = "fixed",
                                  record_params = F,
                                  y_lim = 200)

ggplot2::ggsave("../plots/S_fig_5_B.pdf", height = 5, width = 10,
                useDingbats = FALSE)
ggplot2::ggsave("../plots/S_fig_5_B.png", height = 5, width = 10)

ringbp::box_plot_max_weekly_cases(results = sweep_results,
                                  cap_cases = cap_cases,
                                  extinct_thresold = 0.05,
                                  prop_asym = 0.1,
                                  filt_control_effectiveness = 0.4,
                                  num_initial_cases = 20,
                                  facet_scales = "fixed",
                                  record_params = F,
                                  y_lim = 100)

ggplot2::ggsave("../plots/S_fig_5_C.pdf", height = 5, width = 10,
                useDingbats = FALSE)
ggplot2::ggsave("../plots/S_fig_5_C.png", height = 5, width = 10)

## S6

ringbp::make_figure_S6()

ggplot2::ggsave("../plots/S_fig_6.pdf", height = 8, width = 12,
                useDingbats = FALSE)
ggplot2::ggsave("../plots/S_fig_6.png", height = 8, width = 12)


