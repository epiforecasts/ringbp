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

no.samples <- 2000

scenarios <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("SARS", "Wuhan"),
    delay_shape = c(1.651524, 2.305172),
    delay_scale = c(4.287786, 9.483875)
  )),
  inc_meanlog = 1.434065,
  inc_sdlog = 0.6612,
  inf_shape = 2.115779,
  inf_rate = 0.6898583,
  inf_shift = 3,
  index_R0 = c(1.1, 1.6, 2),
  prop.asym = c(0.2,0.4,0.5,0.7),
  control_effectiveness = seq(0, 1, 0.2),
  num.initial.cases = c(5)) %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

cap_cases <- 2000
max_days <- 300
## Parameterise fixed paramters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = max_days,
                                  cap_cases = cap_cases,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.16,
                                  quarantine = TRUE)


#+ setup_multicore, cache = FALSE
## Set up multicore if using see ?future::plan for details
## Use the workers argument to control the number of cores used.
#future::plan("multiprocess")


#+ full_run

tic()
## Run paramter sweep
sweep_results <- ringbp::parameter_sweep(scenarios,
                                         sim_fn = sim_with_params,
                                         samples = no.samples,
                                         show_progress = TRUE)

toc()


# #+ writeout

 saveRDS(sweep_results, file = "../../data-raw/lucas_quarentine_res.rds")



#+ plots1


#+ plots2, cache = FALSE

# Load in results  -------------------------------------------------------


res <- sweep_results %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(pext = extinct_prob(sims[[1]], cap_cases = cap_cases)) %>%
  dplyr::ungroup(scenario)

#+ plots3


#+ plotsS, eval = TRUE, cache = FALSE


res %>% 
  filter(delay == 'SARS') %>% 
  mutate(prop.asym = factor(prop.asym, labels = c('asympt = 20%', '40%', '50%', '70%'))) %>%
  ggplot(aes(control_effectiveness, pext, colour = factor(index_R0))) +
  geom_line() + 
  geom_point() + 
  facet_grid(~ prop.asym) +
  ggtitle('SARS')


res %>% 
  filter(delay == 'Wuhan') %>% 
  mutate(prop.asym = factor(prop.asym, labels = c('asympt = 20%', '40%', '50%', '70%'))) %>%
  ggplot(aes(control_effectiveness, pext, colour = factor(index_R0))) +
  geom_line() + 
  geom_point() + 
  facet_grid(~ prop.asym) +
  ggtitle('Wuhan')


