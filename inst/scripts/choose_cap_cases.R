#'---
#'output:
#'  pdf_document:
#'    number_sections: true
#'    toc: true
#'    toc_depth: 2
#'title: "Chosing cao_cases value"
#'author: Tim Lucas
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
library(dplyr)

devtools::load_all()

git2r::revparse_single('.',"HEAD")$sha


#+ read_data

res <- readRDS('../../data-raw/res.rds')



#+ calc

end_sim <- function(x){
  x %>% 
    group_by(sim) %>% 
    summarise(sim_finished = min(which(cumulative == max(cumulative)))) %>% 
    dplyr::pull(sim_finished)
}


sim_finished <- lapply(res$sims, end_sim)

sim_finished <- do.call(c, sim_finished) 

hist(sim_finished)

mean_finished <- sapply(1:50, function(x) mean(sim_finished < x))

plot(mean_finished ~ c(1:50), type = 'l')
abline(h = 0.95, col = 'blue')
abline(h = 0.99, col = 'red')

c(1:50)[min(which(mean_finished > 0.95))]
c(1:50)[min(which(mean_finished > 0.99))]


#+ calc2


find_max_cases <- function(x){
  extinct <- 
    x %>% 
      detect_extinct(cap_cases = 5000, week_range = 50:53)
  
  max <-
    x %>% 
      filter(sim %in% extinct$sim[extinct$extinct == 1]) %>% 
      group_by(sim) %>% 
      summarise(cumulative = max(cumulative)) %>% 
      dplyr::pull(cumulative)
    
  return(max)
}


max_cases <- lapply(res$sims, find_max_cases)

max_cases <- do.call(c, max_cases) 

hist(max_cases)


mean_extinct <- sapply(1:5000, function(x) mean(max_cases < x))

plot(mean_extinct ~ c(1:5000), type = 'l', ylim = c())
abline(h = 0.95, col = 'blue')
abline(h = 0.99, col = 'red')

c(1:5000)[min(which(mean_extinct > 0.95))]
c(1:5000)[min(which(mean_extinct > 0.99))]




#+ test_timings



scenarios <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("SARS", "Wuhan"),
    delay_shape = c(1.651524),
    delay_scale = c(4.287786)
  )),
  k_group = list(tibble::tibble(
    theta = c("<1%", "15%", "30%"),
    k = c(30, 1.95, 0.7)
  )),
  index_R0 = c(1.5, 3.5),
  prop.asym = c(0.1),
  control_effectiveness = seq(0.2),
  num.initial.cases = c(5)) %>%
  tidyr::unnest("k_group") %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())


#+ time1

## Parameterise fixed paramters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = 365,
                                  cap_cases = 5000,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.16,
                                  quarantine = FALSE)

tic()
## Run paramter sweep
sweep_results <- ringbp::parameter_sweep(scenarios,
                                         sim_fn = sim_with_params,
                                         samples = 300,
                                         show_progress = TRUE)
toc()



#+ time2

## Parameterise fixed paramters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = 365,
                                  cap_cases = 2500,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.16,
                                  quarantine = FALSE)

tic()
## Run paramter sweep
sweep_results <- ringbp::parameter_sweep(scenarios,
                                         sim_fn = sim_with_params,
                                         samples = 300,
                                         show_progress = TRUE)
toc()



#+ time2

## Parameterise fixed paramters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = 365,
                                  cap_cases = 1000,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.16,
                                  quarantine = FALSE)

tic()
## Run paramter sweep
sweep_results <- ringbp::parameter_sweep(scenarios,
                                         sim_fn = sim_with_params,
                                         samples = 300,
                                         show_progress = TRUE)
toc()






