library(tidyverse)
library(ringbp)

## Make the log file
logs <- file.path("log.txt")
con <- file(logs, open = "wt")
## Send Output to log
sink(con)
sink(con, type = "message")

## Set up scenario gird
scenarios <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  grouped_parameters = list(tibble::tibble(
    disease = c("SARs", "Transition", "Flu"),
    index_R0 = c(1.5, 2.5, 3.5),
    disp.com= c(0.16, 0.5, 1)
  )),
  inf_group = list(tibble::tibble(
    latent = c("short","medium","long","very long"),
    inf_shape = c(2.0,2.6,3.3,3.8),
    inf_scale = c(5.9,7.1,8.7,9.7)
  )),
  delay_group = list(tibble::tibble(
    delay = c("short","medium","long"),
    delay_shape = c(1.8,2,2.4),
    delay_scale = c(3.5,5.5,7.5)
  )),
  control_effectiveness = seq(0,1,0.2)) %>%
  tidyr::unnest("grouped_parameters") %>%
  tidyr::unnest("inf_group") %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())


sim_with_params <- purrr::partial(ringbp::wuhan_sim,
                                  num.initial.cases = 1,
                                  num.initial.clusters = 40,
                                  cap_cases = 5000, cap_max_days = 16*7,
                                  r0isolated = 0, disp.iso = 0.01,
                                  incub_shape = 2.322737, incub_scale = 6.492272)
## Default is to run sequntially on a single core
# future::plan(future::sequential)
## Set up multicore if using see ?future::plan for details
future::plan(future::multiprocess)

## Run paramter sweep
sweep_results <- ringbp::parameter_sweep(scenarios, sim_fn = sim_with_params, samples = 2000)

# sweep_results %>% group_by(scenario) %>% mutate(prob_extinct = extinct_prob(sims[[1]],cap_cases = 5000))

saveRDS(sweep_results,file = "sweep_results.RDS")

sink(type = "message")
sink()
