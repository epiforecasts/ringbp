#'---
#'output:
#'  pdf_document:
#'    number_sections: true
#'title: "Further analysis of COVID branching process"
#'author: Tim Lucas and Emma Davis
#'fontsize: 8pt
#'geometry: margin=0.5in
#'---

#' # Major model update 1
#'
#' - Quarentine now applies to asymptomatics
#' - Examine delay for contact tracing
#' - Change onset delay to either a 1 day delay or non-adherence.
#' - People that are quarentined have adherence of 1.

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

set.seed(200503)


#' Delay shape is adherence probability
#'
#' Cap cases was chosen in a seperate analysis (choose_cap.R or something.)

#+ create_parameters

no.samples <- 1000

scenarios <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("Adherence"),
    delay_shape = c(0.9),
    delay_scale = 1
  )),
  inc_meanlog = 1.434065,
  inc_sdlog = 0.6612,
  inf_shape = 2.115779,
  inf_rate = 0.6898583,
  inf_shift = 3,
  min_quar_delay = 1,
  max_quar_delay = c(1,4,8),
  index_R0 = c(1.1,1.3),
  prop.asym = c(0.4),
  control_effectiveness = seq(0.4, 1, 0.2),
  self_report = c(0,0.4,0.8),
  test_delay = c(0,2), #time from isolation to test result
  sensitivity = c(0.65,0.8,0.95), #percent of cases detected
  precaution = c(0,7), #this could be between 0 and 7? Number of days stay in isolation after negative test
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



#+ full_run

tic()
## Run parameter sweep
sweep_results2 <- ringbp::parameter_sweep(scenarios,
                                         sim_fn = sim_with_params,
                                         samples = no.samples,
                                         show_progress = TRUE)

toc()


# #+ writeout

#saveRDS(sweep_results, file = "data-raw/res_20200504.rds")


#' Panel A is now redundant and has been replaced with an adherence probability.
#+ plots1, eval = TRUE

ringbp::make_figure_2()

#+ plots2, cache = FALSE

# Load in results  -------------------------------------------------------


res <- sweep_results %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(pext = extinct_prob(sims[[1]], cap_cases = cap_cases, week_range = 40:42)) %>%
  dplyr::ungroup(scenario)

#+ plots3


#+ plotsS, eval = TRUE, cache = FALSE, fig.height = 5, fig.width = 9


res %>%
  filter(max_quar_delay == 1) %>%
  mutate(prop.asym = factor(prop.asym, labels = c('asympt = 40%'))) %>%
  mutate(adherence = factor(delay_shape, labels = c('adhere = 90%'))) %>%
  mutate(index_R0 = factor(index_R0)) %>%
  ggplot(aes(control_effectiveness, 1 - pext, colour = index_R0)) +
  geom_line() +
  geom_point() +
  facet_grid(adherence ~ prop.asym) +
  ggtitle('Contact trace delay is 1') +
  ylab('Prob. large outbreak')



res %>%
  filter(delay_shape == 0.9) %>%
  mutate(prop.asym = factor(prop.asym, labels = c('asympt = 40%'))) %>%
  mutate(max_quar_delay = factor(max_quar_delay, labels = c('1 day trace delay', '4 days', '8 days'))) %>%
  mutate(index_R0 = factor(index_R0)) %>%
  ggplot(aes(control_effectiveness, 1 - pext, colour = index_R0)) +
  geom_line() +
  geom_point() +
  facet_grid(max_quar_delay ~ prop.asym) +
  ggtitle('Adherence is 90%') +
  ylab('Prob. large outbreak')




#+ by_size, eval = TRUE, cache = TRUE, fig.height = 5, fig.width = 9

res2 <- list()
week_range <- 40:42

sweep_results2 <-
  sweep_results %>%
  filter(prop.asym == 0.4,
         control_effectiveness > 0.3,
         delay_shape == 0.9)

for(i in seq_len(nrow(sweep_results2))){
  #print(i)
  tmp <- sweep_results2$sims[i][[1]]
  tmp <-
    tmp %>%
    dplyr::group_by(sim) %>% # group by simulation run
    mutate(max_weekly = max(weekly_cases),
           total = max(cumulative)) %>%
    dplyr::filter(week %in% week_range) %>%
    dplyr::summarise(extinct =
                       ifelse(all(weekly_cases == 0 &
                                    cumulative < cap_cases),
                              1, 0),
                     max_weekly = max(max_weekly),
                     total = max(total)) %>%
    dplyr::ungroup()
  tmp <-
    tmp %>%
    mutate(index_R0 = sweep_results2$index_R0[i],
           control_effectiveness = sweep_results2$control_effectiveness[i],
           max_quar_delay = sweep_results2$max_quar_delay[i])

  res2[[i]] <- tmp
}
res2 <- do.call(rbind, res2)



#+ plots_by_size2, eval = TRUE, cache= FALSE

# Cumulation is *the number of runs, with that many total, that went extinct*.
# At cumulative size = 4,
#   cumulation is 0 (everything has more than 4 total cases)
#   so p(outbreak) is total outbreaks / total runs

# at cumulative size = 10
#   cumulation is +ve (say 100)
#   So 100 runs reached 10 but still went extinct.
#   Therefore 1900 runs carried on.
#   so p(outbreak) is (total outbreaks) / (total runs - cumulation)

# we want:
# total outbreaks / n
total_cumulative_distr <-
  res2 %>%
  mutate(total = ifelse(total > 2000, 2000, total)) %>%
  group_by(index_R0, control_effectiveness, max_quar_delay) %>%
  do(res = tibble(cumdistr = nrow(.) * ecdf(.$total)(4:2000),
                  total = 4:2000,
                  outbreaks = nrow(.) - sum(.$extinct),
                  runs = nrow(.),
                  max_quar_delay = .$max_quar_delay[1],
                  index_R0 = .$index_R0[1],
                  control_effectiveness = .$control_effectiveness[1],
                  poutbreak = (outbreaks) / (runs - cumdistr)))


total_cumulative_distr <- do.call(rbind, total_cumulative_distr$res) %>%
  mutate(index_R0 = factor(index_R0, labels = c('R0 = 1.1', '1.3'))) %>%
  mutate(max_quar_delay = factor(max_quar_delay, labels = c('1 day trace delay', '4 days', '8 days'))) %>%
  filter(outbreaks != 0)


ggplot(total_cumulative_distr,
       aes(total, poutbreak, colour = factor(control_effectiveness), group = factor(control_effectiveness))) +
  geom_line() +
  xlim(0, 1000) +
  facet_wrap(index_R0 ~ max_quar_delay, scale = 'free_y') +
  ylab('Prob. large outbreak') +
  guides(colour=guide_legend(title="Prop. Traced")) +
  ggtitle('Prob of outbreak as size of current epidemic increases')


#+ plots_by_max_weekly, cache = FALSE

total_cumulative_distr <-
  res2 %>%
  group_by(index_R0, control_effectiveness, max_quar_delay) %>%
  do(res = tibble(cumdistr = sum(.$extinct) * ecdf(.$max_weekly[.$extinct == 1])(1:max(.$max_weekly)),
                  max_max_weekly = max(.$max_weekly),
                  max_weekly = 1:max(.$max_weekly),
                  extinct = sum(.$extinct),
                  outbreaks = nrow(.) - sum(.$extinct),
                  max_quar_delay = .$max_quar_delay[1],
                  runs = nrow(.),
                  index_R0 = .$index_R0[1],
                  control_effectiveness = .$control_effectiveness[1],
                  poutbreak = (outbreaks) / (runs - cumdistr)))


total_cumulative_distr <-
  do.call(rbind, total_cumulative_distr$res) %>%
  filter(poutbreak < 1) %>%
  mutate(index_R0 = factor(index_R0, labels = c('R0 = 1.1', '1.3'))) %>%
  mutate(max_quar_delay = factor(max_quar_delay, labels = c('1 day trace delay', '4 days', '8 days')))



ggplot(total_cumulative_distr,
       aes(max_weekly, poutbreak, colour = factor(control_effectiveness), group = factor(control_effectiveness))) +
  geom_line() +
  facet_wrap(index_R0 ~ max_quar_delay, scale = 'free_x') +
  ylab('Prob. large outbreak') +
  guides(colour=guide_legend(title="Prop. Traced")) +
  ggtitle('Prob of outbreak with size of worst week')



