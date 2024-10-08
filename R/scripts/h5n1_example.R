library(ringbp)
library(dplyr)
library(tidyr)
library(purrr)
library(future)
library(tibble)
library(data.table)
library(ggplot2)

# Setting range of mean values for incubation period so we can simulate
# outbreaks across this range of values
incubation_period_mean_values <- seq(3, 7, 0.5)

# Setting up the various scenarios. Only varying incubation period currently
# Most other values chosen very approximately for the time-being
# See the original script, at:
# https://github.com/cmmid/ringbp/blob/master/inst/scripts/generate_results.R
# For a more detailed example setting up various scenarios
scenarios <- tidyr::expand_grid(
  delay_shape = 1.651524,
  delay_scale = 4.287786,
  index_R0 = 0.3,
  inc_mean = incubation_period_mean_values, 
  inc_sd = 1,
  si_mean = 1.67,
  si_sd = 0.79,
  quarantine = FALSE,
  prop.asym = 0,
  control_effectiveness = 0.8,
  num.initial.cases = 20) |> 
  dplyr::mutate(scenario = 1:dplyr::n()) |> 
  data.table()

# Parameterise fixed parameters
sim_with_params <- purrr::partial(
  ringbp::scenario_sim,
  cap_max_days = 365,
  cap_cases = 50,
  r0isolated = 0,
  disp.iso = 1,
  disp.com = 0.05)

# Use the workers argument to control the number of cores used.
future::plan(multisession, workers = 7)

# Run parameter sweep
sweep_results <- ringbp::parameter_sweep(
  scenarios, sim_fn = sim_with_params, samples = 1000)

# Combine all the simulation results into a single data.table
dt_sim <- rbindlist(lapply(1:nrow(sweep_results), function(i) {
  data.table(scenario = sweep_results[i]$scenario, sweep_results[i]$sims[[1]])
}))

# Merge scenario ID with incubation period values for plotting
dt_scenario_id_lookup <- data.table(
  scenario = min(dt_sim$scenario):max(dt_sim$scenario),
  incubation_period = incubation_period_mean_values)

dt_sim_plot <- merge(dt_sim, dt_scenario_id_lookup, by = "scenario")

# Summarise the simulations using median and 95% quantiles
dt_sim_plot_sum <- dt_sim_plot[, .(
  me = quantile(cumulative, 0.5),
  lo = quantile(cumulative, 0.025),
  hi = quantile(cumulative, 0.975)), 
  by = .(week, incubation_period)]

# Plotting the cumulative numbers of cases by incubation period
dt_sim_plot_sum |> 
  ggplot() + 
  geom_line(aes(x = week, y = me, colour = factor(incubation_period))) + 
  geom_ribbon(aes(x = week, ymin = lo, ymax = hi, fill = factor(incubation_period)), alpha = 0.5) + 
  facet_wrap(~incubation_period) +
  theme_bw() + 
  labs(x = "Time (weeks)", y = "Cumulative number of cases",
       colour = "Incubation period", fill = "Incubation period",
       title = "Number of new cases of H5N1, varying the mean of the incubation period") + 
  theme(strip.text.x.top = element_blank()) 



