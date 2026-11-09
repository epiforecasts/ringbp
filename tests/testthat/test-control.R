set.seed(515)

test_that("extinct_prob works as expected", {
  cap <- 100
  sims <- 5
  res <- scenario_sim(
    n = sims,
    initial_cases = 5,
    offspring = offspring_opts(
      community = \(n) rnbinom(n = n, mu = 2.5, size = 0.16),
      isolated = \(n) rnbinom(n = n, mu = 0, size = 1)
    ),
    delays = delay_opts(
      incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
      onset_to_isolation = \(n) rweibull(n = n, shape = 2.5, scale = 5)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 0,
      presymptomatic_transmission = 0.3,
      symptomatic_traced = 0
    ),
    interventions = intervention_opts(),
    sim = sim_opts(cap_max_days = 105, cap_cases = cap)
  )

  r1 <- extinct_prob(res, extinction_time = 12:14)
  expect_true(r1 <= 1)
  expect_true(r1 >= 0)
  expect_length(r1, 1)

  is_wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
    abs(x - round(x)) < tol
  }
  expect_true(is_wholenumber(r1 * sims))

  # Manually build an output with known proportion of extinctions
  res2 <- res$outbreak_ts[c(1, 2, 1, 2), ]
  res2$sim <- c(1, 1, 2, 2)
  # Enforce that second sim did not have cases in final week.
  # Ignoring cases_per_gen becayse I think this function works on weekly cases.
  res2$weekly_cases[4] <- 0
  res2$cumulative[4] <- res2$cumulative[3]

  # Enforce that first sim has cases both weeks.
  # Ignoring cases_per_gen becayse I think this function works on weekly cases.
  res2$weekly_cases[1:2] <- c(1, 1)
  res2$cumulative[1:2] <- c(1, 2)

  res2 <- list(outbreak_ts = res2, outbreak_stats = res$outbreak_stats)
  class(res2) <- "ringbp"

  r2 <- extinct_prob(res2, extinction_time = 1)
  expect_equal(r2, 0.5)

  # Run some sims with almost certain outputs
  # Very high r0, shouldn't ever go extinct.
  res3 <- scenario_sim(
    n = sims,
    initial_cases = 5,
    offspring = offspring_opts(
      community = \(n) rnbinom(n = n, mu = 100, size = 0.16),
      isolated = \(n) rnbinom(n = n, mu = 100, size = 1)
    ),
    delays = delay_opts(
      incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
      onset_to_isolation = \(n) rweibull(n = n, shape = 2.5, scale = 5)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 0,
      presymptomatic_transmission = 0.3,
      symptomatic_traced = 0
    ),
    interventions = intervention_opts(),
    sim = sim_opts(cap_max_days = 105, cap_cases = cap)
  )
  r3 <- extinct_prob(res3)
  expect_equal(r3, 0)

  # r0 of 0, should always go extinct.
  res3 <- scenario_sim(
    n = sims,
    initial_cases = 5,
    offspring = offspring_opts(
      community = \(n) rnbinom(n = n, mu = 0, size = 0.16),
      isolated = \(n) rnbinom(n = n, mu = 0, size = 1)
    ),
    delays = delay_opts(
      incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
      onset_to_isolation = \(n) rweibull(n = n, shape = 2.5, scale = 5)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 0,
      presymptomatic_transmission = 0.3,
      symptomatic_traced = 0
    ),
    interventions = intervention_opts(),
    sim = sim_opts(cap_max_days = 105, cap_cases = cap)
  )
  r3 <- extinct_prob(res3)
  expect_equal(r3, 1)
})

test_that("extinct_prob extinction_time argument works", {
  cap <- 100
  sims <- 2
  res <- scenario_sim(
    n = 2,
    initial_cases = 5,
    offspring = offspring_opts(
      community = \(n) rnbinom(n = n, mu = 2.5, size = 0.16),
      isolated = \(n) rnbinom(n = n, mu = 0, size = 1)
    ),
    delays = delay_opts(
      incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
      onset_to_isolation = \(n) rweibull(n = n, shape = 2.5, scale = 5)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 0,
      presymptomatic_transmission = 0.3,
      symptomatic_traced = 0
    ),
    interventions = intervention_opts(),
    sim = sim_opts(cap_max_days = 105, cap_cases = cap)
  )

  # Manually build an output with known proportion of extinctions
  res2 <- res$outbreak_ts[c(1, 2, 1, 2), ]
  res2$sim <- c(1, 1, 2, 2)

  # Enforce that second sim did not have cases in final week.
  # Ignoring cases_per_gen becayse I think this function works on weekly cases.
  res2$weekly_cases[4] <- 0
  res2$cumulative[4] <- res2$cumulative[3]

  # Enforce that first sim has cases both weeks.
  # Ignoring cases_per_gen becayse I think this function works on weekly cases.
  res2$weekly_cases[1:2] <- c(1, 1)
  res2$cumulative[1:2] <- c(1, 2)

  res2 <- list(outbreak_ts = res2, outbreak_stats = res$outbreak_stats)
  class(res2) <- "ringbp"

  r2 <- extinct_prob(res2, extinction_time = 1)
  expect_equal(r2, 0.5)

  # Now add a week and test extinction_time = 1:2
  # Simple case of cases in week 1 and 2
  res3 <- res$outbreak_ts[c(1, 2, 3), ]
  res3$weekly_cases[1:3] <- c(1, 1, 1)
  res3$cumulative[1:3] <- c(1, 2, 3)
  res3 <- list(outbreak_ts = res3, outbreak_stats = res$outbreak_stats)
  class(res3) <- "ringbp"
  r3 <- extinct_prob(res3, extinction_time = 1:2)
  expect_equal(r3, 0)

  # Simple case of no cases in week 1 or 2
  res4 <- res$outbreak_ts[c(1, 2, 3), ]
  res4$weekly_cases[1:3] <- c(1, 0, 0)
  res4$cumulative[1:3] <- c(1, 1, 1)
  res4 <- list(outbreak_ts = res4, outbreak_stats = res$outbreak_stats)
  class(res4) <- "ringbp"
  r4 <- extinct_prob(res4, extinction_time = 1:2)
  expect_equal(r4, 1)

  # Case of cases in week 1 but not 2 (by the definition used in this function
  # this is not an extinction). Test here that extinction_time 1:2 says no
  # extintion but extinction_time 2 says extinction.
  res5 <- res$outbreak_ts[c(1, 2, 3), ]
  res5$weekly_cases[1:3] <- c(1, 1, 0)
  res5$cumulative[1:3] <- c(1, 2, 2)
  res5 <- list(outbreak_ts = res5, outbreak_stats = res$outbreak_stats)
  class(res5) <- "ringbp"
  r5 <- extinct_prob(res5, extinction_time = 1:2)
  expect_equal(r5, 0)

  r5b <- extinct_prob(res5, extinction_time = 2)
  expect_equal(r5b, 1)

  # Case of cases in week 2 but not 1 (by all sensible definitions is not an
  # extinction). Test here that extinction_time 1:2 says no extinction and
  # neither does extinction_time 2.
  res6 <- res$outbreak_ts[c(1, 2, 3), ]
  res6$weekly_cases[1:3] <- c(1, 0, 1)
  res6$cumulative[1:3] <- c(1, 1, 2)
  res6 <- list(outbreak_ts = res6, outbreak_stats = res$outbreak_stats)
  class(res6) <- "ringbp"
  r6 <- extinct_prob(res6, extinction_time = 1:2)
  expect_equal(r6, 0)

  r6b <- extinct_prob(res6, extinction_time = 2)
  expect_equal(r6b, 0)
})

test_that("detect_extinct works", {
  cap <- 100
  sims <- 2
  res <- scenario_sim(
    n = 2,
    initial_cases = 5,
    offspring = offspring_opts(
      community = \(n) rnbinom(n = n, mu = 2.5, size = 0.16),
      isolated = \(n) rnbinom(n = n, mu = 0, size = 1)
    ),
    delays = delay_opts(
      incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
      onset_to_isolation = \(n) rweibull(n = n, shape = 2.5, scale = 5)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 0,
      presymptomatic_transmission = 0.3,
      symptomatic_traced = 0
    ),
    interventions = intervention_opts(),
    sim = sim_opts(cap_max_days = 105, cap_cases = cap)
  )

  # Manually build an output with known proportion of extinctions
  res2 <- res$outbreak_ts[c(1, 2, 1, 2), ]
  res2$sim <- c(1L, 1L, 2L, 2L)
  # Enforce that second sim did not have cases in final week.
  # Ignoring cases_per_gen becayse I think this function works on weekly cases.
  res2$weekly_cases[4] <- 0
  res2$cumulative[4] <- res2$cumulative[3]
  # Enforce that first sim has cases both weeks.
  # Ignoring cases_per_gen becayse I think this function works on weekly cases.
  res2$weekly_cases[1:2] <- c(1, 1)
  res2$cumulative[1:2] <- c(1, 2)

  res2 <- list(outbreak_ts = res2, outbreak_stats = res$outbreak_stats)
  class(res2) <- "ringbp"

  r2 <- detect_extinct(res2, extinction_time = 1)
  expect2 <- data.table(sim = c(1L, 2L), extinct = c(0L, 1L))
  expect_identical(r2, expect2)

  # Now add a week and test extinction_time = 1:2
  # Simple case of cases in week 1 and 2
  res3 <- res$outbreak_ts[c(1, 2, 3), ]
  res3$weekly_cases[1:3] <- c(1, 1, 1)
  res3$cumulative[1:3] <- c(1, 2, 3)
  res3 <- list(outbreak_ts = res3, outbreak_stats = res$outbreak_stats)
  class(res3) <- "ringbp"
  r3 <- detect_extinct(res3, extinction_time = 1:2)
  expect3 <- data.table(sim = 1L, extinct = 0L)
  expect_identical(r3, expect3)

  # Simple case of no cases in week 1 or 2
  res4 <- res$outbreak_ts[c(1, 2, 3), ]
  res4$weekly_cases[1:3] <- c(1, 0, 0)
  res4$cumulative[1:3] <- c(1, 1, 1)
  res4 <- list(outbreak_ts = res4, outbreak_stats = res$outbreak_stats)
  class(res4) <- "ringbp"
  r4 <- detect_extinct(res4, extinction_time = 1:2)
  expect4 <- data.table(sim = 1L, extinct = 1L)
  expect_identical(r4, expect4)

  # Case of cases in week 1 but not 2 (by the definition used in this function
  # this is not an extinction). Test here that extinction_time 1:2 says
  # extinction by extinction_time 2 does not.
  res5 <- res$outbreak_ts[c(1, 2, 3), ]
  res5$weekly_cases[1:3] <- c(1, 1, 0)
  res5$cumulative[1:3] <- c(1, 2, 2)
  res5 <- list(outbreak_ts = res5, outbreak_stats = res$outbreak_stats)
  class(res5) <- "ringbp"
  r5 <- detect_extinct(res5, extinction_time = 1:2)
  expect5 <- data.table(sim = 1L, extinct = 0L)
  expect_identical(r5, expect5)

  r5b <- detect_extinct(res5, extinction_time = 2)
  expect5b <- data.table(sim = 1L, extinct = 1L)
  expect_identical(r5b, expect5b)

  # Case of cases in week 2 but not 1 (by all sensible definitions is not an
  # extinction). Test here that extinction_time 1:2 says no extinction and
  # neither does extinction_time 2.
  res6 <- res$outbreak_ts[c(1, 2, 3), ]
  res6$weekly_cases[1:3] <- c(1, 0, 1)
  res6$cumulative[1:3] <- c(1, 1, 2)
  res6 <- list(outbreak_ts = res6, outbreak_stats = res$outbreak_stats)
  class(res6) <- "ringbp"
  r6 <- detect_extinct(res6, extinction_time = 1:2)
  expect6 <- data.table(sim = 1L, extinct = 0L)
  expect_identical(r6, expect6)

  r6b <- detect_extinct(res6, extinction_time = 2)
  expect6b <- data.table(sim = 1L, extinct = 0L)
  expect_identical(r6b, expect6b)
})

test_that("extinct_prob works with other aggregation intervals", {
  # no transmission so all outbreaks are extinct after the initial cases
  sim_args <- list(
    n = 2,
    initial_cases = 1,
    offspring = offspring_opts(
      community = \(n) rep(0, n),
      isolated = \(n) rep(0, n)
    ),
    delays = delay_opts(
      incubation_period = \(n) rep(10.5, n),
      onset_to_isolation = \(n) rep(1, n)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 0,
      presymptomatic_transmission = 0.3,
      symptomatic_traced = 0
    ),
    interventions = intervention_opts()
  )
  daily <- do.call(
    scenario_sim,
    c(sim_args, list(sim = sim_opts(cap_max_days = 50, aggregation = "daily")))
  )
  # initial cases have onset on day 10
  expect_message(
    expect_identical(extinct_prob(daily, extinction_time = 10), 0),
    regexp = "no new cases within days: 10 to 49"
  )
  expect_identical(extinct_prob(daily, extinction_time = 11), 1)

  # line list is aggregated to daily
  none <- do.call(
    scenario_sim,
    c(sim_args, list(sim = sim_opts(cap_max_days = 50, aggregation = "none")))
  )
  expect_message(
    expect_warning(
      expect_identical(extinct_prob(none, extinction_time = 10), 0),
      regexp = "will be aggregated to daily cases"
    ),
    regexp = "no new cases within days: 10 to 49"
  )
  expect_warning(
    expect_identical(extinct_prob(none, extinction_time = 11), 1),
    regexp = "will be aggregated to daily cases"
  )

  # integer number of days, onset in period 2 (days 10 to 14)
  period <- do.call(
    scenario_sim,
    c(sim_args, list(sim = sim_opts(cap_max_days = 50, aggregation = 5)))
  )
  expect_message(
    expect_identical(extinct_prob(period, extinction_time = 2), 0),
    regexp = "no new cases within 5-day periods: 2 to 9"
  )
  expect_identical(extinct_prob(period, extinction_time = 3), 1)
})

test_that("detect_extinct errors when aggregation attribute is missing", {
  res <- scenario_sim(
    n = 2,
    initial_cases = 1,
    offspring = offspring_opts(
      community = \(n) rep(0, n),
      isolated = \(n) rep(0, n)
    ),
    delays = delay_opts(
      incubation_period = \(n) rep(1, n),
      onset_to_isolation = \(n) rep(1, n)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 0,
      presymptomatic_transmission = 0.3,
      symptomatic_traced = 0
    ),
    interventions = intervention_opts(),
    sim = sim_opts(cap_max_days = 49)
  )
  setattr(res$outbreak_ts, name = "aggregation", value = NULL)
  expect_error(
    detect_extinct(res, extinction_time = 2),
    regexp = "`scenario` is missing the `aggregation` attribute"
  )
  setattr(res$outbreak_ts, name = "extinct", value = NULL)
  expect_error(
    detect_extinct(res),
    regexp = "`extinction_time` not specified and `scenario` is missing"
  )
})

test_that("detect_extinct errors for case line list without ringbp class", {
  res <- scenario_sim(
    n = 2,
    initial_cases = 1,
    offspring = offspring_opts(
      community = \(n) rep(0, n),
      isolated = \(n) rep(0, n)
    ),
    delays = delay_opts(
      incubation_period = \(n) rep(1, n),
      onset_to_isolation = \(n) rep(1, n)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 0,
      presymptomatic_transmission = 0.3,
      symptomatic_traced = 0
    ),
    interventions = intervention_opts(),
    sim = sim_opts(cap_max_days = 50, aggregation = "none")
  )
  res <- unclass(res)
  expect_error(
    detect_extinct(res, extinction_time = 2),
    regexp = "scenario.*Must inherit from class 'ringbp'"
  )
})

# outbreak scenario for control/extinction unit tests
control_scenario <- scenario_sim(
  n = 2,
  initial_cases = 5,
  offspring = offspring_opts(
    community = \(n) rnbinom(n = n, mu = 2.5, size = 0.16),
    isolated = \(n) rnbinom(n = n, mu = 0, size = 1)
  ),
  delays = delay_opts(
    incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
    onset_to_isolation = \(n) rweibull(n = n, shape = 2.5, scale = 5)
  ),
  event_probs = event_prob_opts(
    asymptomatic = 0,
    presymptomatic_transmission = 0.3,
    symptomatic_traced = 0
  ),
  interventions = intervention_opts(),
  sim = sim_opts(cap_max_days = 98, cap_cases = 100)
)

# no transmission, 3 initial cases with symptom onset on day 10 (week 1)
no_transmission_args <- list(
  n = 2,
  initial_cases = 3,
  offspring = offspring_opts(
    community = \(n) rep(0, n),
    isolated = \(n) rep(0, n)
  ),
  delays = delay_opts(
    incubation_period = \(n) rep(10.5, n),
    onset_to_isolation = \(n) rep(1, n)
  ),
  event_probs = event_prob_opts(
    asymptomatic = 0,
    presymptomatic_transmission = 0.3,
    symptomatic_traced = 0
  ),
  interventions = intervention_opts()
)

test_that("extinct_prob matches control_prob when control_threshold = 0", {
  expect_identical(
    control_prob(
      scenario = control_scenario,
      control_time = 5:10,
      control_threshold = 0
    ),
    extinct_prob(scenario = control_scenario, extinction_time = 5:10)
  )
  expect_identical(
    control_prob(scenario = control_scenario),
    extinct_prob(scenario = control_scenario)
  )
})

test_that("detect_control matches detect_extinct when control_threshold = 0", {
  control <- detect_control(
    scenario = control_scenario,
    control_time = 5:10,
    control_threshold = 0
  )
  extinct <- detect_extinct(
    scenario = control_scenario,
    extinction_time = 5:10
  )
  expect_identical(colnames(control), c("sim", "control"))
  expect_identical(colnames(extinct), c("sim", "extinct"))
  data.table::setnames(control, old = "control", new = "extinct")
  expect_identical(control, extinct)
})

test_that("detect_control works for control_threshold > 0", {
  no_transmission_scenario <- do.call(
    scenario_sim,
    c(no_transmission_args, list(sim = sim_opts(cap_max_days = 98)))
  )
  # r0 of 0.8, should go extinct after producing a few cases
  low_transmission_scenario <- scenario_sim(
    n = 2,
    initial_cases = 5,
    offspring = offspring_opts(
      community = \(n) rnbinom(n = n, mu = 0.8, size = 1),
      isolated = \(n) rnbinom(n = n, mu = 0, size = 1)
    ),
    delays = delay_opts(
      incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
      onset_to_isolation = \(n) rweibull(n = n, shape = 2.5, scale = 5)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 0,
      presymptomatic_transmission = 0.3,
      symptomatic_traced = 0
    ),
    interventions = intervention_opts(),
    sim = sim_opts(cap_max_days = 98, cap_cases = 100)
  )
  # r0 of 100, should always go to case cap
  high_transmission_scenario <- scenario_sim(
    n = 2,
    initial_cases = 5,
    offspring = offspring_opts(
      community = \(n) rep(100, n),
      isolated = \(n) rep(100, n)
    ),
    delays = delay_opts(
      incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
      onset_to_isolation = \(n) rweibull(n = n, shape = 2.5, scale = 5)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 0,
      presymptomatic_transmission = 0.3,
      symptomatic_traced = 0
    ),
    interventions = intervention_opts(),
    sim = sim_opts(cap_max_days = 98, cap_cases = 100)
  )
  # no transmission scenario is always controlled
  expect_identical(
    detect_control(
      scenario = no_transmission_scenario,
      control_time = 0,
      control_threshold = 10
    ),
    data.table(sim = c(1L, 2L), control = c(1L, 1L))
  )
  # outbreak with low transmission has fewer than 50 cases a week
  expect_identical(
    detect_control(
      scenario = low_transmission_scenario,
      control_time = 5,
      control_threshold = 50
    ),
    data.table(sim = c(1L, 2L), control = c(1L, 1L))
  )
  # outbreak with high transmission reaches the case cap so is never
  # controlled, even with a threshold above the case cap
  expect_identical(
    detect_control(
      scenario = high_transmission_scenario,
      control_time = 5,
      control_threshold = 1000
    ),
    data.table(sim = c(1L, 2L), control = c(0L, 0L))
  )
})

test_that("detect_control is inclusive of control_threshold", {
  res <- do.call(
    scenario_sim,
    c(no_transmission_args, list(sim = sim_opts(cap_max_days = 49)))
  )
  # 3 cases in week 1
  expect_identical(
    detect_control(res, control_time = 1, control_threshold = 3)$control,
    c(1L, 1L)
  )
  expect_identical(
    detect_control(res, control_time = 1, control_threshold = 2)$control,
    c(0L, 0L)
  )
  expect_identical(
    control_prob(res, control_time = 1, control_threshold = 3),
    1
  )
  expect_identical(
    control_prob(res, control_time = 1, control_threshold = 2),
    0
  )
})

test_that("control_prob calculates the proportion of controlled runs", {
  res <- do.call(
    scenario_sim,
    c(no_transmission_args, list(sim = sim_opts(cap_max_days = 49)))
  )
  # 3 cases in week 1 of the first replicate, 1 case in the second
  res$outbreak_ts[sim == 2 & week == 1, weekly_cases := 1L]
  expect_identical(
    control_prob(res, control_time = 1, control_threshold = 2),
    0.5
  )
})

test_that("control_prob works with other aggregation intervals", {
  daily <- do.call(
    scenario_sim,
    c(
      no_transmission_args,
      list(sim = sim_opts(cap_max_days = 50, aggregation = "daily"))
    )
  )
  # initial cases have onset on day 10
  expect_message(
    expect_identical(
      control_prob(daily, control_time = 10, control_threshold = 2),
      0
    ),
    regexp = "cases <= 2 within days: 10 to 49"
  )
  expect_identical(
    control_prob(daily, control_time = 10, control_threshold = 3),
    1
  )

  # line list is aggregated to daily
  none <- do.call(
    scenario_sim,
    c(
      no_transmission_args,
      list(sim = sim_opts(cap_max_days = 50, aggregation = "none"))
    )
  )
  expect_message(
    expect_warning(
      expect_identical(
        control_prob(none, control_time = 10, control_threshold = 2),
        0
      ),
      regexp = "will be aggregated to daily cases"
    ),
    regexp = "cases <= 2 within days: 10 to 49"
  )
  expect_warning(
    expect_identical(
      control_prob(none, control_time = 10, control_threshold = 3),
      1
    ),
    regexp = "will be aggregated to daily cases"
  )

  # integer number of days, onset in period 2 (days 10 to 14)
  period <- do.call(
    scenario_sim,
    c(
      no_transmission_args,
      list(sim = sim_opts(cap_max_days = 50, aggregation = 5))
    )
  )
  expect_message(
    expect_identical(
      control_prob(period, control_time = 2, control_threshold = 2),
      0
    ),
    regexp = "cases <= 2 within 5-day periods: 2 to 9"
  )
  expect_identical(
    control_prob(period, control_time = 2, control_threshold = 3),
    1
  )
})

test_that("detect_control prints message when control_threshold > 0", {
  expect_message(
    detect_control(
      scenario = control_scenario,
      control_time = 5:10,
      control_threshold = 100
    ),
    regexp = "Calculating control as cases <= 100 within weeks: 5 to 10"
  )
})

test_that("detect_control errors when control_threshold > 0 and no time", {
  expect_error(
    detect_control(scenario = control_scenario, control_threshold = 100),
    regexp = "`control_threshold` > 0 but `control_time` is `NULL`"
  )
})

test_that("detect_control errors without extinct attribute", {
  # copy to not modify `control_scenario` by reference
  res <- data.table::copy(control_scenario)
  setattr(res$outbreak_ts, name = "extinct", value = NULL)
  expect_error(
    detect_control(scenario = res),
    regexp = "`control_time`/`extinction_time` not specified"
  )
  expect_false(is.null(attr(control_scenario$outbreak_ts, which = "extinct")))
})

test_that("detect_control errors without cap_cases attribute", {
  # copy to not modify `control_scenario` by reference
  res <- data.table::copy(control_scenario)
  setattr(res$outbreak_ts, name = "cap_cases", value = NULL)
  # errors before printing the calculation message
  expect_no_message(
    expect_error(
      detect_control(scenario = res, control_time = 5),
      regexp = "`scenario` is missing the `cap_cases` attribute"
    )
  )
})

test_that("detect_control errors with missing values in control_time", {
  expect_error(
    detect_control(scenario = control_scenario, control_time = NA),
    regexp = "control_time.*Contains missing values"
  )
  expect_error(
    detect_extinct(scenario = control_scenario, extinction_time = c(1, NA)),
    regexp = "control_time.*Contains missing values"
  )
})

test_that("detect_control errors when time is not in the outbreak data", {
  expect_error(
    detect_control(scenario = control_scenario, control_time = 100),
    regexp = "`control_time`/`extinction_time` not in simulated outbreak data"
  )
})

test_that("control and extinction functions error without ringbp class", {
  expect_error(
    detect_control(scenario = control_scenario$outbreak_ts),
    regexp = "scenario.*Must inherit from class 'ringbp'"
  )
  expect_error(
    control_prob(scenario = unclass(control_scenario)),
    regexp = "scenario.*Must inherit from class 'ringbp'"
  )
  expect_error(
    detect_extinct(scenario = control_scenario$outbreak_ts),
    regexp = "scenario.*Must inherit from class 'ringbp'"
  )
  expect_error(
    extinct_prob(scenario = list(outbreak_ts = control_scenario$outbreak_ts)),
    regexp = "scenario.*Must inherit from class 'ringbp'"
  )
})

test_that("detect_control errors with invalid control_threshold", {
  expect_error(
    detect_control(control_scenario, control_time = 5, control_threshold = -1),
    regexp = "control_threshold.*Element 1 is not >= 0"
  )
  expect_error(
    detect_control(control_scenario, control_time = 5, control_threshold = NA),
    regexp = "control_threshold.*Contains missing values"
  )
  expect_error(
    detect_control(
      control_scenario,
      control_time = 5,
      control_threshold = c(1, 2)
    ),
    regexp = "control_threshold.*Must have length 1"
  )
  expect_error(
    detect_control(control_scenario, control_time = 5, control_threshold = "1"),
    regexp = "control_threshold.*Must be of type 'integerish'"
  )
  expect_error(
    detect_control(control_scenario, control_time = 5, control_threshold = 1.5),
    regexp = "control_threshold.*Must be of type 'integerish'"
  )
})

test_that("detect_control returns integers with and without control_time", {
  res <- do.call(
    scenario_sim,
    c(no_transmission_args, list(sim = sim_opts(cap_max_days = 49)))
  )
  # initial cases have onset in week 1 so the outbreak is extinct from week 2
  expect_identical(
    detect_control(res),
    detect_control(res, control_time = 2)
  )
  expect_identical(
    detect_extinct(res),
    detect_extinct(res, extinction_time = 2)
  )
  expect_type(detect_control(res)$control, "integer")
  expect_type(
    detect_control(res, control_time = 1, control_threshold = 2)$control,
    "integer"
  )
})

test_that("control functions error for outbreak_model() output", {
  om <- do.call(
    outbreak_model,
    c(
      no_transmission_args[names(no_transmission_args) != "n"],
      list(sim = sim_opts(cap_max_days = 49))
    )
  )
  regexp <- "Names must include the elements \\{'sim'\\}"
  expect_error(detect_extinct(om), regexp = regexp)
  expect_error(extinct_prob(om), regexp = regexp)
  expect_error(detect_extinct(om, extinction_time = 1), regexp = regexp)
  expect_error(
    control_prob(om, control_time = 1, control_threshold = 1),
    regexp = regexp
  )
})
