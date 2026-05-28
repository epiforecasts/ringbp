test_that("outbreak_model runs with no transmission", {
  res <- outbreak_model(
    initial_cases = 1,
    offspring = offspring_opts(
      community = \(n) rep(0, n),
      isolated = \(n) rep(0, n),
      asymptomatic = \(n) rep(0, n)
    ),
    delays = delay_opts(
      incubation_period = \(n) rep(1, n),
      onset_to_isolation = \(n) rep(1, n)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 0,
      presymptomatic_transmission = 0.1,
      symptomatic_traced = 1
    ),
    interventions = intervention_opts(quarantine = TRUE),
    sim = sim_opts()
  )
  expect_equal(res$week, 0:(sim_opts()$cap_max_days / 7))
  # one initial case
  expect_identical(res$weekly_cases[1], 1L)
  # no secondary cases
  expect_identical(unique(res$weekly_cases[-1]), 0L)
  expect_identical(unique(res$cumulative), 1L)
  expect_identical(unique(res$effective_r0), 0)
  expect_identical(unique(res$cases_per_gen), list(0))
})

test_that("outbreak_model runs to cap_max_days stopping criterion", {
  cap_max_days <- 70
  cap_cases <- 1e5
  res <- outbreak_model(
    initial_cases = 1,
    offspring = offspring_opts(
      community = \(n) rep(3, n),
      isolated = \(n) rep(1, n),
      asymptomatic = \(n) rep(3, n)
    ),
    delays = delay_opts(
      incubation_period = \(n) rep(6, n),
      onset_to_isolation = \(n) rep(6, n)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 0.1,
      presymptomatic_transmission = 0.1,
      symptomatic_traced = 0.5
    ),
    interventions = intervention_opts(quarantine = FALSE),
    sim = sim_opts(cap_max_days = cap_max_days, cap_cases = cap_cases)
  )
  expect_equal(res$week, 0:(cap_max_days / 7))
  # has not reached cap_cases stopping criterion
  expect_lt(res[.N, cumulative], cap_cases)
  # monotonic cumulative cases
  expect_identical(res$cumulative, sort(res$cumulative))
  # non-extinction of outbreak
  expect_true(all(res$weekly_cases > 0))
})

test_that("outbreak_model runs to cap_cases stopping criterion", {
  cap_cases <- 1000
  res <- outbreak_model(
    initial_cases = 1,
    offspring = offspring_opts(
      community = \(n) rep(3, n),
      isolated = \(n) rep(1, n),
      asymptomatic = \(n) rep(3, n)
    ),
    delays = delay_opts(
      incubation_period = \(n) rep(6, n),
      onset_to_isolation = \(n) rep(6, n)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 0.1,
      presymptomatic_transmission = 0.1,
      symptomatic_traced = 0.5
    ),
    interventions = intervention_opts(quarantine = FALSE),
    sim = sim_opts(cap_cases = cap_cases)
  )
  expect_equal(res$week, 0:(sim_opts()$cap_max_days / 7))
  # no cases in the last 10 weeks
  expect_identical(tail(res$weekly_cases, n = 10), rep(0L, 10))
  # cap_cases is soft upper bound so cumulative cases exceed cap_cases
  # cumulative cases has reached cap_cases stopping criterion
  expect_gt(res[.N, cumulative], cap_cases)
  # monotonic cumulative cases
  expect_identical(res$cumulative, sort(res$cumulative))
})

test_that("outbreak_model runs for 1 gen when onset > cap_max_days", {
  # regression test for #163: an initial case whose symptom onset exceeds
  # `cap_max_days` should not stop the simulation before any transmission
  expect_no_warning(
    res <- outbreak_model(
      initial_cases = 1,
      offspring = offspring_opts(
        community = \(n) rep(1, n),
        isolated = \(n) rep(1, n),
        asymptomatic = \(n) rep(1, n)
      ),
      delays = delay_opts(
        incubation_period = \(n) rep(20, n),
        onset_to_isolation = \(n) rep(1, n)
      ),
      event_probs = event_prob_opts(
        asymptomatic = 0,
        presymptomatic_transmission = 0.1,
        symptomatic_traced = 1
      ),
      interventions = intervention_opts(quarantine = TRUE),
      sim = sim_opts(cap_max_days = 5)
    ),
    message = "zero generations"
  )
  # the simulation ran at least one generation, so effective R0 and cases per
  # generation are populated rather than the zero-generation NA placeholders
  expect_false(is.nan(res$effective_r0))
  expect_false(identical(res$cases_per_gen, list(NA_real_)))
})

test_that("outbreak_model in-window counts are independent of cap_max_days", {
  # regression test for #225: for a fixed seed, weekly and cumulative case
  # counts for weeks fully inside the simulation window must not change with
  # `cap_max_days`
  outbreaks <- list()
  cap_max_days <- c(50, 100)
  for (i in seq_along(cap_max_days)) {
    set.seed(1)
    outbreaks[[i]] <- outbreak_model(
      initial_cases = 1,
      offspring = offspring_opts(
        community = \(n) rnbinom(n = n, mu = 3, size = 0.27),
        isolated = \(n) rep(0, n)
      ),
      delays = delay_opts(
        incubation_period = \(n) rgamma(n = n, shape = 4, scale = 2),
        onset_to_isolation = \(n) rgamma(n = n, shape = 2.5, scale = 2)
      ),
      event_probs = event_prob_opts(
        asymptomatic = 0.25,
        presymptomatic_transmission = 0.01,
        symptomatic_traced = 0
      ),
      interventions = intervention_opts(test_sensitivity = 0),
      sim = sim_opts(cap_max_days = cap_max_days[i], cap_cases = 1e4)
    )
  }
  # the larger cap runs the (growing) outbreak further: a non-trivial test
  expect_gt(outbreaks[[2]][.N, cumulative], outbreaks[[1]][.N, cumulative])

  # weeks 0-5 fall fully inside both simulation windows, so their cumulative
  # case counts must be identical
  expect_identical(
    outbreaks[[1]][week <= 5, cumulative],
    outbreaks[[2]][week <= 5, cumulative]
  )
})

test_that("outbreak_model warns if outbreak_step is not run (cap_cases)", {
  expect_warning(
    res <- outbreak_model(
      initial_cases = 25,
      offspring = offspring_opts(
        community = \(n) rep(1, n),
        isolated = \(n) rep(1, n),
        asymptomatic = \(n) rep(1, n)
      ),
      delays = delay_opts(
        incubation_period = \(n) rep(1, n),
        onset_to_isolation = \(n) rep(1, n)
      ),
      event_probs = event_prob_opts(
        asymptomatic = 0,
        presymptomatic_transmission = 0.1,
        symptomatic_traced = 1
      ),
      interventions = intervention_opts(quarantine = TRUE),
      sim = sim_opts(cap_cases = 10)
    ),
    regexp = "The outbreak simulation ran for zero generations"
  )
  expect_identical(res$week, 0:50)
  expect_identical(res$weekly_cases[1], 25L)
  expect_identical(unique(res$weekly_cases[-1]), 0L)
  expect_identical(unique(res$cumulative), 25L)
  expect_identical(unique(res$effective_r0), NaN)
  expect_identical(unique(res$cases_per_gen), list(NA_real_))
})

test_that("outbreak_model warns if latent period is positive & cases > 1", {
  expect_warning(
    outbreak_model(
      initial_cases = 1,
      offspring = offspring_opts(
        community = \(n) rep(2, n),
        isolated = \(n) rep(2, n),
        asymptomatic = \(n) rep(2, n)
      ),
      delays = suppressWarnings(
        delay_opts(
          incubation_period = \(n) rep(1, n),
          onset_to_isolation = \(n) rep(5, n),
          latent_period = 0.5
        )
      ),
      event_probs = event_prob_opts(
        asymptomatic = 0,
        presymptomatic_transmission = 0.1,
        symptomatic_traced = 1
      ),
      interventions = intervention_opts(quarantine = TRUE),
      sim = sim_opts()
    ),
    regexp = paste(
      "The proportion of presymptomatic transmission supplied is:",
      "The realised proportion of presymptomatic transmission is:",
      sep = ".*"
    )
  )
})
