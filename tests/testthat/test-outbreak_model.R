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
      symptomatic_ascertained = 1
    ),
    interventions = intervention_opts(quarantine = TRUE),
    sim = sim_opts()
  )
  expect_equal(res$week, 0:(sim_opts()$cap_max_days / 7))
  # one initial case
  expect_identical(res$weekly_cases[1], 1)
  # no secondary cases
  expect_identical(unique(res$weekly_cases[-1]), 0)
  expect_identical(unique(res$cumulative), 1)
  expect_identical(unique(res$effective_r0), 0)
  expect_identical(unique(res$cases_per_gen), list(0))
})

test_that("outbreak_model runs to cap_max_days stopping criterion", {
  cap_max_days <- 70
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
      symptomatic_ascertained = 0.5
    ),
    interventions = intervention_opts(quarantine = FALSE),
    sim = sim_opts(cap_max_days = cap_max_days)
  )
  expect_equal(res$week, 0:(cap_max_days / 7))
  # has not reached cap_cases stopping criterion
  expect_lt(res[.N, cumulative], sim_opts()$cap_cases)
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
      symptomatic_ascertained = 0.5
    ),
    interventions = intervention_opts(quarantine = FALSE),
    sim = sim_opts(cap_cases = cap_cases)
  )
  expect_equal(res$week, 0:(sim_opts()$cap_max_days / 7))
  # no cases in the last 10 weeks
  expect_identical(tail(res$weekly_cases, n = 10), rep(0, 10))
  # cap_cases is soft upper bound so cumulative cases exceed cap_cases
  # cumulative cases has reached cap_cases stopping criterion
  expect_gt(res[.N, cumulative], cap_cases)
  # monotonic cumulative cases
  expect_identical(res$cumulative, sort(res$cumulative))
})
