set.seed(20200410)

test_that("A basic sim setup returns the correct object", {
  # generate initial cases
  case_data <- outbreak_setup(
    initial_cases = 5,
    delays = delay_opts(
      incubation_period = \(n) stats::rweibull(n = n, shape = 2.32, scale = 6.49),
      onset_to_isolation = \(n) stats::rweibull(n = n, shape = 2, scale = 4)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 0,
      presymptomatic_transmission = 0.5,
      symptomatic_ascertained = 0
    )
  )

  expect_equal(nrow(case_data), 5)
  expect_true(all(case_data$missed))
  expect_true(all(!case_data$asymptomatic))
})

test_that("asymptomatic arg works properly", {
  # generate initial cases
  # All asymptomatics
  all_asymptomatic <- outbreak_setup(
    initial_cases = 5,
    delays = delay_opts(
      incubation_period = \(n) stats::rweibull(n = n, shape = 2.32, scale = 6.49),
      onset_to_isolation = \(n) stats::rweibull(n = n, shape = 2, scale = 4)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 1,
      presymptomatic_transmission = 0.5,
      symptomatic_ascertained = 0
    )
  )
  expect_true(all(all_asymptomatic$asymptomatic))

  # Mixed asympt dbinom(0, 10000, 0.5) = 0
  # With 10000 cases, probability of 0 symptomatic or 0 asympt is less than
  # machine precision
  mix <- outbreak_setup(
    initial_cases = 10000,
    delays = delay_opts(
      incubation_period = \(n) stats::rweibull(n = n, shape = 2.32, scale = 6.49),
      onset_to_isolation = \(n) stats::rweibull(n = n, shape = 2, scale = 4)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 0.5,
      presymptomatic_transmission = 0.5,
      symptomatic_ascertained = 0
    )
  )

  expect_length(unique(mix$asymptomatic), 2)
})
