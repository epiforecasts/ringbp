context("Test basic usage")

set.seed(20200410)

test_that("A basic sim setup returns the correct object", {
  # generate initial cases
  case_data <- outbreak_setup(
    parameters = parameters(
      initial_cases = 5,
      r0_community = 1,
      r0_isolated = 1,
      disp_community = 1,
      disp_isolated = 1,
      incubation_period = \(x) stats::rweibull(n = x, shape = 2.32, scale = 6.49),
      prop_presymptomatic = 0.5,
      onset_to_isolation = \(x) stats::rweibull(n = x, shape = 2, scale = 4),
      prop_ascertain = 0,
      prop_asymptomatic = 0
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
    parameters = parameters(
      initial_cases = 5,
      r0_community = 1,
      r0_isolated = 1,
      disp_community = 1,
      disp_isolated = 1,
      incubation_period = \(x) stats::rweibull(n = x, shape = 2.32, scale = 6.49),
      prop_presymptomatic = 0.5,
      onset_to_isolation = \(x) stats::rweibull(n = x, shape = 2, scale = 4),
      prop_ascertain = 0,
      prop_asymptomatic = 1
    )


  )
  expect_true(all(all_asymptomatic$asymptomatic))

  # Mixed asympt dbinom(0, 10000, 0.5) = 0
  # With 10000 cases, probability of 0 symptomatic or 0 asympt is less than
  # machine precision
  mix <- outbreak_setup(
    parameters = parameters(
      initial_cases = 10000,
      r0_community = 1,
      r0_isolated = 1,
      disp_community = 1,
      disp_isolated = 1,
      incubation_period = \(x) stats::rweibull(n = x, shape = 2.32, scale = 6.49),
      prop_presymptomatic = 0.5,
      onset_to_isolation = \(x) stats::rweibull(n = x, shape = 2, scale = 4),
      prop_ascertain = 0,
      prop_asymptomatic = 0.5
    )
  )

  expect_length(unique(mix$asymptomatic), 2)
})
