context("Test basic usage")

set.seed(20200410)
res <- ringbp::scenario_sim(
  n = 2, initial_cases = 10, prop_asymptomatic = 0, prop_ascertain = 0.2,
  cap_cases = 20, cap_max_days = 100, r0isolated = 0, r0community = 2.5,
  disp_community = 0.16, disp_isolated = 1,
  onset_to_isolation = \(x) stats::rweibull(n = x, shape = 1.651524, scale = 4.287786),
  incubation_period = \(x) stats::rweibull(n = x, shape = 2.322737, scale = 6.492272),
  k = 0, quarantine = FALSE)

test_that("A basic sim returns the correct object", {

  # Check we got 2 simulations as requested
  expect_true(length(unique(res$sim)) == 2)

  expect_true(inherits(res, 'data.frame'))
  expect_true(inherits(res, 'data.table'))

  # Test that weeks increase.
  # As we have n = 2, exactly 1 sim should be smaller the the previous week
  # Same for cumulative cases.
  expect_equal(
    sum((res$week[seq(2,nrow(res))] - res$week[seq(nrow(res) - 1)]) < 0), 1
  )
  expect_equal(
    sum((res$cumulative[seq(2,nrow(res))] -
           res$cumulative[seq(nrow(res) - 1)]) < 0), 1
  )
})
