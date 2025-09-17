set.seed(20200410)
res <- ringbp::scenario_sim(
  n = 2,
  initial_cases = 10,
  offspring = offspring_opts(
    community = \(n) rnbinom(n = n, mu = 2.5, size = 0.16),
    isolated = \(n) rnbinom(n = n, mu = 0, size = 1)
  ),
  delays = delay_opts(
    incubation_period = \(n) stats::rweibull(n = n, shape = 2.32, scale = 6.49),
    onset_to_isolation = \(n) stats::rweibull(n = n, shape = 1.65, scale = 4.28)
  ),
  event_probs = event_prob_opts(
    asymptomatic = 0,
    presymptomatic_transmission = 0.5,
    symptomatic_ascertained = 0.2
  ),
  interventions = intervention_opts(quarantine = FALSE),
  sim = sim_opts(cap_max_days = 100, cap_cases = 20)
)

test_that("A basic sim returns the correct object", {

  # Check we got 2 simulations as requested
  expect_true(length(unique(res$sim)) == 2)

  expect_true(inherits(res, "data.frame"))
  expect_true(inherits(res, "data.table"))

  # Test that weeks increase.
  # As we have n = 2, exactly 1 sim should be smaller the the previous week
  # Same for cumulative cases.
  expect_equal(
    sum((res$week[seq(2, nrow(res))] - res$week[seq(nrow(res) - 1)]) < 0), 1
  )
  expect_equal(
    sum((res$cumulative[seq(2, nrow(res))] -
           res$cumulative[seq(nrow(res) - 1)]) < 0), 1
  )
})
