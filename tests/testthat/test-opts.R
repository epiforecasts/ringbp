test_that("offspring_opts works as expected", {
  offspring <- offspring_opts(
    community = \(n) rnbinom(n = n, mu = 2, size = 0.5),
    isolated = \(n) rnbinom(n = n, mu = 2, size = 0.5),
    asymptomatic = \(n) rnbinom(n = n, mu = 2, size = 0.5)
  )
  expect_s3_class(offspring, "ringbp_offspring_opts")
  expect_identical(
    vapply(offspring, class, FUN.VALUE = character(1)),
    c(community = "function", isolated = "function", asymptomatic = "function")
  )
})

test_that("offspring_opts works as expected with default", {
  offspring <- offspring_opts(
    community = \(n) rnbinom(n = n, mu = 2, size = 0.5),
    isolated = \(n) rnbinom(n = n, mu = 2, size = 0.5)
  )
  expect_s3_class(offspring, "ringbp_offspring_opts")
  expect_identical(
    vapply(offspring, class, FUN.VALUE = character(1)),
    c(community = "function", isolated = "function", asymptomatic = "function")
  )
})

test_that("delay_opts works as expected", {
  delays <- delay_opts(
    incubation_period = \(n) rweibull(n = n, shape = 2, scale = 2),
    onset_to_isolation = \(n) rweibull(n = n, shape = 2, scale = 2)
  )
  expect_s3_class(delays, "ringbp_delay_opts")
  expect_identical(
    vapply(delays, class, FUN.VALUE = character(1)),
    c(incubation_period = "function", onset_to_isolation = "function")
  )
})

test_that("event_prob_opts works as expectd", {
  event_probs <- event_prob_opts(
    asymptomatic = 0.5,
    presymptomatic_transmission = 0.5,
    symptomatic_ascertained = 0.5
  )
  expect_s3_class(event_probs, "ringbp_event_prob_opts")
  expect_identical(
    vapply(event_probs, class, FUN.VALUE = character(1)),
    c(asymptomatic = "numeric", alpha = "numeric",
      symptomatic_ascertained = "numeric")
  )
})

test_that("intervention_opts works as expected", {
  interventions <- intervention_opts(quarantine = FALSE)
  expect_s3_class(interventions, "ringbp_intervention_opts")
  expect_identical(
    vapply(interventions, class, FUN.VALUE = character(1)),
    c(quarantine = "logical")
  )
})

test_that("sim_opts works as expected with defaults", {
  sim <- sim_opts()
  expect_s3_class(sim, "ringbp_sim_opts")
  expect_identical(
    vapply(sim, class, FUN.VALUE = character(1)),
    c(cap_max_days = "numeric", cap_cases = "numeric")
  )
})

test_that("sim_opts works as expected with arguments", {
  sim <- sim_opts(cap_max_days = 1000, cap_cases = 1000)
  expect_s3_class(sim, "ringbp_sim_opts")
  expect_identical(
    vapply(sim, class, FUN.VALUE = character(1)),
    c(cap_max_days = "numeric", cap_cases = "numeric")
  )
})
