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
    c(incubation_period = "function", onset_to_isolation = "function",
      latent_period = "numeric", onset_to_self_isolation = "function")
  )
})

test_that("event_prob_opts works as expected", {
  event_probs <- event_prob_opts(
    asymptomatic = 0.5,
    presymptomatic_transmission = 0.5,
    symptomatic_traced = 0.5
  )
  expect_s3_class(event_probs, "ringbp_event_prob_opts")
  expect_identical(
    vapply(event_probs, class, FUN.VALUE = character(1)),
    c(asymptomatic = "numeric", presymptomatic_transmission = "numeric",
      alpha = "numeric", symptomatic_traced = "function",
      symptomatic_self_isolate = "numeric")
  )
})

test_that("intervention_opts works as expected", {
  interventions <- intervention_opts(quarantine = FALSE)
  expect_s3_class(interventions, "ringbp_intervention_opts")
  expect_identical(
    vapply(interventions, class, FUN.VALUE = character(1)),
    c(quarantine = "logical", test_sensitivity = "function")
  )
})

test_that("sim_opts works as expected with defaults", {
  sim <- sim_opts()
  expect_s3_class(sim, "ringbp_sim_opts")
  expect_identical(
    vapply(sim, class, FUN.VALUE = character(1)),
    c(
      cap_max_days = "numeric", cap_cases = "numeric",
      aggregation = "character"
    )
  )
  expect_identical(sim$cap_max_days, 350)
})

test_that("sim_opts works as expected with arguments", {
  sim <- sim_opts(cap_max_days = 700, cap_cases = 1000)
  expect_s3_class(sim, "ringbp_sim_opts")
  expect_identical(
    vapply(sim, class, FUN.VALUE = character(1)),
    c(
      cap_max_days = "numeric", cap_cases = "numeric",
      aggregation = "character"
    )
  )
})

test_that("sim_opts aggregation default is weekly", {
  expect_identical(sim_opts()$aggregation, "weekly")
})

test_that("sim_opts aggregation accepts and standardises aliases", {
  for (x in c("none", "NONE")) {
    expect_identical(sim_opts(aggregation = x)$aggregation, "none")
  }
  for (x in c("day", "days", "daily", "Daily")) {
    expect_identical(sim_opts(aggregation = x)$aggregation, "daily")
  }
  for (x in c("week", "weeks", "weekly", "WEEKS")) {
    expect_identical(sim_opts(aggregation = x)$aggregation, "weekly")
  }
  for (x in c("month", "months", "monthly", "mOnTh")) {
    expect_identical(
      sim_opts(cap_max_days = 360, aggregation = x)$aggregation, "monthly"
    )
  }
})

test_that("sim_opts aggregation accepts an integer number of days", {
  expect_identical(sim_opts(aggregation = 14)$aggregation, 14L)
  expect_identical(sim_opts(aggregation = 1L)$aggregation, 1L)
})

test_that("sim_opts errors if cap_max_days is not whole intervals", {
  expect_error(
    sim_opts(cap_max_days = 350, aggregation = "monthly"),
    regexp = "`cap_max_days` \\(350\\) is not a whole number of `monthly`"
  )
  expect_error(
    sim_opts(cap_max_days = 350, aggregation = "monthly"),
    regexp = "for example 330 or 360"
  )
  expect_error(
    sim_opts(cap_max_days = 20, aggregation = "monthly"),
    regexp = "for example 30\\."
  )
  expect_error(
    sim_opts(cap_max_days = 50, aggregation = "weekly"),
    regexp = "multiple of 7"
  )
  expect_no_error(sim_opts(cap_max_days = 360, aggregation = "monthly"))
  expect_no_error(sim_opts(cap_max_days = 50, aggregation = "none"))
})

test_that("sim_opts aggregation errors with invalid input", {
  expect_error(
    sim_opts(aggregation = "fortnightly"),
    regexp = "Must be element"
  )
  expect_error(sim_opts(aggregation = 0), regexp = "Element 1 is not >= 1")
  expect_error(sim_opts(aggregation = 1.5), regexp = "integerish")
  expect_error(
    sim_opts(aggregation = c("daily", "weekly")),
    regexp = "atomic scalar"
  )
  expect_error(sim_opts(aggregation = NA), regexp = "Must be element")
})
