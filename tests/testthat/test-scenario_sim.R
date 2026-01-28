test_that("scenario_sim runs as expected", {
  set.seed(1)
  n_sim <- 5
  res <- scenario_sim(
    n = n_sim,
    initial_cases = 5,
    offspring = offspring_opts(
      community = \(n) rnbinom(n = n, mu = 2.5, size = 0.16),
      isolated = \(n) rnbinom(n = n, mu = 0, size = 1),
      asymptomatic = \(n) rnbinom(n = n, mu = 2.5, size = 0.16)
    ),
    delays = delay_opts(
      incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
      onset_to_isolation = \(n) rweibull(n = n, shape = 2.5, scale = 5)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 0,
      presymptomatic_transmission = 0.3,
      symptomatic_ascertained = 0
    ),
    interventions = intervention_opts(quarantine = TRUE),
    sim = sim_opts(
      cap_max_days = 365,
      cap_cases = 2000
    )
  )
  expect_snapshot(print(res, topn = 100))
  expect_s3_class(res, class = c("data.table", "data.frame"), exact = TRUE)
  expect_identical(
    vapply(res, class, FUN.VALUE = character(1)),
    c(sim = "integer", week = "integer", weekly_cases = "integer",
      cumulative = "integer", effective_r0 = "numeric", cases_per_gen = "list")
  )
  expect_identical(unique(res$sim), 1:n_sim)
})

test_that("scenario_sim with dynamic seed and parameters runs as expected", {
  seed <- as.integer(Sys.Date())
  if (on_ci()) message("Seed: ", seed)
  set.seed(seed)
  n_sim <- 5
  res <- scenario_sim(
    n = n_sim,
    initial_cases = sample(1:50, size = 1),
    offspring = offspring_opts(
      community = \(n) rnbinom(
        n = n,
        mu = runif(n = 1, min = 0.5, max = 3),
        size = runif(n = 1, min = 0.1, max = 1000)
      ),
      isolated = \(n) rnbinom(
        n = n,
        mu = runif(n = 1, min = 0, max = 3),
        size = runif(n = 1, min = 0.1, max = 1000)
      ),
      asymptomatic = \(n) rnbinom(
        n = n,
        mu = runif(n = 1, min = 0.5, max = 3),
        size = runif(n = 1, min = 0.1, max = 1000)
      )
    ),
    delays = delay_opts(
      incubation_period = \(n) rweibull(
        n = n,
        shape = runif(n = 1, min = 2, max = 4),
        scale = runif(n = 1, min = 2, max = 6)
      ),
      onset_to_isolation = \(n) rweibull(
        n = n,
        shape = runif(n = 1, min = 2, max = 4),
        scale = runif(n = 1, min = 2, max = 6)
      )
    ),
    event_probs = event_prob_opts(
      asymptomatic = runif(n = 1, min = 0, max = 1),
      presymptomatic_transmission = runif(n = 1, min = 0, max = 1),
      symptomatic_ascertained = runif(n = 1, min = 0, max = 1)
    ),
    interventions = intervention_opts(
      quarantine = sample(c(TRUE, FALSE), size = 1)
    ),
    sim = sim_opts(
      cap_max_days = sample(100:1000, size = 1),
      cap_cases = sample(100:10000, size = 1)
    )
  )
  expect_s3_class(res, class = c("data.table", "data.frame"), exact = TRUE)
  expect_identical(
    vapply(res, class, FUN.VALUE = character(1)),
    c(sim = "integer", week = "integer", weekly_cases = "integer",
      cumulative = "integer", effective_r0 = "numeric", cases_per_gen = "list")
  )
  expect_identical(unique(res$sim), 1:n_sim)
})
