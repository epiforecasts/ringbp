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
    c(sim = "integer", week = "numeric", weekly_cases = "numeric",
      cumulative = "numeric", effective_r0 = "numeric", cases_per_gen = "list")
  )
  expect_identical(unique(res$sim), 1:n_sim)
})
