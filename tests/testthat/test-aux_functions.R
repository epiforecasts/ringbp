set.seed(515)

test_that("incubation_to_generation_time works as expected", {
  symptom_onset_time <- 1:5
  res <- incubation_to_generation_time(
    symptom_onset_time = symptom_onset_time,
    alpha = 2
  )
  expect_length(res, length(symptom_onset_time))
  expect_type(res, type = "double")
  expect_false(anyNA(res))
  # default latent period (minimum generation time) is 0
  expect_gte(min(res), 0)
})

test_that("incubation_to_generation_time works with latent_period > 0", {
  symptom_onset_time <- 1:5
  latent_period <- 3
  res <- incubation_to_generation_time(
    symptom_onset_time = symptom_onset_time,
    alpha = 2,
    latent_period = latent_period
  )
  expect_length(res, length(symptom_onset_time))
  expect_type(res, type = "double")
  expect_false(anyNA(res))
  # default latent period (minimum generation time) is 0
  expect_gte(min(res), latent_period)
})

test_that("incubation_to_generation_time errors with latent period", {
  expect_error(
    incubation_to_generation_time(
      symptom_onset_time = 5,
      alpha = -5, # ~94% presymptomatic
      latent_period = 10
    ), regexp = "(Cannot sample generation time)*(incubation)*(latent)"
  )
})

test_that("incubation_to_generation_time alpha works as expected", {
  # positive is right skew so mean is greater than median
  res <- incubation_to_generation_time(rep(100, 1e5), alpha = 10)
  expect_gt(mean(res), median(res))

  # negative is left skew so mean is less than median
  res <- incubation_to_generation_time(rep(100, 1e5), alpha = -10)
  expect_lt(mean(res), median(res))

  # zero is symmetrical (normal) so mean and median are approximately equal
  res <- incubation_to_generation_time(rep(100, 1e5), alpha = 0)
  expect_equal(mean(res), median(res), tolerance = 0.01)
})

test_that("presymptomatic_transmission_to_alpha and incubation_to_generation_time", {
  # ~50% presymptomatic
  incubation_period <- 5
  prop_presymptomatic <- 0.5
  exposure <- incubation_to_generation_time(
    symptom_onset_time = rep(incubation_period, 1e5),
    alpha = presymptomatic_transmission_to_alpha(prop_presymptomatic)
  )
  expect_equal(
    sum(exposure < incubation_period) / length(exposure),
    expected = prop_presymptomatic,
    tolerance = 0.01
  )

  # ~10% presymptomatic
  prop_presymptomatic <- 0.1
  exposure <- incubation_to_generation_time(
    symptom_onset_time = rep(incubation_period, 1e5),
    alpha = presymptomatic_transmission_to_alpha(prop_presymptomatic)
  )
  expect_equal(
    sum(exposure < incubation_period) / length(exposure),
    expected = prop_presymptomatic,
    tolerance = 0.01
  )
})

test_that("presymptomatic_transmission_to_alpha errors from non-convergence", {
  expect_error(
    presymptomatic_transmission_to_alpha(presymptomatic_transmission = 1.1),
    regexp = paste(
      "(Estimating)*(alpha)*(from)*(presymptomatic_transmission)",
      "(did not converge)",
      sep = "*"
    )
  )
})

test_that("extinct_prob works as expected", {
  cap <- 100
  sims <- 5
  res <- scenario_sim(
    n = sims,
    initial_cases = 5,
    offspring = offspring_opts(
      community = \(n) rnbinom(n = n, mu = 2.5, size = 0.16),
      isolated = \(n) rnbinom(n = n, mu = 0, size = 1)
    ),
    delays = delay_opts(
      incubation_period = \(n) stats::rweibull(n = n, shape = 2.32, scale = 6.49),
      onset_to_isolation = \(n) stats::rweibull(n = n, shape = 2.5, scale = 5)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 0,
      presymptomatic_transmission = 0.3,
      symptomatic_ascertained = 0
    ),
    interventions = intervention_opts(),
    sim = sim_opts(cap_max_days = 100, cap_cases = cap)
  )

  r1 <- extinct_prob(res, cap)
  expect_true(r1 <= 1)
  expect_true(r1 >= 0)
  expect_length(r1, 1)

  is_wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
    abs(x - round(x)) < tol
  }
  expect_true(is_wholenumber(r1 * sims))

  # Manually build an output with known proportion of extinctions
  res2 <- res[c(1, 2, 1, 2), ]
  res2$sim <- c(1, 1, 2, 2)
  # Enforce that second sim did not have cases in final week.
  # Ignoring cases_per_gen becayse I think this function works on weekly cases.
  res2$weekly_cases[4] <- 0
  res2$cumulative[4] <- res2$cumulative[3]

  # Enforce that first sim has cases both weeks.
  # Ignoring cases_per_gen becayse I think this function works on weekly cases.
  res2$weekly_cases[1:2] <- c(1, 1)
  res2$cumulative[1:2] <- c(1, 2)

  r2 <- extinct_prob(res2, cap_cases = cap, week_range = 1)
  expect_equal(r2, 0.5)

  # Run some sims with almost certain outputs
  # Very high r0, shouldn't ever go extinct.
  res3 <- scenario_sim(
    n = sims,
    initial_cases = 5,
    offspring = offspring_opts(
      community = \(n) rnbinom(n = n, mu = 100, size = 0.16),
      isolated = \(n) rnbinom(n = n, mu = 100, size = 1)
    ),
    delays = delay_opts(
      incubation_period = \(n) stats::rweibull(n = n, shape = 2.32, scale = 6.49),
      onset_to_isolation = \(n) stats::rweibull(n = n, shape = 2.5, scale = 5)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 0,
      presymptomatic_transmission = 0.3,
      symptomatic_ascertained = 0
    ),
    interventions = intervention_opts(),
    sim = sim_opts(cap_max_days = 100, cap_cases = cap)
  )

  r3 <- extinct_prob(res3, cap)
  expect_equal(r3, 0)

  # r0 of 0, should always go extinct.
  res3 <- scenario_sim(
    n = sims,
    initial_cases = 5,
    offspring = offspring_opts(
      community = \(n) rnbinom(n = n, mu = 0, size = 0.16),
      isolated = \(n) rnbinom(n = n, mu = 0, size = 1)
    ),
    delays = delay_opts(
      incubation_period = \(n) stats::rweibull(n = n, shape = 2.32, scale = 6.49),
      onset_to_isolation = \(n) stats::rweibull(n = n, shape = 2.5, scale = 5)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 0,
      presymptomatic_transmission = 0.3,
      symptomatic_ascertained = 0
    ),
    interventions = intervention_opts(),
    sim = sim_opts(cap_max_days = 100, cap_cases = cap)
  )

  r3 <- extinct_prob(res3, cap)
  expect_equal(r3, 1)
})

test_that("extinct_prob week_range argument works", {
  cap <- 100
  sims <- 2
  res <- scenario_sim(
    n = 2,
    initial_cases = 5,
    offspring = offspring_opts(
      community = \(n) rnbinom(n = n, mu = 2.5, size = 0.16),
      isolated = \(n) rnbinom(n = n, mu = 0, size = 1)
    ),
    delays = delay_opts(
      incubation_period = \(n) stats::rweibull(n = n, shape = 2.32, scale = 6.49),
      onset_to_isolation = \(n) stats::rweibull(n = n, shape = 2.5, scale = 5)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 0,
      presymptomatic_transmission = 0.3,
      symptomatic_ascertained = 0
    ),
    interventions = intervention_opts(),
    sim = sim_opts(cap_max_days = 100, cap_cases = cap)
  )

  # Manually build an output with known proportion of extinctions
  res2 <- res[c(1, 2, 1, 2), ]
  res2$sim <- c(1, 1, 2, 2)

  # Enforce that second sim did not have cases in final week.
  # Ignoring cases_per_gen becayse I think this function works on weekly cases.
  res2$weekly_cases[4] <- 0
  res2$cumulative[4] <- res2$cumulative[3]

  # Enforce that first sim has cases both weeks.
  # Ignoring cases_per_gen becayse I think this function works on weekly cases.
  res2$weekly_cases[1:2] <- c(1, 1)
  res2$cumulative[1:2] <- c(1, 2)

  r2 <- extinct_prob(res2, cap_cases = cap, week_range = 1)
  expect_equal(r2, 0.5)

  # Now add a week and test week_range = 1:2
  # Simple case of cases in week 1 and 2
  res3 <- res[c(1, 2, 3), ]
  res3$weekly_cases[1:3] <- c(1, 1, 1)
  res3$cumulative[1:3] <- c(1, 2, 3)
  r3 <- extinct_prob(res3, cap_cases = cap, week_range = 1:2)
  expect_equal(r3, 0)

  # Simple case of no cases in week 1 or 2
  res4 <- res[c(1, 2, 3), ]
  res4$weekly_cases[1:3] <- c(1, 0, 0)
  res4$cumulative[1:3] <- c(1, 1, 1)
  r4 <- extinct_prob(res4, cap_cases = cap, week_range = 1:2)
  expect_equal(r4, 1)

  # Case of cases in week 1 but not 2 (by the definition used in this function
  # this is not an extinction). Test here that week_range 1:2 says no extintion
  # but week_range 2 says extinction.
  res5 <- res[c(1, 2, 3), ]
  res5$weekly_cases[1:3] <- c(1, 1, 0)
  res5$cumulative[1:3] <- c(1, 2, 2)
  r5 <- extinct_prob(res5, cap_cases = cap, week_range = 1:2)
  expect_equal(r5, 0)

  r5b <- extinct_prob(res5, cap_cases = cap, week_range = 2)
  expect_equal(r5b, 1)

  # Case of cases in week 2 but not 1 (by all sensible definitions is not an
  # extinction). Test here that week_range 1:2 says no extinction and neither
  # does week_range 2.
  res6 <- res[c(1, 2, 3), ]
  res6$weekly_cases[1:3] <- c(1, 0, 1)
  res6$cumulative[1:3] <- c(1, 1, 2)
  r6 <- extinct_prob(res6, cap_cases = cap, week_range = 1:2)
  expect_equal(r6, 0)

  r6b <- extinct_prob(res6, cap_cases = cap, week_range = 2)
  expect_equal(r6b, 0)
})

test_that("detect_extinct works", {
  cap <- 100
  sims <- 2
  res <- scenario_sim(
    n = 2,
    initial_cases = 5,
    offspring = offspring_opts(
      community = \(n) rnbinom(n = n, mu = 2.5, size = 0.16),
      isolated = \(n) rnbinom(n = n, mu = 0, size = 1)
    ),
    delays = delay_opts(
      incubation_period = \(n) stats::rweibull(n = n, shape = 2.32, scale = 6.49),
      onset_to_isolation = \(n) stats::rweibull(n = n, shape = 2.5, scale = 5)
    ),
    event_probs = event_prob_opts(
      asymptomatic = 0,
      presymptomatic_transmission = 0.3,
      symptomatic_ascertained = 0
    ),
    interventions = intervention_opts(),
    sim = sim_opts(cap_max_days = 100, cap_cases = cap)
  )

  # Manually build an output with known proportion of extinctions
  res2 <- res[c(1, 2, 1, 2), ]
  res2$sim <- c(1, 1, 2, 2)
  # Enforce that second sim did not have cases in final week.
  # Ignoring cases_per_gen becayse I think this function works on weekly cases.
  res2$weekly_cases[4] <- 0
  res2$cumulative[4] <- res2$cumulative[3]
  # Enforce that first sim has cases both weeks.
  # Ignoring cases_per_gen becayse I think this function works on weekly cases.
  res2$weekly_cases[1:2] <- c(1, 1)
  res2$cumulative[1:2] <- c(1, 2)

  r2 <- detect_extinct(res2, cap_cases = cap, week_range = 1)
  # The types in the output is a bit random. So just force all to doubles.
  r2 <- data.table(r2)[, lapply(.SD, as.double)]

  expect2 <- data.table(sim = c(1.0, 2.0), extinct = c(0.0, 1.0))
  expect_equal(r2, expect2)

  # Now add a week and test week_range = 1:2
  # Simple case of cases in week 1 and 2
  res3 <- res[c(1, 2, 3), ]
  res3$weekly_cases[1:3] <- c(1, 1, 1)
  res3$cumulative[1:3] <- c(1, 2, 3)
  r3 <- detect_extinct(res3, cap_cases = cap, week_range = 1:2)
  # The types in the output is a bit random. So just force all to doubles.
  r3 <- data.table(r3)[, lapply(.SD, as.double)]

  expect3 <- data.table(sim = c(1.0), extinct = c(0.0))
  expect_equal(r3, expect3)

  # Simple case of no cases in week 1 or 2
  res4 <- res[c(1, 2, 3), ]
  res4$weekly_cases[1:3] <- c(1, 0, 0)
  res4$cumulative[1:3] <- c(1, 1, 1)
  r4 <- detect_extinct(res4, cap_cases = cap, week_range = 1:2)
  # The types in the output is a bit random. So just force all to doubles.
  r4 <- data.table(r4)[, lapply(.SD, as.double)]

  expect4 <- data.table(sim = c(1.0), extinct = c(1.0))
  expect_equal(r4, expect4)

  # Case of cases in week 1 but not 2 (by the definition used in this function
  # this is not an extinction). Test here that week_range 1:2 says extintion by
  # week_range 2 does not.
  res5 <- res[c(1, 2, 3), ]
  res5$weekly_cases[1:3] <- c(1, 1, 0)
  res5$cumulative[1:3] <- c(1, 2, 2)
  r5 <- detect_extinct(res5, cap_cases = cap, week_range = 1:2)
  # The types in the output is a bit random. So just force all to doubles.
  r5 <- data.table(r5)[, lapply(.SD, as.double)]

  expect5 <- data.table(sim = c(1.0), extinct = c(0.0))
  expect_equal(r5, expect5)

  r5b <- detect_extinct(res5, cap_cases = cap, week_range = 2)
  # The types in the output is a bit random. So just force all to doubles.
  r5b <- data.table(r5b)[, lapply(.SD, as.double)]

  expect5b <- data.table(sim = c(1.0), extinct = c(1.0))
  expect_equal(r5b, expect5b)

  # Case of cases in week 2 but not 1 (by all sensible definitions is not an
  # extinction). Test here that week_range 1:2 says extintion and week_range 2
  # does as well.
  res6 <- res[c(1, 2, 3), ]
  res6$weekly_cases[1:3] <- c(1, 0, 1)
  res6$cumulative[1:3] <- c(1, 1, 2)
  r6 <- detect_extinct(res5, cap_cases = cap, week_range = 1:2)
  # The types in the output is a bit random. So just force all to doubles.
  r6 <- data.table(r6)[, lapply(.SD, as.double)]

  expect6 <- data.table(sim = c(1.0), extinct = c(0.0))
  expect_equal(r6, expect5)

  r6b <- detect_extinct(res5, cap_cases = cap, week_range = 2)

  r6b <- data.table(r6b)[, lapply(.SD, as.double)]

  expect6b <- data.table(sim = c(1.0), extinct = c(0.0))
  expect_equal(r6b, expect5b)
})
