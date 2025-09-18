set.seed(1234567)

test_that("A basic sim returns the correct object", {
  initial_cases <- 1
  offspring <- offspring_opts(
    # almost guarentees to get new cases
    community = \(n) rnbinom(n = n, mu = 500, size = 0.16),
    isolated = \(n) rnbinom(n = n, mu = 0, size = 1),
    asymptomatic = \(n) rnbinom(n = n, mu = 500, size = 0.16)
  )
  delays <- delay_opts(
    incubation_period = \(n) stats::rweibull(n = n, shape = 2.32, scale = 6.49),
    onset_to_isolation = \(n) stats::rweibull(n = n, shape = 2, scale = 4)
  )
  event_probs <- event_prob_opts(
    asymptomatic = 0,
    presymptomatic_transmission = 0.15,
    symptomatic_ascertained = 0
  )
  interventions <- intervention_opts(quarantine = FALSE)

  # generate initial cases
  case_data <- outbreak_setup(
    initial_cases = initial_cases,
    delays = delays,
    event_probs = event_probs
  )

  # generate next generation of cases
  case_data2 <- outbreak_step(
    case_data = case_data,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions
  )

  expect_gt(nrow(case_data2$cases), 1)
  expect_equal(
    as.vector(table(case_data2$cases$infector)),
    c(1, nrow(case_data2$cases) - 1)
  )

  # almost guarentees to get new cases
  offspring$community <- \(n) rnbinom(n = n, mu = 0, size = 0.16)
  offspring$asymptomatic <- \(n) rnbinom(n = n, mu = 0, size = 0.16)

  # With R0 = 0 we should get no additional cases.
  case_data3 <- outbreak_step(
    case_data = case_data,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions
  )

  expect_equal(nrow(case_data3$cases), 1)
})

test_that("Sim with multiple infectors makes senes", {
  initial_cases <- 2
  offspring <- offspring_opts(
    # almost guarentees both index cases create infections
    community = \(n) rnbinom(n = n, mu = 10000, size = 0.16),
    isolated = \(n) rnbinom(n = n, mu = 0, size = 1),
    asymptomatic = \(n) rnbinom(n = n, mu = 10000, size = 0.16)
  )
  delays <- delay_opts(
    incubation_period = \(n) stats::rweibull(n = n, shape = 2.32, scale = 6.49),
    onset_to_isolation = \(n) stats::rweibull(n = n, shape = 2, scale = 4)
  )
  event_probs <- event_prob_opts(
    asymptomatic = 0,
    presymptomatic_transmission = 0.15,
    symptomatic_ascertained = 0
  )
  interventions <- intervention_opts(quarantine = FALSE)

  # generate initial cases
  case_data <- outbreak_setup(
    initial_cases = initial_cases,
    delays = delays,
    event_probs = event_probs
  )

  # generate next generation of cases
  case_data2 <- outbreak_step(
    case_data = case_data,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions
  )

  expect_gt(nrow(case_data2$cases), 1)
  expect_equal(as.vector(table(case_data2$cases$infector))[1], 2)
  expect_true(all(as.vector(table(case_data2$cases$infector))[2:3] > 1))
})


test_that("r0_isolated is working properly", {
  initial_cases <- 1
  offspring <- offspring_opts(
    # Case is isolated so irrelevent
    community = \(n) rnbinom(n = n, mu = 500, size = 0.16),
    # Shoiuld get zero cases
    isolated = \(n) rnbinom(n = n, mu = 0, size = 1),
    asymptomatic = \(n) rnbinom(n = n, mu = 500, size = 0.16)
  )
  delays <- delay_opts(
    incubation_period = \(n) stats::rweibull(n = n, shape = 2.32, scale = 6.49),
    onset_to_isolation = \(n) stats::rweibull(n = n, shape = 2, scale = 4)
  )
  event_probs <- event_prob_opts(
    asymptomatic = 0,
    presymptomatic_transmission = 0.15,
    symptomatic_ascertained = 0
  )
  interventions <- intervention_opts(quarantine = FALSE)

  # generate initial cases
  case_data <- outbreak_setup(
    initial_cases = initial_cases,
    delays = delays,
    event_probs = event_probs
  )

  case_data$isolated <- TRUE

  # generate next generation of cases
  case_data2 <- outbreak_step(
    case_data = case_data,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions
  )

  expect_equal(nrow(case_data2$cases), 1)

  # Case is isolated so irrelevent
  offspring$community <- \(n) rnbinom(n = n, mu = 0, size = 0.16)
  # Shoiuld get lots of cases
  offspring$isolated <- \(n) rnbinom(n = n, mu = 500, size = 1)
  offspring$asymptomatic <- \(n) rnbinom(n = n, mu = 0, size = 0.16)

  # generate next generation of cases
  case_data3 <- outbreak_step(
    case_data = case_data,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions
  )

  expect_gt(nrow(case_data3$cases), 1)
})

test_that("Test a bunch of args", {
  initial_cases <- 1
  offspring <- offspring_opts(
    # almost guarentees both index cases create infections
    community = \(n) rnbinom(n = n, mu = 10000, size = 0.16),
    isolated = \(n) rnbinom(n = n, mu = 0, size = 1),
    asymptomatic = \(n) rnbinom(n = n, mu = 100000, size = 0.16)
  )
  delays <- delay_opts(
    incubation_period = \(n) stats::rweibull(n = n, shape = 2.32, scale = 6.49),
    onset_to_isolation = \(n) stats::rweibull(n = n, shape = 2, scale = 4)
  )
  event_probs <- event_prob_opts(
    asymptomatic = 0,
    presymptomatic_transmission = 0.15,
    symptomatic_ascertained = 0
  )
  interventions <- intervention_opts(quarantine = FALSE)

  # generate initial cases
  case_data <- outbreak_setup(
    initial_cases = initial_cases,
    delays = delays,
    event_probs = event_probs
  )

  # generate next generation of cases
  case_data2 <- outbreak_step(
    case_data = case_data,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions
  )

  expect_true(all(case_data2$cases$missed))

  event_probs$symptomatic_ascertained <- 1

  case_data3 <- outbreak_step(
    case_data = case_data,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions
  )

  # The index case should be missed but no others.
  expect_equal(sum(case_data3$cases$missed), 1)

  # To test a mix make sure there's loads of cases.
  offspring$community <- \(n) rnbinom(n = n, mu = 100000, size = 0.16)
  event_probs$symptomatic_ascertained <- 0.5

  case_data4 <- outbreak_step(
    case_data = case_data,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions
  )

  # After ignoring the index case we should still get both true and false.
  expect_length(unique(case_data4$cases$missed[-1]), 2)
})
