context("Test basic usage")

set.seed(12345678)

test_that("A basic sim returns the correct object", {
  parameters <- parameters(
    initial_cases = 1,
    r0_community = 500, # almost guarentees to get new cases
    r0_isolated = 0,
    r0_asymptomatic = 500,
    disp_community = 0.16,
    disp_isolated = 1,
    disp_asymptomatic = 0.16,
    incubation_period = \(x) stats::rweibull(n = x, shape = 2.32, scale = 6.49),
    prop_presymptomatic = 0.15,
    onset_to_isolation = \(x) stats::rweibull(n = x, shape = 2, scale = 4),
    prop_ascertain = 0,
    prop_asymptomatic = 0,
    quarantine = FALSE
  )

  # generate initial cases
  case_data <- outbreak_setup(parameters = parameters)

  # generate next generation of cases
  case_data2 <- outbreak_step(
    case_data = case_data,
    parameters = parameters
  )

  expect_gt(nrow(case_data2$cases), 1)
  expect_equal(
    as.vector(table(case_data2$cases$infector)),
    c(1, nrow(case_data2$cases) - 1)
  )

  parameters$r0_community <- 0 # almost guarentees to get new cases
  parameters$r0_asymptomatic <- 0

  # With R0 = 0 we should get no additional cases.
  case_data3 <- outbreak_step(
    case_data = case_data,
    parameters = parameters
  )

  expect_equal(nrow(case_data3$cases), 1)
})

test_that("Sim with multiple infectors makes senes", {
  parameters <- parameters(
    initial_cases = 2,
    r0_community = 10000, # almost guarentees both index cases create infections
    r0_isolated = 0,
    r0_asymptomatic = 10000,
    disp_community = 0.16,
    disp_isolated = 1,
    disp_asymptomatic = 0.16,
    incubation_period = \(x) stats::rweibull(n = x, shape = 2.32, scale = 6.49),
    prop_presymptomatic = 0.15,
    onset_to_isolation = \(x) stats::rweibull(n = x, shape = 2, scale = 4),
    prop_ascertain = 0,
    prop_asymptomatic = 0,
    quarantine = FALSE
  )
  # generate initial cases
  case_data <- outbreak_setup(parameters = parameters)

  # generate next generation of cases
  case_data2 <- outbreak_step(
    case_data = case_data,
    parameters = parameters
  )

  expect_gt(nrow(case_data2$cases), 1)
  expect_equal(as.vector(table(case_data2$cases$infector))[1], 2)
  expect_true(all(as.vector(table(case_data2$cases$infector))[2:3] > 1))
})


test_that("r0_isolated is working properly", {
  parameters <- parameters(
    initial_cases = 1,
    r0_community = 500, # Case is isolated so irrelevent
    r0_isolated = 0, # Shoiuld get zero cases
    r0_asymptomatic = 500,
    disp_community = 0.16,
    disp_isolated = 1,
    disp_asymptomatic = 0.16,
    incubation_period = \(x) stats::rweibull(n = x, shape = 2.32, scale = 6.49),
    prop_presymptomatic = 0.15,
    onset_to_isolation = \(x) stats::rweibull(n = x, shape = 2, scale = 4),
    prop_ascertain = 0,
    prop_asymptomatic = 0,
    quarantine = FALSE
  )

  # generate initial cases
  case_data <- outbreak_setup(parameters = parameters)

  case_data$isolated <- TRUE

  # generate next generation of cases
  case_data2 <- outbreak_step(
    case_data = case_data,
    parameters = parameters
  )
  expect_equal(nrow(case_data2$cases), 1)

  parameters$r0_community <- 0 # Case is isolated so irrelevent
  parameters$r0_isolated <- 500 # Shoiuld get lots of cases
  parameters$r0_asymptomatic <- 0

  # generate next generation of cases
  case_data3 <- outbreak_step(
    case_data = case_data,
    parameters = parameters
  )
  expect_gt(nrow(case_data3$cases), 1)
})

test_that("Test a bunch of args", {
  parameters <- parameters(
    initial_cases = 1,
    r0_community = 10000, # almost guarentees both index cases create infections
    r0_isolated = 0,
    r0_asymptomatic = 100000,
    disp_community = 0.16,
    disp_isolated = 1,
    disp_asymptomatic = 0.16,
    incubation_period = \(x) stats::rweibull(n = x, shape = 2.32, scale = 6.49),
    prop_presymptomatic = 0.15,
    onset_to_isolation = \(x) stats::rweibull(n = x, shape = 2, scale = 4),
    prop_ascertain = 0,
    prop_asymptomatic = 0,
    quarantine = FALSE
  )

  # generate initial cases
  case_data <- outbreak_setup(parameters = parameters)

  # generate next generation of cases
  case_data2 <- outbreak_step(
    case_data = case_data,
    parameters = parameters
  )
  expect_true(all(case_data2$cases$missed))

  parameters$prop_ascertain <- 1

  case_data3 <- outbreak_step(
    case_data = case_data,
    parameters = parameters
  )
  # The index case should be missed but no others.
  expect_equal(sum(case_data3$cases$missed), 1)

  parameters$r0_community <- 100000 # To test a mix make sure there's loads of cases.
  parameters$prop_ascertain <- 0.5

  case_data4 <- outbreak_step(
    case_data = case_data,
    parameters = parameters
  )
  # After ignoring the index case we should still get both true and false.
  expect_length(unique(case_data4$cases$missed[-1]), 2)
})
