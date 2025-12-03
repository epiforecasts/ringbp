offspring <- offspring_opts(
  community = \(n) rep(0, n),
  isolated = \(n) rep(0, n),
  asymptomatic = \(n) rep(0, n)
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

initial_case_data <- outbreak_setup(
  initial_cases = 1,
  delays = delays,
  event_probs = event_probs
)

test_that("outbreak_step creates new cases as expected", {
  # guarantee new cases from community transmission
  offspring$community <- \(n) rep(2, n)

  # set onset-to-isolation to Inf to not remove new cases
  delays$onset_to_isolation <- \(n) rep(Inf, n)

  initial_case_data <- outbreak_setup(
    initial_cases = 1,
    delays = delays,
    event_probs = event_probs
  )

  # generate first generation of cases
  first_gen_case_data <- outbreak_step(
    case_data = initial_case_data,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions
  )

  # community offspring R = 2 so 3 total cases
  expect_identical(nrow(first_gen_case_data$cases), 3L)
  expect_identical(first_gen_case_data$cases$infector, c(0, 1, 1))
  expect_identical(first_gen_case_data$effective_r0, 2)
  expect_identical(first_gen_case_data$cases_in_gen, 2L)
  # initial case gets sampled after first generation
  expect_identical(first_gen_case_data$cases$sampled, c(TRUE, FALSE, FALSE))
})

test_that("outbreak_step creates no new cases as expected", {
  # guarantee to get no new cases
  offspring$community <- \(n) rep(0, n)

  first_gen_case_data <- outbreak_step(
    case_data = initial_case_data,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions
  )

  # all offspring R = 0 so 1 total case
  expect_identical(nrow(first_gen_case_data$cases), 1L)
  expect_identical(first_gen_case_data$effective_r0, 0)
  expect_identical(first_gen_case_data$cases_in_gen, 0)
  # new_cases goes from NA_integer_ to numeric integer after first generation
  expect_true(
    !is.na(first_gen_case_data$cases$new_cases) &&
      is.integer(first_gen_case_data$cases$new_cases)
  )
  # initial case gets sampled after first generation
  expect_identical(first_gen_case_data$cases$sampled, TRUE)
  # only isolation status of initial infector has changed
  expect_identical(
    initial_case_data[, !c("new_cases", "sampled")],
    first_gen_case_data$cases[, !c("new_cases", "sampled")]
  )
})

test_that("outbreak_step with > 1 initial infections", {
  offspring$community <- \(n) rep(100, n)
  offspring$asymptomatic <- \(n) rep(100, n)

  initial_case_data <- outbreak_setup(
    initial_cases = 2,
    delays = delays,
    event_probs = event_probs
  )

  first_gen_case_data <- outbreak_step(
    case_data = initial_case_data,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions
  )

  # 200 new cases but some are removed from isolation truncating the offspring
  expect_lt(first_gen_case_data$cases_in_gen, 200)
  # 50 is a approximate threshold that should be exceeded given the incubation
  # period and onset-to-isolation time parameters
  expect_gt(first_gen_case_data$cases_in_gen, 50)
  # first two cases have infector zero (index case)
  expect_identical(first_gen_case_data$cases$infector[1:2], rep(0, 2))
  # all new cases are infected by either case 1 or 2
  expect_identical(unique(first_gen_case_data$cases$infector[-(1:2)]), c(1, 2))
  # initial case gets sampled after first generation
  expect_identical(first_gen_case_data$cases$sampled[1:2], rep(TRUE, 2))
  # all new cases are not sampled
  expect_identical(
    first_gen_case_data$cases$sampled[3:nrow(first_gen_case_data$cases)],
    rep(FALSE, nrow(first_gen_case_data$cases) - 2)
  )
  # R should be much larger than 3
  expect_gt(first_gen_case_data$effective_r0, 3)
  expect_identical(
    first_gen_case_data$cases_in_gen,
    nrow(first_gen_case_data$cases) - 2L
  )
})

test_that("sampled cases behave as expected", {
  # case is sampled so should not trasmit in the community
  offspring$community <- \(n) rep(100, n)

  initial_case_data$sampled <- TRUE

  # generate next generation of cases
  first_gen_case_data <- outbreak_step(
    case_data = initial_case_data,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions
  )

  # initial case is isolated and isolated R = 0 so 1 total case
  expect_identical(nrow(first_gen_case_data$cases), 1L)
  expect_identical(first_gen_case_data$effective_r0, 0)
  expect_identical(first_gen_case_data$cases_in_gen, 0)
  expect_identical(initial_case_data, first_gen_case_data$cases)
})

test_that("isolated cases transmit as expected", {
  offspring$community <- \(n) rep(0, n)
  offspring$isolated <- \(n) rep(100, n)

  # set short onset-to-isolation so cases isolate quickly and most transmission
  # in isolation is kept
  delays$onset_to_isolation <- \(n) rep(0.5, n)

  initial_case_data <- outbreak_setup(
    initial_cases = 1,
    delays = delays,
    event_probs = event_probs
  )

  # generate next generation of cases
  first_gen_case_data <- outbreak_step(
    case_data = initial_case_data,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions
  )

  # initial case is isolated and isolated R > 0 so multiple case
  # 100 cases but some are removed from truncating the offspring from isolation
  # 25 is a approximate threshold that should be exceeded
  expect_gt(nrow(first_gen_case_data$cases), 25)
  # R >> 0, 25 is an approximate threshold that should be exceeded
  expect_gt(first_gen_case_data$effective_r0, 25)
  expect_gt(first_gen_case_data$cases_in_gen, 25)
})

test_that("All cases are missed when ascertained = 0", {
  offspring$community <- \(n) rep(100, n)
  offspring$asymptomatic <- \(n) rep(100, n)

  # generate next generation of cases
  first_gen_case_data <- outbreak_step(
    case_data = initial_case_data,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions
  )

  expect_true(all(first_gen_case_data$cases$missed))
})

test_that("No cases are missed when ascertained = 1", {
  offspring$community <- \(n) rep(100, n)
  offspring$asymptomatic <- \(n) rep(100, n)

  event_probs$symptomatic_ascertained <- 1

  # generate next generation of cases
  first_gen_case_data <- outbreak_step(
    case_data = initial_case_data,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions
  )

  # only the index case is missed, all others are ascertained
  expect_true(first_gen_case_data$cases$missed[1])
  expect_false(any(
    first_gen_case_data$cases$missed[2:nrow(first_gen_case_data$cases)]
  ))
})

test_that("Some cases are missed when ascertained = 0.5", {
  offspring$community <- \(n) rep(100, n)
  offspring$asymptomatic <- \(n) rep(100, n)

  event_probs$symptomatic_ascertained <- 0.5

  # generate next generation of cases
  first_gen_case_data <- outbreak_step(
    case_data = initial_case_data,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions
  )

  # only the index case is missed, all others are ascertained
  missed <- table(first_gen_case_data$cases$missed)
  expect_identical(names(missed), c("FALSE", "TRUE"))
  # multiple missed and ascertained, 5 is an arbitrary threshold
  expect_gt(missed[1], 5)
  expect_gt(missed[2], 5)
})
