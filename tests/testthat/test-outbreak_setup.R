test_that("outbreak_setup works as expected", {
  set.seed(1)
  expect_snapshot(
    outbreak_setup(
      initial_cases = 5,
      delays = delay_opts(
        incubation_period = \(n) stats::rweibull(n = n, shape = 2.32, scale = 6.49),
        onset_to_isolation = \(n) stats::rweibull(n = n, shape = 2, scale = 4)
      ),
      event_probs = event_prob_opts(
        asymptomatic = 0.2,
        presymptomatic_transmission = 0.5,
        symptomatic_ascertained = 0.8
      )
    )
  )
})

test_that("outbreak_setup with dynamic seed and parameters runs as expected", {
  seed <- as.integer(Sys.Date())
  if (on_ci()) message("Seed: ", seed)
  set.seed(seed)
  asymptomatic <- runif(n = 1, min = 0, max = 1)
  res <- outbreak_setup(
    initial_cases = sample(1:50, size = 1),
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
      asymptomatic = asymptomatic,
      presymptomatic_transmission = runif(n = 1, min = 0, max = 1),
      symptomatic_ascertained = runif(n = 1, min = 0, max = 1)
    )
  )
  expect_s3_class(res, class = c("data.table", "data.frame"), exact = TRUE)
  expect_identical(
    vapply(res, class, FUN.VALUE = character(1)),
    c(exposure = "numeric", asymptomatic = "logical", caseid = "integer",
      infector = "numeric", isolated = "logical", missed = "logical",
      onset = "numeric", new_cases = "logical", isolated_time = "numeric")
  )
  expect_identical(unique(res$exposure), 0)
  expect_identical(res$caseid, 1:nrow(res))
  expect_identical(unique(res$infector), 0)
  expect_identical(unique(res$isolated), FALSE)
  expect_identical(unique(res$missed), TRUE)
  expect_identical(unique(res$new_cases), NA)
})

test_that("outbreak_setup has expected distribution properties (dynamic seed)", {
  seed <- as.integer(Sys.Date())
  if (on_ci()) message("Seed: ", seed)
  set.seed(seed)
  initial_cases <- 1e5
  asymptomatic <- 0.5
  incubation_shape <- runif(n = 1, min = 2, max = 4)
  incubation_scale <- runif(n = 1, min = 2, max = 6)
  incubation_period <- \(n) rweibull(
    n = n,
    shape = incubation_shape,
    scale = incubation_scale
  )

  onset_to_isolation_shape <- runif(n = 1, min = 2, max = 4)
  onset_to_isolation_scale <- runif(n = 1, min = 2, max = 6)
  onset_to_isolation <- \(n) rweibull(
    n = n,
    shape = onset_to_isolation_shape,
    scale = onset_to_isolation_scale
  )

  res <- outbreak_setup(
    initial_cases = initial_cases,
    delays = delay_opts(
      incubation_period = incubation_period,
      onset_to_isolation = onset_to_isolation
    ),
    event_probs = event_prob_opts(
      asymptomatic = asymptomatic,
      presymptomatic_transmission = 0,
      symptomatic_ascertained = 0
    )
  )

  # proportion of asymptomatic cases is approximately equal to input parameter
  expect_equal(
    sum(res$asymptomatic) / nrow(res),
    asymptomatic,
    tolerance = 0.05
  )
  # test that onset times match incubation period distribution
  # accept the null hypothesis that both distributions are the same
  expect_gt(
    # suppress warning about approximate p-value
    suppressWarnings(
      ks.test(res$onset, incubation_period(initial_cases))$p.value
    ),
    expected = 0.05
  )

  # remove asymptomatic cases that have an Inf isolation time
  res <- res[asymptomatic == FALSE, ]

  # test that isolation times match onset-to-isolation distribution
  # accept the null hypothesis that both distributions are the same
  expect_gt(
    # suppress warning about approximate p-value
    suppressWarnings(
      ks.test(
        (res$isolated_time - res$onset),
        onset_to_isolation(initial_cases)
      )$p.value
    ),
    expected = 0.05
  )
})
