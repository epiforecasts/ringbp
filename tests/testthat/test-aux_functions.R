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
    ), regexp = paste(
      "Unable to sample generation times satisfying `latent_period` >= ",
      "`incubation_period`", "Consider reducing the `latent_period`",
      sep = ".*"
    )
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
      "Estimating the `alpha` parameter from `presymptomatic_transmission`",
      "did not converge"
    )
  )
})

test_that("as_prob_function coerces a numeric scalar to a constant function", {
  f <- as_prob_function(0.3)
  expect_type(f, "closure")
  # output length matches input vector length
  expect_identical(f(c(0, 5, 10)), rep(0.3, 3))
  expect_identical(f(numeric(0)), numeric(0))
  expect_identical(f(seq_len(100)), rep(0.3, 100))
})

test_that("as_prob_function coerces probability boundaries (0 and 1)", {
  expect_identical(as_prob_function(0)(c(1, 2, 3)), c(0, 0, 0))
  expect_identical(as_prob_function(1)(c(1, 2, 3)), c(1, 1, 1))
})

test_that("as_prob_function passes valid functions through unchanged", {
  g <- \(t) ifelse(t < 30, 0, 0.8)
  f <- as_prob_function(g)
  expect_identical(f, g)
  expect_identical(f(c(10, 40)), c(0, 0.8))
})

test_that("as_prob_function errors on out-of-range scalar input", {
  expect_error(
    as_prob_function(-0.1),
    regexp = "not >= 0"
  )
  expect_error(
    as_prob_function(1.5),
    regexp = "not <= 1"
  )
})

test_that("as_prob_function errors on functions returning out-of-range probabilities", {
  expect_error(
    as_prob_function(\(t) rep(1.5, length(t))),
    regexp = "not <= 1"
  )
  expect_error(
    as_prob_function(\(t) rep(-0.1, length(t))),
    regexp = "not >= 0"
  )
})

test_that("as_prob_function errors on functions returning the wrong length", {
  expect_error(
    as_prob_function(\(t) 0.5),
    regexp = "length"
  )
})

test_that("as_prob_function errors on functions returning missing values", {
  expect_error(
    as_prob_function(\(t) rep(NA_real_, length(t))),
    regexp = "missing"
  )
})

test_that("as_prob_function errors on functions with the wrong number of arguments", {
  expect_error(
    as_prob_function(\(t, extra) 0.5),
    regexp = "Must have exactly 1 formal arguments"
  )
})

test_that("as_prob_function errors on non-numeric, non-function input", {
  expect_error(
    as_prob_function("0.5"),
    regexp = "Probabilities must be `numeric` or a `numeric` generating function"
  )
  expect_error(
    as_prob_function(NULL),
    regexp = "Probabilities must be `numeric` or a `numeric` generating function"
  )
  expect_error(
    as_prob_function(list(0.5)),
    regexp = "Probabilities must be `numeric` or a `numeric` generating function"
  )
})
