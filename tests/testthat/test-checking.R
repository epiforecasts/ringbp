test_that("check_dist_func works as expected", {
  expect_true(check_dist_func(func = \(n) rgamma(n = n, shape = 2, scale = 2)))
})

test_that("check_dist_func errors when not a function", {
  expect_error(
    check_dist_func(func = "function"),
    regexp = "(func)*(failed)*(Must be a function)"
  )
})

dist_func_error <- paste0(
  "(generator must be a function with 1 argument(s) that returns a vector)*",
  "(of non-negative numbers with length equal to the input argument.)"
)

test_that("check_dist_func errors with non-negative non-numeric evaluation", {
  expect_error(
    check_dist_func(func = \(n) rep("i", n), dist_name = "generator"),
    regexp = dist_func_error
  )
  expect_error(
    check_dist_func(func = \(n) rep(-1, n), dist_name = "generator"),
    regexp = dist_func_error
  )
})

test_that("check_dist_func errors with incorrect number of arguments", {
  expect_error(
    check_dist_func(func = \(x, y) x + y, dist_name = "generator"),
    regexp = dist_func_error
  )
})

test_that("check_dist_func errors when output length does not equal input arg", {
  expect_error(
    check_dist_func(func = \(n) 1:5, dist_name = "generator"),
    regexp = dist_func_error
  )
})

test_that("cross_check_opts works as expected", {
  delays <- delay_opts(
    incubation_period = \(n) rweibull(n = n, shape = 1, scale = 1),
    onset_to_isolation = \(n) rweibull(n = n, shape = 1, scale = 1)
  )
  event_probs <- event_prob_opts(
    asymptomatic = 0.1,
    presymptomatic_transmission = 0.5,
    symptomatic_traced = 0.2
  )
  expect_true(cross_check_opts(delays = delays, event_probs = event_probs))
})

test_that("cross_check_opts errors when delay is NULL and event is non-zero", {
  delays <- delay_opts(
    incubation_period = \(n) rweibull(n = n, shape = 1, scale = 1),
    onset_to_isolation = \(n) rweibull(n = n, shape = 1, scale = 1)
  )
  event_probs <- event_prob_opts(
    asymptomatic = 0.1,
    presymptomatic_transmission = 0.5,
    symptomatic_traced = 0.2,
    symptomatic_self_isolate = 0.1
  )
  expect_error(
    cross_check_opts(delays = delays, event_probs = event_probs),
    regexp = paste(
      "(A non-zero `symptomatic_self_isolate` has been specified)",
      "(but `onset_to_self_isolation` is `NULL`)",
      sep = "*"
    )
  )
})

test_that("cross_check_opts warns when delay is function and event is zero", {
  delays <- delay_opts(
    incubation_period = \(n) rweibull(n = n, shape = 1, scale = 1),
    onset_to_isolation = \(n) rweibull(n = n, shape = 1, scale = 1),
    onset_to_self_isolation = \(n) rweibull(n = n, shape = 1, scale = 1)
  )
  event_probs <- event_prob_opts(
    asymptomatic = 0.1,
    presymptomatic_transmission = 0.5,
    symptomatic_traced = 0.2
  )
  expect_warning(
    cross_check_opts(delays = delays, event_probs = event_probs),
    regexp = paste(
      "(An `onset_to_self_isolation` delay has been specified)",
      "(but the `symptomatic_self_isolate` in `event_prob_opts()` is zero.)",
      "(Ignoring `onset_to_self_isolation`.)",
      sep = "*"
    )
  )
})
