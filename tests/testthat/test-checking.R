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
