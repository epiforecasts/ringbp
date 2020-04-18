
context("Test all aux functions")

set.seed(515)



test_that("dist_setup returns a partialised function", {
  
  r1 <- dist_setup(0.1, 2)
  
  expect_error(r1(20), NA)

  expect_true(inherits(r1, 'purrr_function_partial'))
  
})


test_that("dist_setup parameters behave as expected", {
  
  set.seed(32515)
  
  f1 <- dist_setup(1, 0.01)
  f2 <- dist_setup(1, 1e5)
  
  
  r1 <- f1(1e5)
  r2 <- f2(1e5)
  
  expect_true(mean(r1) < mean(r2))
  
  expect_true(inherits(f1, 'purrr_function_partial'))
  expect_true(is.numeric(r1))
  expect_length(r1, 1e5)
  
  f3 <- dist_setup(0.01, 10)
  r3 <- f3(1e5)
  expect_true(mean(r3) > median(r3))  
  
  f4 <- dist_setup(50, 10)
  r4 <- f4(1e5)
  expect_true(mean(r4) < median(r4))  
  
  
})





test_that("inf_fn parameters behave as expected", {
  
  set.seed(32515)
  r1 <- inf_fn(c(1, 4, 1), 2)
  
  expect_length(r1, 3)
  
  
  set.seed(499211)
  r2 <- inf_fn(rep(0.01, 1e6), 2)
  expect_length(r2, 1e6)
  expect_true(is.numeric(r2))
  expect_false(anyNA(r2))
  
  # Function forces a minimum of 1.
  expect_equal(min(r2), 1)
  expect_true(all(r2 >= 1))
  
  # skew normal collapses to normal with alpha = k =  0
  # SD is hard coded as 2
  # Can't be bothered to quite do this properly but upper 99%
  #   of normal with mean 1 sd 2: qnorm(0.99, 0, 2) = 4.7
  # probability that normal with mean 1e6, sd 2 being less than 4.7
  #   pnorm(4.7, 1e6, 2) < 1e-200
  # So this test should almost certainly pass if code is correct.
  r3 <- inf_fn(c(1, 1e6), 0)
  expect_true(r3[1] < r3[2])

  # Test the alpha parameter. 
  # positive is right skew so mean is greater than median.
  r4 <- inf_fn(rep(100, 1e6), 10)
  expect_true(mean(r4) > median(r4))

  # negative is left skew so mean is less than median.    
  r5 <- inf_fn(rep(100, 1e6), 0.01)
  expect_true(mean(r5) < median(r5))
  
})


