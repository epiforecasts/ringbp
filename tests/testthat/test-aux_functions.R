
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


test_that('extinct_prob works as expected', {
  
  set.seed(12341240)
  cap <- 100
  sims <- 5
  res <- scenario_sim(n.sim = sims,
                      num.initial.cases = 5,
                      cap_max_days = 100,
                      cap_cases = cap,
                      r0isolated = 0,
                      r0community = 2.5,
                      disp.iso = 1,
                      disp.com = 0.16,
                      k = 0.7,
                      delay_shape = 2.5,
                      delay_scale = 5,
                      prop.asym = 0,
                      prop.ascertain = 0)
  
  r1 <- extinct_prob(res, cap)
  
  expect_true(r1 <= 1)
  expect_true(r1 >= 0)
  expect_length(r1, 1)
  
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  
  expect_true(is.wholenumber(r1 * sims))
  
  
  # Manually build an output with known proportion of extinctions
  res2 <- res[c(1, 2, 1, 2), ]
  res2$sim <- c(1, 1, 2, 2)
  
  # enforce that second sim did not have cases in final week.
  #   I am ignoring cases_per_gen becayse I think this function works on weekly cases.
  res2$weekly_cases[4] <- 0
  res2$cumulative[4] <- res2$cumulative[3]
  
  # enforce that first sim has cases both weeks.
  #   I am ignoring cases_per_gen becayse I think this function works on weekly cases.
  res2$weekly_cases[1:2] <- c(1, 1)
  res2$cumulative[1:2] <- c(1, 2)
  
  r2 <- extinct_prob(res2, cap_cases = cap, week_range = 1)
  expect_equal(r2, 0.5)
  
  
  
  # Run some sims with almost certain outputs
  # Very high r0, shouldn't ever go extinct.
  res3 <- scenario_sim(n.sim = sims,
                      num.initial.cases = 5,
                      cap_max_days = 100,
                      cap_cases = cap,
                      r0isolated = 100,
                      r0community = 100,
                      disp.iso = 1,
                      disp.com = 0.16,
                      k = 0.7,
                      delay_shape = 2.5,
                      delay_scale = 5,
                      prop.asym = 0,
                      prop.ascertain = 0)
  
  r3 <- extinct_prob(res3, cap)
  expect_equal(r3, 0)
  
  
  
  # r0 of 0, should always go extinct.
  res3 <- scenario_sim(n.sim = sims,
                       num.initial.cases = 5,
                       cap_max_days = 100,
                       cap_cases = cap,
                       r0isolated = 0,
                       r0community = 0,
                       disp.iso = 1,
                       disp.com = 0.16,
                       k = 0.7,
                       delay_shape = 2.5,
                       delay_scale = 5,
                       prop.asym = 0,
                       prop.ascertain = 0)
  
  r3 <- extinct_prob(res3, cap)
  expect_equal(r3, 1)
  
})




test_that('extinct_prob week_range argument works', {
  
  
  set.seed(12341240)
  cap <- 100
  sims <- 2
  res <- scenario_sim(n.sim = 2,
                      num.initial.cases = 5,
                      cap_max_days = 100,
                      cap_cases = cap,
                      r0isolated = 0,
                      r0community = 2.5,
                      disp.iso = 1,
                      disp.com = 0.16,
                      k = 0.7,
                      delay_shape = 2.5,
                      delay_scale = 5,
                      prop.asym = 0,
                      prop.ascertain = 0)
  
  
  # Manually build an output with known proportion of extinctions
  res2 <- res[c(1, 2, 1, 2), ]
  res2$sim <- c(1, 1, 2, 2)
  
  # enforce that second sim did not have cases in final week.
  #   I am ignoring cases_per_gen becayse I think this function works on weekly cases.
  res2$weekly_cases[4] <- 0
  res2$cumulative[4] <- res2$cumulative[3]
  
  # enforce that first sim has cases both weeks.
  #   I am ignoring cases_per_gen becayse I think this function works on weekly cases.
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
  
  
  
  # Case of cases in week 1 but not 2 (by the definition used in this function this is not an extinction)
  #   test here that week_range 1:2 says extintion by week_range 2 does not.
  res5 <- res[c(1, 2, 3), ]
  res5$weekly_cases[1:3] <- c(1, 1, 0)
  res5$cumulative[1:3] <- c(1, 2, 2)
  r5 <- extinct_prob(res5, cap_cases = cap, week_range = 1:2)
  expect_equal(r5, 0)
  
  
  
  # Case of cases in week 2 but not 1 (by all sensible definitions is not an extinction)
  #   test here that week_range 1:2 says extintion and week_range 2 does as well.
  res6 <- res[c(1, 2, 3), ]
  res6$weekly_cases[1:3] <- c(1, 0, 1)
  res6$cumulative[1:3] <- c(1, 1, 2)
  r6 <- extinct_prob(res6, cap_cases = cap, week_range = 1:2)
  expect_equal(r6, 0)
  
    
})


