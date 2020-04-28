
context("Test basic usage")

set.seed(20200428)



test_that("A basic sim setup returns the correct object", {

  incfn <- dist_setup(1.434065,0.6612,dist_type='lognormal')
  # delay distribution sampling function
  delayfn <- dist_setup(2, 4, 'weibull')
  # generate initial cases
  case_data <- outbreak_setup(num.initial.cases = 5,
                              incfn=incfn,
                              delayfn = delayfn,
                              prop.asym=0)

  expect_equal(nrow(case_data), 5)
  expect_true(all(case_data$missed))
  expect_true(all(!case_data$asym))


})


test_that("asym arg works properly", {


  incfn <- dist_setup(1.434065,0.6612,dist_type='lognormal')
  # delay distribution sampling function
  delayfn <- dist_setup(2, 4,'weibull')
  # generate initial cases
  # All asymptomatics
  all_asym <- outbreak_setup(num.initial.cases = 5,
                              incfn=incfn,
                              delayfn = delayfn,
                              prop.asym=1)
  expect_true(all(all_asym$asym))


  set.seed(110102932)
  # Mixed asympt
  # dbinom(0, 10000, 0.5)
  # With 10000 cases, probability of 0 symptomatic or 0 asympt is less than machine precision
  mix <- outbreak_setup(num.initial.cases = 10000,
                             incfn=incfn,
                             delayfn = delayfn,
                             prop.asym=0.5)

  expect_length(unique(mix$asym), 2)

})

