
context("Test basic usage")


test_that("A basic sim returns the correct object", {
  
  set.seed(20200410)
  incfn <- dist_setup(dist_shape = 2.322737,dist_scale = 6.492272)
  # delay distribution sampling function
  delayfn <- dist_setup(2, 4)
  # generate initial cases
  case_data <- outbreak_setup(num.initial.cases = 1,
                              incfn=incfn,
                              delayfn = delayfn,
                              k=1.95,
                              prop.asym=0)
  
  # generate next generation of cases
  case_data2 <- outbreak_step(case_data = case_data,
                             disp.iso = 1,
                             disp.com = 0.16,
                             r0isolated = 0,
                             r0community = 500, # almost guarentees to get new cases
                             prop.asym = 0,
                             incfn = incfn,
                             delayfn = delayfn,
                             prop.ascertain = 0,
                             k = 1.95,
                             quarantine = FALSE)
  
  expect_true(nrow(case_data2$cases) > 1)
  expect_equal(as.vector(table(case_data2$cases$infector)), c(1, nrow(case_data2$cases) - 1))
    
  
  
  # With R0 = 0 we should get no additional cases.
  case_data3 <- outbreak_step(case_data = case_data,
                              disp.iso = 1,
                              disp.com = 0.16,
                              r0isolated = 0,
                              r0community = 0, # almost guarentees to get new cases
                              prop.asym = 0,
                              incfn = incfn,
                              delayfn = delayfn,
                              prop.ascertain = 0,
                              k = 1.95,
                              quarantine = FALSE)
  
  expect_true(nrow(case_data3$cases) == 1)

  
})



test_that("R0isolated is working properly", {
  
  set.seed(20200410)
  incfn <- dist_setup(dist_shape = 2.322737,dist_scale = 6.492272)
  # delay distribution sampling function
  delayfn <- dist_setup(2, 4)
  # generate initial cases
  case_data <- outbreak_setup(num.initial.cases = 1,
                              incfn=incfn,
                              delayfn = delayfn,
                              k=1.95,
                              prop.asym=0)
  case_data$isolated <- TRUE
  
  # generate next generation of cases
  case_data2 <- outbreak_step(case_data = case_data,
                              disp.iso = 1,
                              disp.com = 0.16,
                              r0isolated = 0, # Shoiuld get zero cases
                              r0community = 500, # Case is isolated so irrelevent
                              prop.asym = 0,
                              incfn = incfn,
                              delayfn = delayfn,
                              prop.ascertain = 0,
                              k = 1.95,
                              quarantine = FALSE)
  
  expect_true(nrow(case_data2$cases) == 1)
  
  
  
  # generate next generation of cases
  case_data3 <- outbreak_step(case_data = case_data,
                              disp.iso = 1,
                              disp.com = 0.16,
                              r0isolated = 500, # Shoiuld get lots of cases
                              r0community = 0, # Case is isolated so irrelevent
                              prop.asym = 0,
                              incfn = incfn,
                              delayfn = delayfn,
                              prop.ascertain = 0,
                              k = 1.95,
                              quarantine = FALSE)
  
  expect_true(nrow(case_data3$cases) > 1)
  