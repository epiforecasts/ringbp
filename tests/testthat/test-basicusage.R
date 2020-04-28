
context("Test basic usage")

<<<<<<< HEAD
set.seed(20200428)
res <- ringbp::scenario_sim(n.sim = 2, num.initial.cases = 10, prop.asym=0,
                            prop.ascertain = 0.2, cap_cases = 20, cap_max_days = 100,
                            r0isolated = 0, r0community = 2.5, disp.com = 0.16, disp.iso = 1,
                            delay_shape = 1.651524, delay_scale = 4.287786, inc_meanlog = 1.434065,
                            inc_sdlog = 0.6612, inf_shape = 2.115779, inf_rate = 0.6898583,
                            inf_shift = 3, quarantine = FALSE)


test_that("A basic sim returns the correct object", {

  # Check we got 2 simulations as requested
  expect_true(length(unique(res$sim)) == 2)

  expect_true(inherits(res, 'data.frame'))
  expect_true(inherits(res, 'data.table'))

=======
set.seed(20200410)
res <- ringbp::scenario_sim(n.sim = 2, num.initial.cases = 10, prop.asym=0,
                            prop.ascertain = 0.2, cap_cases = 20, cap_max_days = 100,
                            r0isolated = 0, r0community = 2.5, disp.com = 0.16, disp.iso = 1, 
                            delay_shape = 1.651524,
                            delay_scale = 4.287786,k = 0, quarantine = FALSE)


test_that("A basic sim returns the correct object", {
  
  # Check we got 2 simulations as requested
  expect_true(length(unique(res$sim)) == 2)
  
  expect_true(inherits(res, 'data.frame'))
  expect_true(inherits(res, 'data.table'))
  
>>>>>>> 4517591eecdedae1c3839e2539d2d3dbb73002a1
  # Test that weeks increase.
  # As we have n.sim = 2, exactly 1 sim should be smaller the the previous week
  # Same for cumulative cases.
  expect_equal(sum((res$week[seq(2,nrow(res))] - res$week[seq(nrow(res) - 1)]) < 0), 1)
  expect_equal(sum((res$cumulative[seq(2,nrow(res))] - res$cumulative[seq(nrow(res) - 1)]) < 0), 1)
<<<<<<< HEAD


  # Cap cases doesn't stop the sim going until week = ceiling(100 / 7).
  #   Maybe it just copies the last week?



=======
  
  
  # Cap cases doesn't stop the sim going until week = ceiling(100 / 7).
  #   Maybe it just copies the last week?
  
  
  
>>>>>>> 4517591eecdedae1c3839e2539d2d3dbb73002a1
})
