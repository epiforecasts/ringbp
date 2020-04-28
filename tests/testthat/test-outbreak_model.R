
context("Test out_model usage")


test_that("A basic sim returns the correct object", {
<<<<<<< HEAD

  set.seed(202010)
  incfn <- dist_setup(1.434065,0.6612,dist_type='lognormal')
  # delay distribution sampling function
  delayfn <- dist_setup(2, 4,dist_type='weibull')


  incfn <- dist_setup(1.434065,0.6612,dist_type='lognormal')
  # delay distribution sampling function
  delayfn <- dist_setup(2, 4,dist_type='weibull')
  # generate initial cases

  r1 <-
=======
  
  set.seed(202010)
  incfn <- dist_setup(dist_shape = 2.322737,dist_scale = 6.492272)
  # delay distribution sampling function
  delayfn <- dist_setup(2, 4)
  
  
  incfn <- dist_setup(dist_shape = 2.322737,dist_scale = 6.492272)
  # delay distribution sampling function
  delayfn <- dist_setup(2, 4)
  # generate initial cases

  r1 <- 
>>>>>>> 4517591eecdedae1c3839e2539d2d3dbb73002a1
    outbreak_model(num.initial.cases = 1, prop.ascertain = 0.8,
                   cap_max_days = 50, cap_cases = 30,
                   r0isolated = 5, r0community = 5,
                   disp.iso = 1, disp.com = 0.16,
<<<<<<< HEAD
                   delay_shape = 1.651524, inf_shape = 2.115779,
                   inf_rate = 0.6898583, inf_shift = 3,
                   inc_meanlog = 1.434065, inc_sdlog = 0.6612,
                   delay_scale = 4.287786, prop.asym = 0.4,
                   quarantine = FALSE)

  # Check initialise at time = 0
  expect_equal(r1$week[1],0)
  # Check final week entry is correct (increment by 1 with each time step)
  expect_equal(tail(r1$week,1)+1, length(r1$week))

  # Check cumulative cases increase from start to end
  expect_true(tail(r1$cumulative,1) >= head(r1$cumulative,1))
  # Check final cumulative cases equal to sum of all cases
  expect_true(tail(r1$cumulative,1) == sum(r1$weekly_cases))

  # Check R0 always >=0
  expect_true(all(r1$effective_r0 >= 0))

})
=======
                   k = 2, delay_shape = 1.651524,
                   delay_scale = 4.287786, prop.asym = 0.4,
                   quarantine = FALSE)
    
  
})
>>>>>>> 4517591eecdedae1c3839e2539d2d3dbb73002a1
