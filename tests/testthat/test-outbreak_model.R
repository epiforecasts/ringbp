
context("Test out_model usage")


test_that("A basic sim returns the correct object", {
  
  set.seed(202010)
  incfn <- dist_setup(dist_shape = 2.322737,dist_scale = 6.492272)
  # delay distribution sampling function
  delayfn <- dist_setup(2, 4)
  
  
  incfn <- dist_setup(dist_shape = 2.322737,dist_scale = 6.492272)
  # delay distribution sampling function
  delayfn <- dist_setup(2, 4)
  # generate initial cases

  r1 <- 
    outbreak_model(num.initial.cases = 1, prop.ascertain = 0.8,
                   cap_max_days = 50, cap_cases = 30,
                   r0isolated = 5, r0community = 5,
                   disp.iso = 1, disp.com = 0.16,
                   k = 2, delay_shape = 1.651524,
                   delay_scale = 4.287786, prop.asym = 0.4,
                   quarantine = FALSE)
    
  
})