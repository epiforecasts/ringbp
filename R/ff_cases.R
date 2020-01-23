# - - - 
# Calculate marginal likelihood for each parameter

# Use joint distribution of probability with Poisson observation
# Requires the global variable/data: real_cases
ff_cases <- function(yy,xx,censor0=3000) {
  
  lik1 <- ppois(real_cases,lambda = censor0,lower.tail=T) # Account for cut off in longer runs
  lik2 <- dpois(yy,lambda = xx)
  
  lik <-  (xx>censor0)*lik1 + (xx<=censor0)*lik2 # combine likelihoods
  
  lik
}