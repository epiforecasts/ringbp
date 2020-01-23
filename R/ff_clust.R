# - - - 
# Calculate marginal likelihood for each parameter

# Use joint distribution of probability with Poisson observation

ff_clust <- function(yy,xx) {
  dnorm(yy,mean=xx,sd=1)
  #dpois(yy,lambda = xx)
}