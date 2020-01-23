#' Marginal likelihood calculation for each parameter
#' @author Adam Kucharski
#' @param yy
#' @param xx
#' @param censor0
#'
#' @return
#' @export
#'
#' @examples
#'
ff_cases <- function(yy,xx,censor0=3000) {

  lik1 <- ppois(real_cases,lambda = censor0,lower.tail=T) # Account for cut off in longer runs
  lik2 <- dpois(yy,lambda = xx)

  lik <-  (xx>censor0)*lik1 + (xx<=censor0)*lik2 # combine likelihoods

  lik
}
