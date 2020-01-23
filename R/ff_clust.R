#' Marginal likelihood calculation for cluster number
#' @author Adam Kucharski
#' @param yy
#' @param xx
#'
#' @return
#' @export
#'
#' @examples
#'
ff_clust <- function(yy,xx) {
  dnorm(yy,mean=xx,sd=1)
  #dpois(yy,lambda = xx)
}
