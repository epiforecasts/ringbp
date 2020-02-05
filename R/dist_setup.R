#' Create partial function to sample from gamma distributions
#' @author Joel Hellewell
#' @param dist_mean mean of gamma distribution
#' @param dist_var variance of gamma distribution
#'
#' @return
#' @export
#' @importFrom purrr partial
#' @examples
#'
dist_setup <- function(dist_shape,dist_scale){
  out <- purrr::partial(rweibull,
                 shape = dist_shape,
                 scale = dist_scale)
  return(out)
}


#' Samples the serial interval
#'
#' @param inc_samp A vector of samples from the incubation period
#' @param k The skew parameter for sampling the serial interval from the incubation period
#'
#' @return
#' @export
#' @importFrom sn rsn
#' @examples
#'
inf_fn <- function(inc_samp,k){
  out <- sn::rsn(n=length(inc_samp),xi = inc_samp,omega = 2,alpha = k)
  out <- ifelse(out<1,1,out)
  return(out)
}



