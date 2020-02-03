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
  out <- partial(rweibull,
                 shape = dist_shape,
                 scale = dist_scale)
  return(out)
}

inf_fn <- function(inc_samp,k){
  out <- sn::rsn(n=length(inc_samp),xi = inc_samp,omega = 2,alpha = k)
  out <- ifelse(out<1,1,out)
  return(out)
}

#' Sample from multivariate normal
#'
#' @param mu_ip
#' @param mu_si
#' @param sd_ip
#' @param sd_si
#' @param k
#'
#' @return
#' @export
#' @importFrom mvtnorm rmvnorm
#' @examples
dist_setup_mvt <- function(mu_ip,mu_si,sd_ip,sd_si,k){
  out <- partial(mvtnorm::rmvnorm,
                 mean = c(mu_ip, mu_si),
                 sigma = matrix(c(sd_ip^2,k*sd_ip*sd_si,k*sd_ip*sd_si,sd_si^2),byrow=TRUE,ncol=2))
  return(out)
}



