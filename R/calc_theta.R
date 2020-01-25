#' Estimates theta from given R0 and infectiousness distribtion
#' @author Joel Hellewell
#' @param R0
#' @param inf_mean
#'
#' @return
#' @export
#'
#' @examples
#'
calc_theta <- function(R0,inf_mean){


  inf_var <- 2
  inf_shape <- inf_mean / (inf_var^2 / inf_mean)
  inf_scale <- inf_var^2 / inf_mean
  infecfn <- function(b){ dgamma(b, shape=inf_shape,scale=inf_scale)}

  top <- integrate(f=function(c){incubfn(c)*infecfn(c)*R0},
                   lower=0,upper=20)$value

  bot <- integrate(f=function(d){infecfn(d)*R0},
                   lower=0,upper=20)$value
  return(top/bot)
}






