#' Estimates theta from given R0 and infectiousness distribtion
#' @author Joel Hellewell
#' @param R0
#' @param inf_shape
#' @param inf_scale
#' @param inc_shape
#' @param inc_scale
#'
#' @return
#' @export
#'
#' @examples
#'
calc_theta <- function(R0,inf_shape,inf_scale,inc_shape,inc_scale){

  infecfn <- function(b){dweibull(b,shape=inf_shape,scale=inf_scale)}
  incubfn <- function(a){pweibull(a,shape=inc_shape,scale=inc_scale)}

  top <- integrate(f=function(c){(1-incubfn(c))*infecfn(c)*R0},
                   lower=0,upper=20)$value

  bot <- integrate(f=function(d){infecfn(d)*R0},
                   lower=0,upper=20)$value
  return(top/bot)
}






