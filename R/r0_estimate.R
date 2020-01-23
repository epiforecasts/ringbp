#Estimate R0 for ascertained and missed chains

#' R0 estimated for ascertained and missed chains
#' @author Adam Kucharski
#' @return
#' @export
#'
#' @examples
#'
r0_estimate<-function(){

  # Cases in single cluster
  yy=1/(1-r0within)
  xx=1+r0Amiss*yy

  total.index=1/(1-(1-pr.id)*yy)
  total.cases=xx*total.index

  # Relationship between ascertained and missed

  r0av=r0Amiss*(total.index/total.cases)+r0within*(1-total.index/total.cases)

  r0av

}
