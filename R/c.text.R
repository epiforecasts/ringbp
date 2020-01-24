#' Text summary of simulated outbreak
#' @author Adam Kucharski
#' @param x
#' @param sigF
#'
#' @return
#' @export
#'
#' @examples
#'
c_text<-function(x,sigF=3){
  bp1=signif(c(median(x),quantile(x,0.025),quantile(x,0.975)),sigF)
  paste(bp1[1]," (",bp1[2],"-",bp1[3],")",sep="")
}

