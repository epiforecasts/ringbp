# Generate new cluster based on how many new cases a missed case generates
#' Cluster generation function
#' @author Adam Kucharski
#' @param onsett.new
#' @param r0miss
#' @param overkkmiss
#'
#' @return
#' @export
#'
#' @examples
#'
newclustn<-function(onsett.new,r0miss,overkkmiss){
  sapply(onsett.new,function(xx){
    #if((1-pr.id)>runif(1)){rnbinom(1,size=overkkmiss,mu=r0miss)}else{0}
    rnbinom(1,size=overkkmiss,mu=r0miss)
  })
}
