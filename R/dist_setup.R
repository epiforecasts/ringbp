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
