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
dist_setup <- function(dist_mean,dist_var){
  dist_param <- c(dist_mean,dist_var)
  out <- partial(rgamma,
                 shape = dist_param[1] / (dist_param[2] ^ 2 / dist_param[1]),
                 scale = (dist_param[2] ^ 2 / dist_param[1]))
  return(out)
}
