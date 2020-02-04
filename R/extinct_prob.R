#' Calculate proportion of runs that have controlled outbreak
#'
#' @author Joel Hellewell
#' @return
#' @export
#' @inheritParams detect_extinct
#' @examples
#'
extinct_prob <- function(outbreak_df_week,cap_cases){
  n.sim = max(outbreak_df_week$sim)
  out <- outbreak_df_week %>%
    detect_extinct(cap_cases) %>% # new variable extinct = 1 if cases in weeks 10-12 all 0, 0 if not
    .$extinct %>% sum(.) / n.sim # number of runs where extinct = TRUE / number of runs
  return(out)
}
