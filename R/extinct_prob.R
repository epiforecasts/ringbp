#' Calculate proportion of runs that have controlled outbreak
#'
#' @param outbreak_df_week
#' @author Joel Hellewell
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr group_by filter summarise
extinct_prob <- function(outbreak_df_week,cap_cases){
  n.sim = max(outbreak_df_week$n.sim)
  out <- outbreak_df_week %>%
    group_by(n.sim) %>% # group by simulation run
    mutate(cs=cumsum(number)) %>%
    filter(week %in% 12:16) %>%
    summarise(extinct=ifelse(all(number==0 & cs < cap_cases),1,0)) %>% # new variable extinct = 1 if cases in weeks 10-12 all 0, 0 if not
    .$extinct %>% sum(.)/n.sim # number of runs where extinct = TRUE / number of runs
  return(out)
}
