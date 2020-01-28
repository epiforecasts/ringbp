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
  n.sim = max(outbreak_df_week$sim)
  out <- outbreak_df_week %>%
    group_by(sim) %>% # group by simulation run
    filter(week %in% 12:16) %>%
    summarise(extinct=ifelse(all(weekly_cases==0 & cumulative < cap_cases),1,0)) %>% # new variable extinct = 1 if cases in weeks 10-12 all 0, 0 if not
    .$extinct %>% sum(.)/n.sim # number of runs where extinct = TRUE / number of runs
  return(out)
}
