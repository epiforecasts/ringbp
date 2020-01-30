#' Detect if a simulation is extinct
#'
#' @param outbreak_df_week
#' @param cap_cases
#' @author Sam Abbott
#' @return
#' @export
#' @importFrom dplyr group_by filter summarise
#' @examples
detect_extinct <- function(outbreak_df_week, cap_cases) {
  outbreak_df_week %>%
    dplyr::group_by(sim) %>% # group by simulation run
    dplyr::filter(week %in% 12:16) %>%
    dplyr::summarise(extinct=ifelse(all(weekly_cases==0 & cumulative < cap_cases),1,0)) %>%
    dplyr::ungroup()
}
