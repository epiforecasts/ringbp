
#' Calculate proportion of outbreaks that went extinct
#' @author Joel Hellewell
#' @param outbreak_df_week A tibble of weekly cases producted by the outbreak model
#' @param cap_cases The cumulative number of cases at which the branching process was terminated
#'
#' @return
#' @export
#' @importFrom dplyr group_by filter summarise ungroup
#' @examples
#'
detect_extinct <- function(outbreak_df_week, cap_cases) {
  outbreak_df_week %>%
    dplyr::group_by(sim) %>% # group by simulation run
    dplyr::filter(week %in% 12:16) %>%
    dplyr::summarise(extinct=ifelse(all(weekly_cases==0 & cumulative < cap_cases),1,0)) %>%
    dplyr::ungroup()
}
