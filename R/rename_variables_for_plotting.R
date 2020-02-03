#' Rename variables for plotting
#'
#' @param df Dataframe of results
#'
#' @return
#' @export
#' @importFrom dplyr mutate
#' @examples
#'
rename_variables_for_plotting <- function(df) {

  df %>%
    dplyr::mutate(theta=factor(theta,levels=c("<1%","15%","30%"),
                       labels=c(expression(paste("under 1% of transmission before symptom onset")),
                                expression(paste("15% of transmission before symptom onset")),
                                expression(paste("30% of transmission before symptom onset"))))) %>%
    dplyr::mutate(delay=factor(delay, levels=c("Best case","SARS","Wuhan"),
                        labels= c(expression(paste("Best case delay from onset to isolation (mean = 2 days)")),
                                  expression(paste("3.8 day mean delay ")),
                                  expression(paste("9.1 day mean delay")))))
}
