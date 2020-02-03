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
                                  expression(paste("Delay from onset to isolation from SARS outbreak (mean = 3.8 days)")),
                                  expression(paste("Delay from onset to isolation from Wuhan nCoV outbreak (mean = 9.1 days)"))))) %>%
    dplyr::mutate(index_R0=factor(index_R0, levels=c("1.5","2.0", "2.5", "3.0", "3.5"),
                               labels= c(expression(paste("Index R0 1.5")),
                                         expression(paste("Index R0 2.0")),
                                         expression(paste("Index R0 2.5")),
                                         expression(paste("Index R0 3.0")),
                                         expression(paste("Index R0 3.5"))))) %>%
    dplyr::mutate(control_effectiveness =
                  round(control_effectiveness, 1) %>%
                  factor(levels = c("0","0.2","0.4","0.6","0.8", "1"),
                         labels = c(
                           expression("0"),
                           expression("0.2"),
                           expression("0.4"),
                           expression("0.6"),
                           expression("0.8"),
                           expression("1.0")
                         )))

  return(df)
}
