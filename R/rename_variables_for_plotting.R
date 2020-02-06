#' Rename variables for plotting
#'
#' @param df Dataframe of results
#'
#' @return
#' @export
#' @importFrom dplyr mutate
#' @author Sam Abbott
#' @examples
#'
rename_variables_for_plotting <- function(df) {

  df %>%
    dplyr::mutate(delay=factor(delay, levels=c("Best case","SARS","Wuhan"),
                        labels= c(paste("Best case delay from onset to isolation (mean = 2 days)"),
                                  paste("Short delay"),
                                  paste("Long delay")))) %>%
    dplyr::mutate(index_R0 = index_R0 %>%
                    paste0("R0 = ", .))
}
