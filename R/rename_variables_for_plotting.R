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
    dplyr::mutate(latent=factor(latent,levels=c("short","medium","long","very long"),
                       labels=c(expression(paste("Short pre-infectious period (", theta, "=56%)")),
                                expression(paste("Medium pre-infectious period (", theta, "=44%)")),
                                expression(paste("Long pre-infectious period (", theta, "=29%)")),
                                expression(paste("Very long pre-infectious period (", theta, "=20%)"))))) %>%
    dplyr::mutate(delay=factor(delay, levels=c("short","medium","long"),
                        labels= c(expression(paste("Short delay from onset to isolation (mean = 3.1 days)")),
                                  expression(paste("Medium delay from onset to isolation (mean = 4.9 days)")),
                                  expression(paste("Long delay from onset to isolation (mean = 6.6 days)")))))
}
