#' Heatmap results
#'
#' @return
#' @export
#' @importFrom ggplot2 ggplot geom_tile facet_grid ylab xlab scale_x_discrete theme_bw scale_fill_gradient geom_text ggtitle theme aes element_text
#' @importFrom dplyr group_by mutate
#' @examples
#'
#'
#'\dontrun{
#'## Load results from package directory
#'res <- readRDS("data-raw/res.rds")
#'
#'
#'heatmap_results(res)
#'}
heatmap_results <- function(results){

plot_df <- results %>%
  group_by(scenario) %>%
  mutate(prob_extinct = extinct_prob(sims[[1]],cap_cases = 5000))

fig1 <- plot_df %>%
  rename_variables_for_plotting() %>%
  ggplot() +
  geom_tile(aes(x=as.factor(control_effectiveness),y=as.factor(index_R0),fill=prob_extinct)) +
  facet_grid(delay ~ theta, labeller = label_parsed) +
  ylab("Basic reproduction number for missed cases") +
  xlab("Proportion of infected contacts ascertained by contact tracing") +
  scale_x_discrete(breaks = seq(0,1,0.2),labels = paste0(seq(0,100,20),"%")) +
  theme_bw() +
  scale_fill_gradient(low = "white",high = "deepskyblue3",guide="none") +
  geom_text(aes(label=ifelse(prob_extinct>0.05,paste0(signif(100*prob_extinct,2),"%"),""),
                x=as.factor(control_effectiveness),y=as.factor(index_R0)),fontface="bold") +
  ggtitle("Proportion of simulations where the outbreak had been controlled within 3 months of initial exposure") +
  theme(plot.title = element_text(hjust = 0.5),strip.text.x = element_text(size=9),
        strip.text.y = element_text(size=9),axis.text = element_text(size=11),
        axis.title = element_text(size=12))

return(fig1)
}


