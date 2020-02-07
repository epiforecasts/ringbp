#' Make Figure S4
#'
#' @param res
#'
#' @return
#' @export
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 ggplot geom_line facet_wrap geom_point ylab xlab scale_y_continuous scale_x_continuous theme
#' @importFrom ggplot2 scale_color_manual scale_fill_manual
#' @importFrom patchwork
#' @importFrom cowplot panel_border plot_layout plot_spacer
#'
#' @examples
#'
make_figure_S4 <- function(res){
theta_asym <- res %>%
  dplyr::filter(num.initial.cases==20,
                delay == "SARS",
                index_R0 == 2.5) %>%
  dplyr::mutate(prop.asym = factor(prop.asym,levels=c(0,0.1),labels = c("No asymptomatic cases ","10% cases asmyptomatic"))) %>%
  ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(theta))) + geom_line(size=0.75) +
  geom_point(shape=21,col="black",aes(fill=as.factor(theta)),size=3) +
  scale_fill_manual(guide="none",values = c("mediumpurple2","black","mediumpurple4")) +
  scale_color_manual(values=c("mediumpurple2","black","mediumpurple4"), name = "Percentage of\ntransmission\nbefore symptoms") +
  theme_cowplot() + facet_wrap(prop.asym ~ .) + theme(axis.title = element_text(size=12)) +
  labs(y="Simulated outbreaks controlled (%)",x="Contacts traced (%)") + scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
  scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) + panel_border()

theta_asym
}

