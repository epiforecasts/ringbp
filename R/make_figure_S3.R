make_figure_S3 <- function(res){
  delay_theta <- res %>%
    dplyr::filter(num.initial.cases==20,
                  index_R0 == 2.5,
                  prop.asym == 0) %>%
    mutate(delay=factor(delay,levels = c("SARS","Wuhan"),labels = c("Short","Long"))) %>%
    dplyr::mutate(theta = factor(theta,levels=c("<1%","15%","30%"),labels = c("<1% trans. pre-onset","15% trans. pre-onset","30% trans. pre-onset"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(delay))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(delay)),size=3) +
    scale_fill_manual(guide="none",values = c("black","forestgreen")) +
    scale_color_manual(values=c("black","forestgreen"), name="Onset to\nisolation delay") + theme_cowplot() + facet_wrap(theta~.)

  delay_asym <- res %>%
    dplyr::filter(num.initial.cases==20,
                  index_R0 == 2.5,
                  theta=="15%") %>%
    mutate(delay=factor(delay,levels = c("SARS","Wuhan"),labels = c("Short","Long"))) %>%
    dplyr::mutate(theta = factor(theta,levels=c("<1%","15%","30%"),labels = c("<1% trans. pre-onset","15% trans. pre-onset","30% trans. pre-onset"))) %>%
    dplyr::mutate(prop.asym = factor(prop.asym,levels=c(0,0.1),labels = c("No asymptomatic cases ","10% cases asmyptomatic"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(delay))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(delay)),size=3) +
    scale_fill_manual(guide="none",values = c("black","forestgreen")) +
    scale_color_manual(values=c("black","forestgreen"), name="Onset to\nisolation delay") + theme_cowplot() + facet_wrap(prop.asym~.)

  delay_theta / delay_asym + plot_layout(guides = "collect") & theme(axis.title = element_text(size=12)) &
    labs(y="Simulated outbreaks controlled (%)",x="Contacts traced (%)") & scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) &
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) & panel_border()
}
