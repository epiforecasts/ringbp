make_figure_S2 <- function(res){
  init_delay <- res %>%
    dplyr::filter(theta == "15%",
                  index_R0 == 2.5,
                  prop.asym == 0) %>%
    dplyr::mutate(delay = factor(delay,levels=c("SARS","Wuhan"),labels = c("Short isolation delay","Long isolation delay"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(num.initial.cases))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(num.initial.cases)),size=3) +
    scale_fill_manual(guide="none",values = c("dodgerblue","black","dodgerblue3")) +
    scale_color_manual(values=c("dodgerblue","black","dodgerblue3"),name="Number of\ninitial cases")  + theme_cowplot() + facet_wrap(~delay)

  init_theta <- res %>%
    dplyr::filter(delay == "SARS",
                  index_R0 == 2.5,
                  prop.asym == 0) %>%
    dplyr::mutate(theta = factor(theta,levels=c("<1%","15%","30%"),labels = c("<1% trans. pre-onset","15% trans. pre-onset","30% trans. pre-onset"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(num.initial.cases))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(num.initial.cases)),size=3) +
    scale_fill_manual(guide="none",values = c("dodgerblue","black","dodgerblue3")) +
    scale_color_manual(values=c("dodgerblue","black","dodgerblue3"),name="Number of\ninitial cases")  + theme_cowplot()  + facet_wrap(~theta)

  init_asym <- res %>%
    dplyr::filter(delay == "SARS",
                  index_R0 == 2.5,
                  theta == "15%") %>%
    dplyr::mutate(prop.asym = factor(prop.asym,levels=c(0,0.1),labels = c("No asymptomatic cases ","10% cases asmyptomatic"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(num.initial.cases))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(num.initial.cases)),size=3) +
    scale_fill_manual(guide="none",values = c("dodgerblue","black","dodgerblue3")) +
    scale_color_manual(values=c("dodgerblue","black","dodgerblue3"),name="Number of\ninitial cases")  + theme_cowplot()  + facet_wrap(~prop.asym)

  init_delay / init_theta / init_asym + plot_layout(guides = "collect") & theme(axis.title = element_text(size=12)) &
    labs(y="Simulated outbreaks controlled (%)",x="Contacts traced (%)") & scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) &
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) & panel_border()
}
