make_figure_S1 <- function(res){
  r0_init <- res %>%
    dplyr::filter(theta == "15%",
                  delay == "SARS",
                  prop.asym==0) %>%
    dplyr::mutate(num.initial.cases = factor(num.initial.cases,levels=c(5,20,40),labels = c("5 cases","20 cases","40 cases"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(index_R0))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(index_R0)),size=3) + facet_wrap(.~num.initial.cases ) +
    scale_fill_manual(guide="none",values = c("firebrick","black","firebrick3")) +
    scale_color_manual(values = c("firebrick","black","firebrick3"),name="Reproduction\nnumber")  + theme_cowplot() +
    ylab("Simulated outbreaks controlled (%)") +
    xlab("Contacts traced (%)") + scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) + panel_border()

  r0_delay <- res %>%
    dplyr::filter(theta == "15%",
                  num.initial.cases==20,
                  prop.asym==0) %>%
    dplyr::mutate(delay = factor(delay,levels=c("SARS","Wuhan"),labels = c("Short isolation delay","Long isolation delay"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(index_R0))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(index_R0)),size=3) + facet_wrap(.~delay ) +
    scale_fill_manual(guide="none",values = c("firebrick","black","firebrick3")) +
    scale_color_manual(values = c("firebrick","black","firebrick3"),name="Reproduction\nnumber")  + theme_cowplot() +
    ylab("Simulated outbreaks controlled (%)") +
    xlab("Contacts traced (%)") + scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) + panel_border()

  r0_theta <- res %>%
    dplyr::filter(delay == "SARS",
                  num.initial.cases==20,
                  prop.asym==0) %>%
    dplyr::mutate(theta = factor(theta,levels=c("<1%","15%","30%"),labels = c("<1% trans. pre-onset","15% trans. pre-onset","30% trans. pre-onset"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(index_R0))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(index_R0)),size=3) + facet_wrap(.~theta ) +
    scale_fill_manual(guide="none",values = c("firebrick","black","firebrick3")) +
    scale_color_manual(values = c("firebrick","black","firebrick3"),name="Reproduction\nnumber")  + theme_cowplot() +
    ylab("Simulated outbreaks controlled (%)") +
    xlab("Contacts traced (%)") + scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) + panel_border()

  r0_asym <- res %>%
    dplyr::filter(delay == "SARS",
                  num.initial.cases==20,
                  theta == "15%") %>%
    dplyr::mutate(prop.asym = factor(prop.asym,levels=c(0,0.1),labels = c("No asymptomatic cases ","10% cases asmyptomatic"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(index_R0))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(index_R0)),size=3) + facet_wrap(.~prop.asym ) +
    scale_fill_manual(guide="none",values = c("firebrick","black","firebrick3")) +
    scale_color_manual(values = c("firebrick","black","firebrick3"),name="Reproduction\nnumber")  + theme_cowplot() +
    ylab("Simulated outbreaks controlled (%)") +
    xlab("Contacts traced (%)") + scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) + panel_border()

  r0_init / r0_delay / r0_theta / r0_asym + plot_layout(guides = "collect") & theme(axis.title = element_text(size=10.5))
}
