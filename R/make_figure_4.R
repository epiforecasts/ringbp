make_figure_4 <- function(df){

  f4p1 <- res %>%
    dplyr::filter(delay == "SARS",
                  theta == "15%",
                  index_R0 == 2.5,
                  prop.asym == 0) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(num.initial.cases))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(num.initial.cases)),size=3) +
    scale_fill_manual(guide="none",values = c("dodgerblue","black","dodgerblue3")) +
    scale_color_manual(values=c("dodgerblue","black","dodgerblue3"),name="Number of\ninitial cases")  + theme_cowplot()

  f4p2 <- res %>%
    dplyr::filter(num.initial.cases==20,
                  theta == "15%",
                  index_R0 == 2.5,
                  prop.asym == 0) %>%
    mutate(delay=factor(delay,levels = c("SARS","Wuhan"),labels = c("Short","Long"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(delay))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(delay)),size=3) +
    scale_fill_manual(guide="none",values = c("black","forestgreen")) +
    scale_color_manual(values=c("black","forestgreen"), name="Onset to\nisolation delay") + theme_cowplot()

  f4p3 <- res %>%
    dplyr::filter(num.initial.cases==20,
                  delay == "SARS",
                  index_R0 == 2.5,
                  prop.asym == 0) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(theta))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(theta)),size=3) +
    scale_fill_manual(guide="none",values = c("mediumpurple2","black","mediumpurple4")) +
    scale_color_manual(values=c("mediumpurple2","black","mediumpurple4"), name = "Percentage of\ntransmission\nbefore symptoms") +
    theme_cowplot()

  f4p4 <- res %>%
    dplyr::filter(delay == "SARS",
                  theta == "15%",
                  index_R0 == 2.5,
                  num.initial.cases==20) %>%
    dplyr::mutate(prop.asym = factor(prop.asym,levels = c(0,0.1),labels = c("0%","10%"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(prop.asym))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(prop.asym)),size=3) +
    scale_fill_manual(guide="none",values = c("black","chocolate4")) +
    scale_color_manual(values=c("black","chocolate4"),name="Proportion of\nasymptomatic\ncases")  + theme_cowplot()

  fig4 <- (f4p1 + f4p2) / (f4p3 + f4p4) + plot_annotation(tag_levels = "A") &
    theme(legend.position = "right",
          plot.tag.position = "topright",
          plot.tag = element_text(size=20,face="bold"),
          axis.text = element_text(size=11),
          axis.title = element_text(size=11),
          legend.title = element_text(size=11),
          legend.text = element_text(size=11)) & ylab("Simulated outbreaks controlled (%)") &
    xlab("Contacts traced (%)") & scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) &
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20))

  return(fig4)
}

