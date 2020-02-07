#' Make Figure S1
#'
#' @param res
#'
#' @return
#' @export
#' @importFrom dplyr filter mutate
#' @import ggplot2
#' @import patchwork
#' @import cowplot
#'
#' @examples
#'
make_figure_S1 <- function(res){

  r0_init1 <- res %>%
    dplyr::filter(theta == "15%",
                  delay == "SARS",
                  prop.asym==0,
                  num.initial.cases==5) %>%
    dplyr::mutate(num.initial.cases = factor(num.initial.cases,levels=c(5,20,40),labels = c("5 cases","20 cases","40 cases"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(index_R0))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(index_R0)),size=3) + facet_wrap(.~num.initial.cases ) +
    # scale_fill_manual(guide="none",values = c("firebrick","black","firebrick3")) +
    # scale_color_manual(values = c("firebrick","black","firebrick3"),name="Reproduction\nnumber")  + theme_cowplot() +
    ylab("Simulated outbreaks controlled (%)") +
    xlab("Contacts traced (%)") + scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) + panel_border()

  r0_init2 <- res %>%
    dplyr::filter(theta == "15%",
                  delay == "SARS",
                  prop.asym==0,
                  num.initial.cases==20) %>%
    dplyr::mutate(num.initial.cases = factor(num.initial.cases,levels=c(5,20,40),labels = c("5 cases","20 cases","40 cases"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(index_R0))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(index_R0)),size=3) + facet_wrap(.~num.initial.cases ) +
    # scale_fill_manual(guide="none",values = c("firebrick","black","firebrick3")) +
    # scale_color_manual(values = c("firebrick","black","firebrick3"),name="Reproduction\nnumber")  + theme_cowplot() +
    ylab("Simulated outbreaks controlled (%)") +
    xlab("Contacts traced (%)") + scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) + panel_border()

  r0_init3 <- res %>%
    dplyr::filter(theta == "15%",
                  delay == "SARS",
                  prop.asym==0,
                  num.initial.cases==40) %>%
    dplyr::mutate(num.initial.cases = factor(num.initial.cases,levels=c(5,20,40),labels = c("5 cases","20 cases","40 cases"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(index_R0))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(index_R0)),size=3) + facet_wrap(.~num.initial.cases ) +
    # scale_fill_manual(guide="none",values = c("firebrick","black","firebrick3")) +
    # scale_color_manual(values = c("firebrick","black","firebrick3"),name="Reproduction\nnumber")  + theme_cowplot() +
    ylab("Simulated outbreaks controlled (%)") +
    xlab("Contacts traced (%)") + scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) + panel_border()

  r0_delay1 <- res %>%
    dplyr::filter(theta == "15%",
                  num.initial.cases==20,
                  prop.asym==0,
                  delay == "SARS") %>%
    dplyr::mutate(delay = factor(delay,levels=c("SARS","Wuhan"),labels = c("Short isolation delay","Long isolation delay"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(index_R0))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(index_R0)),size=3) + facet_wrap(.~delay ) +
    # scale_fill_manual(guide="none",values = c("firebrick","black","firebrick3")) +
    # scale_color_manual(values = c("firebrick","black","firebrick3"),name="Reproduction\nnumber")  + theme_cowplot() +
    ylab("Simulated outbreaks controlled (%)") +
    xlab("Contacts traced (%)") + scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) + panel_border()

  r0_delay2 <- res %>%
    dplyr::filter(theta == "15%",
                  num.initial.cases==20,
                  prop.asym==0,
                  delay=="Wuhan") %>%
    dplyr::mutate(delay = factor(delay,levels=c("SARS","Wuhan"),labels = c("Short isolation delay","Long isolation delay"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(index_R0))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(index_R0)),size=3) + facet_wrap(.~delay ) +
    # scale_fill_manual(guide="none",values = c("firebrick","black","firebrick3")) +
    # scale_color_manual(values = c("firebrick","black","firebrick3"),name="Reproduction\nnumber")  + theme_cowplot() +
    ylab("Simulated outbreaks controlled (%)") +
    xlab("Contacts traced (%)") + scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) + panel_border()

  r0_theta1 <- res %>%
    dplyr::filter(delay == "SARS",
                  num.initial.cases==20,
                  prop.asym==0,
                  theta=="<1%") %>%
    dplyr::mutate(theta = factor(theta,levels=c("<1%","15%","30%"),labels = c("<1% trans. pre-onset","15% trans. pre-onset","30% trans. pre-onset"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(index_R0))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(index_R0)),size=3) + facet_wrap(.~theta ) +
    # scale_fill_manual(guide="none",values = c("firebrick","black","firebrick3")) +
    # scale_color_manual(values = c("firebrick","black","firebrick3"),name="Reproduction\nnumber")  + theme_cowplot() +
    ylab("Simulated outbreaks controlled (%)") +
    xlab("Contacts traced (%)") + scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) + panel_border()

  r0_theta2 <- res %>%
    dplyr::filter(delay == "SARS",
                  num.initial.cases==20,
                  prop.asym==0,
                  theta=="15%") %>%
    dplyr::mutate(theta = factor(theta,levels=c("<1%","15%","30%"),labels = c("<1% trans. pre-onset","15% trans. pre-onset","30% trans. pre-onset"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(index_R0))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(index_R0)),size=3) + facet_wrap(.~theta ) +
    # scale_fill_manual(guide="none",values = c("firebrick","black","firebrick3")) +
    # scale_color_manual(values = c("firebrick","black","firebrick3"),name="Reproduction\nnumber")  + theme_cowplot() +
    ylab("Simulated outbreaks controlled (%)") +
    xlab("Contacts traced (%)") + scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) + panel_border()

  r0_theta3 <- res %>%
    dplyr::filter(delay == "SARS",
                  num.initial.cases==20,
                  prop.asym==0,
                  theta=="30%") %>%
    dplyr::mutate(theta = factor(theta,levels=c("<1%","15%","30%"),labels = c("<1% trans. pre-onset","15% trans. pre-onset","30% trans. pre-onset"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(index_R0))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(index_R0)),size=3) + facet_wrap(.~theta ) +
    # scale_fill_manual(guide="none",values = c("firebrick","black","firebrick3")) +
    # scale_color_manual(values = c("firebrick","black","firebrick3"),name="Reproduction\nnumber")  + theme_cowplot() +
    ylab("Simulated outbreaks controlled (%)") +
    xlab("Contacts traced (%)") + scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) + panel_border()


  r0_asym1 <- res %>%
    dplyr::filter(delay == "SARS",
                  num.initial.cases==20,
                  theta == "15%",
                  prop.asym ==0) %>%
    dplyr::mutate(prop.asym = factor(prop.asym,levels=c(0,0.1),labels = c("No asymptomatic cases ","10% cases asmyptomatic"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(index_R0))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(index_R0)),size=3) + facet_wrap(.~prop.asym ) +
    # scale_fill_manual(guide="none",values = c("firebrick","black","firebrick3")) +
    # scale_color_manual(values = c("firebrick","black","firebrick3"),name="Reproduction\nnumber")  +
    # theme_cowplot() +
    ylab("Simulated outbreaks controlled (%)") +
    xlab("Contacts traced (%)") + scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) + panel_border()

  r0_asym2 <- res %>%
    dplyr::filter(delay == "SARS",
                  num.initial.cases==20,
                  theta == "15%",
                  prop.asym ==0.1) %>%
    dplyr::mutate(prop.asym = factor(prop.asym,levels=c(0,0.1),labels = c("No asymptomatic cases ","10% cases asmyptomatic"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(index_R0))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(index_R0)),size=3) + facet_wrap(.~prop.asym ) +
    # scale_fill_manual(guide="none",values = c("firebrick","black","firebrick3")) +
    # scale_color_manual(values = c("firebrick","black","dodgerblue3"),name="Reproduction\nnumber")  +
    # theme_cowplot() +
    ylab("Simulated outbreaks controlled (%)") +
    xlab("Contacts traced (%)") + scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) + panel_border()


 (r0_init1 + r0_init2 + r0_init3) /
    (plot_spacer() + r0_delay1 + r0_delay2) /
    (r0_theta1 + r0_theta2 + r0_theta3) /
    (plot_spacer() + r0_asym1 + r0_asym2) + plot_layout(guides="collect") & theme_cowplot() & theme(axis.title = element_blank()) &
    scale_fill_manual(guide="none",values = c("red","black","firebrick4")) &
    scale_color_manual(values = c("red","black","firebrick4"),name="Reproduction\nnumber")

}
