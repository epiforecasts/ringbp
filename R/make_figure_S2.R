#' Make Figure S2
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
make_figure_S2 <- function(res){

  init_delay1 <- res %>%
    dplyr::filter(theta == "15%",
                  index_R0 == 2.5,
                  prop.asym == 0,
                  delay == "SARS") %>%
    dplyr::mutate(delay = factor(delay,levels=c("SARS","Wuhan"),labels = c("Short isolation delay","Long isolation delay"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(num.initial.cases))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(num.initial.cases)),size=3) + facet_wrap(~delay)+
    scale_fill_manual(guide="none",values = c("dodgerblue","black","dodgerblue4")) +
    scale_colour_manual(guide="none",values = c("dodgerblue","black","dodgerblue4"))+
    scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20))

  init_delay2 <- res %>%
    dplyr::filter(theta == "15%",
                  index_R0 == 2.5,
                  prop.asym == 0,
                  delay == "Wuhan") %>%
    dplyr::mutate(delay = factor(delay,levels=c("SARS","Wuhan"),labels = c("Short isolation delay","Long isolation delay"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(num.initial.cases))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(num.initial.cases)),size=3) + facet_wrap(~delay)+
    scale_fill_manual(guide="none",values = c("dodgerblue","black","dodgerblue4")) +
    scale_colour_manual(guide="none",values = c("dodgerblue","black","dodgerblue4"))+
    scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20))


  init_theta1 <- res %>%
    dplyr::filter(delay == "SARS",
                  index_R0 == 2.5,
                  prop.asym == 0,
                  theta == "<1%") %>%
    dplyr::mutate(theta = factor(theta,levels=c("<1%","15%","30%"),labels = c("<1% trans. pre-onset","15% trans. pre-onset","30% trans. pre-onset"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(num.initial.cases))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(num.initial.cases)),size=3) + facet_wrap(~theta)+
    scale_fill_manual(guide="none",values = c("dodgerblue","black","dodgerblue4")) +
    scale_colour_manual(guide="none",values = c("dodgerblue","black","dodgerblue4"))+
    scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20))

  init_theta2 <- res %>%
    dplyr::filter(delay == "SARS",
                  index_R0 == 2.5,
                  prop.asym == 0,
                  theta == "15%") %>%
    dplyr::mutate(theta = factor(theta,levels=c("<1%","15%","30%"),labels = c("<1% trans. pre-onset","15% trans. pre-onset","30% trans. pre-onset"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(num.initial.cases))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(num.initial.cases)),size=3) + facet_wrap(~theta)+
    scale_fill_manual(guide="none",values = c("dodgerblue","black","dodgerblue4")) +
    scale_colour_manual(guide="none",values = c("dodgerblue","black","dodgerblue4"))+
    scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20))

  init_theta3 <- res %>%
    dplyr::filter(delay == "SARS",
                  index_R0 == 2.5,
                  prop.asym == 0,
                  theta == "30%") %>%
    dplyr::mutate(theta = factor(theta,levels=c("<1%","15%","30%"),labels = c("<1% trans. pre-onset","15% trans. pre-onset","30% trans. pre-onset"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(num.initial.cases))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(num.initial.cases)),size=3) + facet_wrap(~theta)+
    scale_fill_manual(guide="none",values = c("dodgerblue","black","dodgerblue4")) +
    scale_colour_manual(guide="none",values = c("dodgerblue","black","dodgerblue4"))+
    scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20))



  init_asym1 <- res %>%
    dplyr::filter(delay == "SARS",
                  index_R0 == 2.5,
                  theta == "15%",
                  prop.asym == 0) %>%
    dplyr::mutate(prop.asym = factor(prop.asym,levels=c(0,0.1),labels = c("No asymptomatic cases ","10% cases asmyptomatic"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(num.initial.cases))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(num.initial.cases)),size=3) + facet_wrap(~prop.asym)+
    scale_fill_manual(guide="none",values = c("dodgerblue","black","dodgerblue4")) +
    scale_colour_manual(guide="none",values = c("dodgerblue","black","dodgerblue4"))+
    scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20))

  init_asym2 <- res %>%
    dplyr::filter(delay == "SARS",
                  index_R0 == 2.5,
                  theta == "15%",
                  prop.asym == 0.1) %>%
    dplyr::mutate(prop.asym = factor(prop.asym,levels=c(0,0.1),labels = c("No asymptomatic cases ","10% cases asmyptomatic"))) %>%
    ggplot(aes(x=control_effectiveness,y=pext,color=as.factor(num.initial.cases))) + geom_line(size=0.75) +
    geom_point(shape=21,col="black",aes(fill=as.factor(num.initial.cases)),size=3) + facet_wrap(~prop.asym) +
    scale_fill_manual(guide="none",values = c("dodgerblue","black","dodgerblue4")) +
    scale_colour_manual(values = c("dodgerblue","black","dodgerblue4"),name="Number of\ninitial cases")+
    scale_x_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20)) +
    scale_y_continuous(breaks=seq(0,1,0.2),labels=seq(0,100,20))


  pl <- ((plot_spacer() + init_delay1 + init_delay2) / (init_theta1 + init_theta2 + init_theta3) / (plot_spacer() + init_asym1 + init_asym2)) + plot_layout(guide="collect")


  pl & theme_cowplot() & panel_border() & theme(axis.title = element_blank())

}

