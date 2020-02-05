#' Multivariate plot
#'
#' @return
#' @export
#' @importFrom ggplot2 ggplot geom_point theme_bw geom_abline coord_cartesian theme
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous xlab ylab ggtitle geom_vline
#' @importFrom ggplot2 coord_flip coord_cartesian element_blank element_text geom_hline geom_density
#' @importFrom ggthemes scale_color_colorblind scale_fill_colorblind
#' @importFrom patchwork guide_area plot_layout
#' @examples
#'
#'\dontrun{
#'mvt_plot()
#'}
mvt_plot <- function(){

  tab <- data.frame(inc = rweibull(n = 200,shape = 2.322737,scale = 6.492272))
  tab$inc <- ifelse(tab$inc<1,1,tab$inc)
  tab$si <- c(sn::rsn(n=100,xi = tab$inc[1:100],omega = 1,alpha = 0.5),sn::rsn(n=100,xi = tab$inc[101:200],omega = 1,alpha = 3))
  tab$si <- ifelse(tab$si<1,1,tab$si)
  tab$skew <- c(rep(0.5,100),rep(3,100))

  tab2 <- tab %>% group_by(skew) %>% summarise(mean=mean(si))

  p1 <- tab %>% ggplot(aes(x=inc,y=si,col=as.factor(skew))) + geom_point() +
    geom_abline(slope=1,intercept=0) + coord_cartesian(ylim=c(0,16),xlim=c(0,13)) +
    scale_color_colorblind(name="Proportion of transmission \n before symptom onset",labels=c("35%","12%")) + theme_bw() +
    coord_cartesian(xlim=c(0,13),ylim=c(0,16)) + theme(legend.position="bottom",axis.title=element_blank()) +
    scale_x_continuous(position="top") +
    scale_y_continuous(position="right") +
    geom_hline(data=tab2,aes(yintercept=mean,col=as.factor(skew)),lty=2) +
    geom_vline(aes(xintercept=5.8),lty=2)

  p2 <- data.frame(x=seq(0,15,0.1),y=dweibull(x=seq(0,15,0.1),shape = 2.322737,scale = 6.492272)) %>% ggplot(aes(x=x,y=y)) + geom_line() +
    xlab("") + ylab("") + theme_bw() + geom_vline(xintercept=5.8,lty=2) + coord_cartesian(xlim=c(0,13)) +
    theme(axis.title=element_blank(),plot.title = element_text(hjust = 0.5),
          axis.text=element_blank(),axis.ticks.y=element_blank()) + ggtitle("Incubation Period")

  p3 <- tab %>% ggplot(aes(x=si,fill=as.factor(skew))) + geom_density(alpha=0.4) + theme_bw() + coord_flip(xlim=c(0,16)) +
    theme(axis.title.x=element_blank(),axis.title.y=element_text(size=14),
          axis.text=element_blank(),axis.ticks = element_blank()) +
    scale_x_continuous(position="left") + xlab("Serial Interval") + scale_fill_colorblind(name="Skew parameter",guide="none") +
    geom_vline(data=tab2,aes(xintercept=mean,col=as.factor(skew)),lty=2) + scale_color_colorblind(guide="none")

  p4 <- delay_plot() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Delay from onset to isolation")


  layout <- "AAAACCC#
             AAAABBBD
             AAAABBBD
             AAAABBBD
             EEEEEEEE"

  ( p4 + p1 + p2 + p3) + patchwork::guide_area() +  plot_layout(design=layout,guides="collect")

}
