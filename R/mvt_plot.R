#' Multivariate plot
#'
#' @return
#' @export
#' @importFrom ggplot2 ggplot geom_point theme_bw geom_abline coord_cartesian theme scale_fill_manual
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

  p2 <- data.frame(x=seq(0,15,0.1),y=dweibull(x=seq(0,15,0.1),shape = 2.322737,scale = 6.492272)) %>%
    ggplot(aes(x=x,y=y)) + geom_line() + cowplot::theme_cowplot() + geom_vline(xintercept=5.8,lty=2) + coord_cartesian(xlim=c(0,13)) +
    labs(tag = "B", x = "time since infection (days)",y = "probability density") +
    geom_ribbon(aes(ymax=y,ymin=0),fill="chartreuse2",alpha=0.4)


  p3 <- data.frame(y=c(sn::dsn(x=seq(0,13,0.1),xi = 5,omega = 2,alpha = 0.7),
                       sn::dsn(x=seq(0,13,0.1),xi = 5,omega = 2,alpha = 1.95),
                       sn::dsn(x=seq(0,13,0.1),xi = 5,omega = 2,alpha = 30)),
                   x = rep(rep(seq(0,13,0.1),3)),
                   theta= rep(c("30%","15%","<1%"),rep(131,3))) %>% ggplot(aes(x,y,fill=theta)) +
    geom_ribbon(aes(ymin=0,ymax=y),alpha=0.4) +
    geom_line(aes(x,y)) +
    cowplot::theme_cowplot() + coord_cartesian(xlim=c(0,13)) +
    geom_vline(xintercept = 5) + theme(legend.position = "bottom") +
    scale_fill_manual(values=c("grey65","goldenrod3","orchid"),name="Proportion of\ntransmission\nbefore symptoms") +
    labs(tag = "C", x = "time since infection (days)",y = "probability density")


  (delay_plot() | (p2 / p3)) & theme(axis.text = element_text(size=10),
                                     legend.title = element_text(size=11),
                                     axis.title = element_text(size=11))

}
