#' Multivariate plot
#'
#' @return
#' @export
#' @importFrom ggplot2 ggplot geom_point theme_bw geom_abline coord_cartesian theme scale_x_continuous scale_y_continuous xlab ylab ggtitle geom_vline coord_flip coord_cartesian
#' @importFrom ggthemes scale_color_colorblind
#' @importFrom patchwork guide_area plot_layout
#' @examples
#'
mvt_plot <- function(){

k08 <- dist_setup_mvt(mu_ip=5.8,mu_si=7.5,sd_ip=2.6,sd_si=3.4,k=0.95)

k04 <- dist_setup_mvt(mu_ip=5.8,mu_si=7.5,sd_ip=2.6,sd_si=3.4,k=0.05)

tab <- cbind(rbind(k08(100),k04(100)),c(rep(0.05,100),rep(0.95,100)))
colnames(tab) <- c("ip","si","k")

p1 <- as.data.frame(tab) %>%
  ggplot(aes(x=ip,y=si,col=as.factor(k))) + geom_point() + theme_bw() +
  geom_abline(intercept = 0,slope=1) + scale_color_colorblind(name="Correlation parameter (k)") +
  coord_cartesian(xlim=c(0,13),ylim=c(0,16)) + theme(legend.position="bottom",axis.title=element_blank()) +
  scale_x_continuous(position="top") +
  scale_y_continuous(position="right")


p2 <- data.frame(x=seq(0,13,0.1),y=dnorm(x = seq(0,13,0.1),mean=5.8,sd = 2.6)) %>%
  ggplot(aes(x=x,y=y)) + geom_line() + xlab("") + ylab("") + theme_bw() + geom_vline(xintercept=5.8,lty=2) +
  theme(axis.title=element_blank(),plot.title = element_text(hjust = 0.5),
        axis.text=element_blank()) + ggtitle("Incubation Period")

p3 <- data.frame(x=seq(0,16,0.1),y=dnorm(x = seq(0,16,0.1),mean=7.5,sd = 3.4)) %>%
  ggplot(aes(x=x,y=y)) + geom_line() + xlab("Serial Interval") + ylab("") + theme_bw() + geom_vline(xintercept=7.5,lty=2) +
  coord_flip() + theme(axis.title.x=element_blank(),axis.title.y=element_text(size=14),
                       axis.text=element_blank(),axis.ticks = element_blank()) +
  scale_x_continuous(position="left")

p4 <- delay_plot() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Delay from onset to isolation")

layout <- "
DDDDBBB#
DDDDAAAC
DDDDAAAC
DDDDAAAC
EEEEEEEE
"
p1 + p2 + p3 + p4 + guide_area() +  plot_layout(design=layout,guides="collect")

}
