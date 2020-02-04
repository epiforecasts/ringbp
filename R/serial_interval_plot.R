#' Constructs plot of serial intervals for different skew values
#' @author Joel Hellewell
#' @return
#' @export
#'
#' @examples
#'
serial_interval_plot <- function(){

  inf_fn <- function(inc_samp,k){
    out <- sn::rsn(n=length(inc_samp),xi = inc_samp,omega = 2,alpha = k)
    out <- ifelse(out<1,1,out)
    return(out)
  }

  incfn <- dist_setup(dist_shape = 2.322737,dist_scale = 6.492272)

  inc_df <- data.table(x=seq(0,20,0.01),y=dweibull(seq(0,20,0.01),shape=2.322737,scale=6.492272))

  inc_samp <- incfn(100000)
  # 30, 1.95, 0.7

  tab <- rbindlist(list(data.table(samp=inf_fn(inc_samp,30),k=30),
                        data.table(samp=inf_fn(inc_samp,1.95),k=1.95),
                        data.table(samp=inf_fn(inc_samp,0.7),k=0.7)))

  tab <- tab %>% mutate(theta=factor(k,levels = c(30,1.95,0.7),
                                     labels = c("<1%","15%","30%")))

  tab_sm <- tab %>% group_by(theta) %>% summarise(mean=mean(samp))

  tab %>% ggplot(aes(x=samp,fill=as.factor(theta),col=as.factor(theta))) + geom_density(alpha=0.2) + theme_bw() +
    geom_vline(data=tab_sm,aes(xintercept=mean,col=as.factor(theta)),lty=2) +
    scale_x_continuous(breaks=seq(0,20,2)) + coord_cartesian(xlim=c(0,20)) +
    xlab("Days since exposure") + ylab("Probability density") +
    scale_fill_discrete(name="Proportion of transmission that occurs before symptom onset") +
    scale_color_discrete(guide="none") +
    theme(legend.position = "bottom")

}
