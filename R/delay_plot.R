delay_plot <- function(){

  delay_sars <- function(x){dgamma(x,shape=2.448898,rate = 0.639399)}
  delay_wuhan <- function(x){dweibull(x,shape=2.4,scale = 7.5)}

  out <- data.table(x=seq(0,15,0.01))
  out[,`:=`(delay_wuhan=delay_wuhan(x),delay_sars=delay_sars(x)),]
  out %<>% tidyr::gather("dist","value",-x)


  out %>% mutate(dist = factor(dist,levels=c("delay_sars","delay_wuhan"),
                               labels=c("Delay distribution towards the end of 2003 SARS outbreak",
                                        "Empirical delay distribution from Wuhan data")))


  means <- data.frame(x=c(3.83,4.9),dist=c("delay_sars","delay_wuhan")) %>%
    mutate(dist = factor(dist,levels=c("delay_sars","delay_wuhan"),
                         labels=c("Delay distribution towards the end of 2003 SARS outbreak",
                                  "Empirical delay distribution from Wuhan data")))

  out %>% ggplot(aes(x=x,y=value,col=as.factor(dist),ymin=0,ymax=value,fill=as.factor(dist))) + geom_ribbon(alpha=0.3) +
    theme_bw() +
    theme(legend.position="bottom",axis.text = element_text(size=10),axis.title = element_text(size=12),
          legend.text = element_text(size=10)) +
    xlab("Days since infection") + scale_fill_manual(name="",values=pokepal(137,2)) +
    ylab("Probability density") + scale_colour_manual(guide="none",values=pokepal(137,2)) +
    geom_vline(data=means,aes(xintercept=x),col="orange",lty=2,size=0.8)

}


