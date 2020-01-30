delay_plot <- function(){


# calc_theta(R0=1,inc_shape = 2.3,inc_scale = 6.5,inf_shape = 3.8,inf_scale = 9.7)

# incubfn <- function(x){dweibull(x,shape = 2.3,scale=6.5)}
# infecfn_early <- function(x){dweibull(x,shape=2.0,scale=5.9)}
# infecfn_medium <- function(x){dweibull(x,shape=2.6,scale = 7.1)}
# infecfn_late <- function(x){dweibull(x,shape=3.3,scale = 8.7)}
# infecfn_very_late <- function(x){dweibull(x,shape=3.8,scale = 9.7)}

delay_early <- function(x){dweibull(x,shape=1.8,scale=3.5)}
delay_medium <- function(x){dweibull(x,shape=2,scale = 5.5)}
delay_late <- function(x){dweibull(x,shape=2.4,scale = 7.5)}

out <- data.table(x=seq(0,15,0.01))
out[,`:=`(delay_early=delay_early(x),delay_late=delay_late(x),
          delay_medium=delay_medium(x)),]
out %<>% tidyr::gather("dist","value",-x)


means <- data.frame(x=c(3.1,4.9,6.6),dist=c("delay_early","delay_medium","delay_late")) %>%
  mutate(dist=factor(dist,levels=c("delay_early","delay_medium","delay_late"))) %>%
  mutate(dist=forcats::fct_recode(dist,`Short delay from onset to isolation`="delay_early",
                                  `Medium delay from onset to isolation`="delay_medium",
                                  `Long delay from onset to isolation`="delay_late"))

out %>% mutate(dist=factor(dist,levels=c("delay_early","delay_medium","delay_late"))) %>%
  mutate(dist=forcats::fct_recode(dist,`Short delay from onset to isolation`="delay_early",
                                        `Medium delay from onset to isolation`="delay_medium",
                                        `Long delay from onset to isolation`="delay_late")) %>%
  ggplot(aes(x=x,ymax=value,ymin=0,fill=as.factor(dist))) +
  geom_line(aes(y=value,col=as.factor(dist))) +
  geom_ribbon(alpha=0.3) + facet_wrap(~dist) + theme_bw() + xlab("Days since onset of symptoms") +
  ylab("Probability density") +
  scale_fill_colorblind(guide="none") +
  scale_color_colorblind(guide="none") +
  geom_vline(data=means,aes(xintercept=x),lty=2,colour="red",size=0.8) +
  theme(plot.title = element_text(hjust = 0.5),strip.text.x = element_text(size=11),
        strip.text.y = element_text(size=9),axis.text = element_text(size=11),
        axis.title = element_text(size=12))

}


