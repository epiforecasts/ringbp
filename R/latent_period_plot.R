latent_period_plot <- function(){

incubfn <- function(x){dweibull(x,shape = 2.3,scale=6.5)}
infecfn_early <- function(x){dweibull(x,shape=2.0,scale=5.9)}
infecfn_medium <- function(x){dweibull(x,shape=2.6,scale = 7.1)}
infecfn_late <- function(x){dweibull(x,shape=3.3,scale = 8.7)}
infecfn_very_late <- function(x){dweibull(x,shape=3.8,scale = 9.7)}

# delay_early <- function(x){dweibull(x,shape=1.8,scale=3.5)}
# delay_medium <- function(x){dweibull(x,shape=2,scale = 5.5)}
# delay_late <- function(x){dweibull(x,shape=2.4,scale = 7.5)}

out <- data.table(x=seq(0,15,0.01))
out[,`:=`(latent_early=infecfn_early(x),latent_late=infecfn_late(x),
          latent_medium=infecfn_medium(x),latent_very_late=infecfn_very_late(x)),]
out %<>% tidyr::gather("dist","value",-x)

inc_out <- data.table(x2=seq(0,15,0.01))[,`:=`(value2=incubfn(x2)),]


inc_line <- data.frame(x=4.8)

inf_line <- data.frame(x=c(7.19,5),dist=c("latent_late","latent_early")) %>%
  mutate(dist=forcats::fct_recode(dist,`Short latent period`="latent_early",
                                  `Long latent period`="latent_late"))

out %>% mutate(dist=forcats::fct_recode(dist,`Short latent period`="latent_early",
                                        `Medium latent period`="latent_medium",
                                        `Late latent period`="latent_late",
                                        `Very late latent period`="latent_very_late")) %>%
  ggplot(aes(x=x,ymax=value,ymin=0,fill=as.factor(dist))) +
  geom_ribbon(data=inc_out,inherit.aes = FALSE,aes(x=x2,ymax=value2,ymin=0),alpha=0.5) +
  geom_ribbon(alpha=0.6) + facet_wrap(~dist) + theme_bw() + xlab("Days since infection") +
  ylab("Probability density") + scale_fill_discrete(guide="none")

}

