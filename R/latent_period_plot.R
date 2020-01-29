latent_period_plot <- function(){

  incubfn <- function(x){dweibull(x,shape = 2.322737,scale=6.492272)}
  infecfn_late <- function(x){dweibull(x,shape=2.622737,scale = 7.492272)}
  infecfn_early <- function(x){dweibull(x,shape=2.022737,scale=5.492272)}


  out <- data.table(x=seq(0,15,0.01))
  out[,`:=`(latent_early=infecfn_early(x),latent_late=infecfn_late(x)),]
  out %<>% tidyr::gather("dist","value",-x)

  inc_out <- data.table(x2=seq(0,15,0.01))[,`:=`(value2=incubfn(x2)),]


  inc_line <- data.frame(x=4.8)

  inf_line <- data.frame(x=c(7.19,5),dist=c("latent_late","latent_early")) %>%
    mutate(dist=forcats::fct_recode(dist,`Short latent period`="latent_early",
                                    `Long latent period`="latent_late"))

  out %>% mutate(dist=forcats::fct_recode(dist,`Short latent period`="latent_early",
                                          `Long latent period`="latent_late")) %>%
    ggplot(aes(x=x,ymax=value,ymin=0,fill=as.factor(dist))) +
    geom_ribbon(data=inc_out,inherit.aes = FALSE,aes(x=x2,ymax=value2,ymin=0),alpha=0.5) +
    geom_ribbon(alpha=0.6) + facet_wrap(~dist) + theme_bw() + xlab("Days since infection") +
    ylab("Probability density") + scale_fill_discrete(guide="none")

}

