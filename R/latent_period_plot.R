latent_period_plot <- function(){

incubfn <- function(x){dweibull(x,shape = 2.3,scale=6.5)}
infecfn_early <- function(x){dweibull(x,shape=4.174,scale=10.64)}
infecfn_medium <- function(x){dweibull(x,shape=9,scale = 23.0)}
infecfn_late <- function(x){dweibull(x,shape=3.1,scale = 8.6)}



calc_theta(R0 = 3,inc_shape = 2.3,inc_scale = 6.5,
           inf_shape = inf_shape, inf_scale = inf_scale)

# delay_early <- function(x){dweibull(x,shape=1.8,scale=3.5)}
# delay_medium <- function(x){dweibull(x,shape=2,scale = 5.5)}
# delay_late <- function(x){dweibull(x,shape=2.4,scale = 7.5)}

out <- data.table(x=seq(0,30,0.01))
out[,`:=`(latent_early=infecfn_early(x),latent_late=infecfn_late(x),
          latent_medium=infecfn_medium(x)),]
out %<>% tidyr::gather("dist","value",-x)

inc_out <- data.table(x2=seq(0,30,0.01))[,`:=`(value2=incubfn(x2)),]


# inc_line <- data.frame(x=4.8)
#
# inf_line <- data.frame(x=c(7.19,5),dist=c("latent_late","latent_early")) %>%
#   mutate(dist=forcats::fct_recode(dist,`Short latent period`="latent_early",
#                                   `Long latent period`="latent_late"))\

value1 <- "15%"
value2 <- "0%"
value3 <- "30%"
mylabs <- list(bquote(theta==.(value1)),bquote(theta==.(value2)),bquote(theta==.(value3)))
cols <- c("darkgoldenrod3","deepskyblue4","brown1")
out %>% mutate(dist=factor(dist,levels=c("latent_early","latent_medium","latent_late"),
                             labels=c(expression(paste(theta, " = 0%)")),
                                      expression(paste(theta, " = 15%)")),
                                      expression(paste(theta, " = 30%)"))))) %>%
  ggplot(aes(x=x,y=value,col=as.factor(dist))) +
  geom_ribbon(data=inc_out,inherit.aes = FALSE,aes(x=x2,ymax=value2,ymin=0),alpha=0.7) +
  geom_line(alpha=0.6)  + theme_bw() + xlab("Days since infection") +
  ylab("Probability density") + scale_colour_manual(guide="none",values=cols) +
  geom_ribbon(aes(ymin=0,ymax=value,fill=as.factor(dist)),alpha=0.3) +
  scale_fill_manual(labels=mylabs,name="Percentage of transmission pre-symptoms",values=cols) +
  theme(legend.position="bottom",axis.text = element_text(size=10),axis.title = element_text(size=12),
        legend.text = element_text(size=10))




}

