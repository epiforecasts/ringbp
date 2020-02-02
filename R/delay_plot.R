#' Plots delay distributions
#' @author Joel Hellewell
#'
#' @return
#' @export
#' @importFrom data.table data.table
#' @importFrom tidyr gather
#' @importFrom magrittr %<>%
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot geom_ribbon theme_bw theme xlab ylab geom_line geom_vline scale_colour_discrete scale_fill_discrete
#' @examples
#'
delay_plot <- function(){

  delay_sars <- function(x){dweibull(x,shape=1.651524,scale=4.287786)} # Weibull estimation of dgamma(x,shape=2.448898,rate = 0.639399)
  delay_wuhan <- function(x){dweibull(x,shape=2.305172,scale = 9.483875)}

  out <- data.table(x=seq(0,15,0.01))
  out[,`:=`(delay_wuhan=delay_wuhan(x),delay_sars=delay_sars(x)),]
  out %<>% tidyr::gather("dist","value",-x)


  out %>% mutate(dist = factor(dist,levels=c("delay_sars","delay_wuhan"),
                               labels=c("Delay distribution towards the end of 2003 SARS outbreak",
                                        "Empirical delay distribution from Wuhan data")))


  means <- data.frame(x=c(3.83,9.1),dist=c("delay_sars","delay_wuhan")) %>%
    mutate(dist = factor(dist,levels=c("delay_sars","delay_wuhan"),
                         labels=c("Delay distribution towards the end of 2003 SARS outbreak",
                                  "Empirical delay distribution from Wuhan data")))

  out %>% ggplot(aes(x=x,y=value,col=as.factor(dist),ymin=0,ymax=value,fill=as.factor(dist))) + geom_ribbon(alpha=0.3) +
    theme_bw() +
    theme(legend.position="bottom",axis.text = element_text(size=10),axis.title = element_text(size=12),
          legend.text = element_text(size=10)) +
    xlab("Days since infection") + scale_fill_discrete(name="",labels=c("SARS outbreak 2003","Wuhan nCoV 2019")) +
    ylab("Probability density") + scale_colour_discrete(guide="none") +
    geom_vline(data=means,aes(xintercept=x),col="orange",lty=2,size=0.8)

}


