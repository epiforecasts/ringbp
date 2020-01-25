#' Produces first figure from summary
#'
#' @return
#' @export
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot geom_ribbon xlab theme_bw aes
#' @examples
#'
transmission_figure <- function(){
  incub_param <- c(6,2)
  incub <- function(x){
    pgamma(x,shape=incub_param[1]/(incub_param[2]^2/incub_param[1]),
           scale=(incub_param[2]^2/incub_param[1]))
  }

  infect_param <- c(6,2)
  infect <- function(x){
    dgamma(x,shape=infect_param[1]/(infect_param[2]^2/infect_param[1]),
           scale=(infect_param[2]^2/infect_param[1]))
  }

  delay_param <- c(8,2)
  delayf <- function(x){
    pgamma(x,shape=delay_param[1]/(delay_param[2]^2/delay_param[1]),
           scale=(delay_param[2]^2/delay_param[1]))
  }

  dists <- data.frame(delay=delayf(seq(0,15,0.1)),infect=infect(seq(0,15,0.1)),incub=incub(seq(0,15,0.1)),x=seq(0,15,0.1)) %>%
    mutate(pre_symp_trans=(1-incub)*infect,post_symp_trans=infect-pre_symp_trans)

  dists %>%# full_join(trans_prevented_by_iso,by="x") %>%
    ggplot(aes(x=x)) + xlab("days since infection") +
    geom_ribbon(aes(ymax=infect,ymin=pre_symp_trans+post_symp_trans*(1-delay)),fill="green4") +
    geom_ribbon(aes(ymax=pre_symp_trans,ymin=0),fill="blue") +
    geom_ribbon(aes(ymax=pre_symp_trans+post_symp_trans*(1-delay),ymin=pre_symp_trans),fill="red") +
    theme_bw()
}

