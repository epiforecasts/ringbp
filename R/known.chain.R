# Estimate propn in known chain

#' Estimate proportion in known chain
#' @author Joel Hellewell
#' @param chain_data_path
#' @param save_path
#'
#' @return
#' @export
#'
#' @examples
#'
known.chain <- function(chain_data_path = "output/chain/prop_in_chain.csv",
                      save_path = "output/plots/Knownchain.png"){
  chaindata <- read.csv(chain_data_path)
  not.chain <- as.numeric(chaindata$not.chain)
  total <- as.numeric(chaindata$not.chain+chaindata$not.contact+chaindata$known)

  date1 <- as.Date(chaindata$date)

  plot(date1,not.chain,pch=19,col=rgb(1,1,1),ylim=c(0,1), xlab = "Date", ylab = "Proportion not in known chain")
  #axis(2,at=seq(0,2500,500),labels=c(0,500,"1,000","1,500","2,000","2,500"))
  grid(ny = NULL, nx = 0, col = rgb(0.9,0.9,0.9), lty = "solid")

  for(i in 1:length(total)){
    htest <- binom.test(not.chain[i], total[i], p = 1,conf.level=0.95)
    meanA = not.chain[i]/total[i]
    conf1 = htest$conf.int[1]
    conf2 = htest$conf.int[2]

    points(date1[i], meanA, pch = 19, col = rgb(0.2,0.4,1))

    lines(c(date1[i], date1[i]), c(conf1,conf2), col = rgb(0.2,0.4,1))
  }

  dev.copy(png, save_path,
           units = "cm",width = 20,height = 15,res = 300)
  dev.off()

}

