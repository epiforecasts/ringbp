# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# SET UP DISTRIBUTIONS FOR PARAMETERS
# Ring vaccination analysis
# Paper: Kucharski et al. Effectiveness of Ring Vaccination as Control Strategy for Ebola Virus Disease. EID, 2016
# Adaptations AR: Guinea values from Robert et al. AJE analysis adjusted
# and subsequent generation. No lag to vaccination
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# SEA: -> Function storage
# Fit NB to individual level Guinea chain data

#' Fit Negative Binomial to data
#' @author Adam Kucharski
#' @param save_path
#'
#' @return
#' @export
#'
#' @examples
#'
offspring.inference<-function(save_path = "plots/NBsecondary.png"){



  # Althaus fit all data
  # Number of individuals in the trees
  n <- 152
  # Number of secondary cases for all individuals
  c1 <- c(1,2,2,5,14,1,4,4,1,3,3,8,2,1,1,4,9,9,1,1,17,
          2,1,1,1,4,3,3,4,2,5,1,2,2,1,9,1,3,1,2,1,1,2)
  c0 <- c(c1,rep(0,n-length(c1)))
  # Fitting a negative binomial distribution to the number of fitsecondary cases
  fit.cases <- fitdist(c0,"nbinom")
  summary(fit.cases)


  # Secondary cases from index cases in Guinea
  index.conakry <-c(17,5,1,8,2,14,2)

  index.conakry.nosuper <-c(5,1,2,2) # No funeral transmission

  # Fit NB() to number of secondary cases

  fit.cases <- fitdist(index.conakry,"nbinom")
  summary(fit.cases)

  # Secondary cases from non-index cases in Guinea

  c1 <- c(1,2,5,1,4,4,1,3,3,2,1,1,4,9,9,1,1,2,1,1,1,4,3,3,4,2,2,1,9,1,3,1,2,1,1,2)
  #c1 <- c(1,2,5,1,4,4,1,3,3,2,1,1,4,1,1,2,1,1,1,4,3,3,4,2,2,1,1,3,1,2,1,1,2)
  n <- 152-length(index.conakry)

  nonindex.conakry <- c(c1,rep(0,n-length(c1)))

  fit.casesB <- fitdist(nonindex.conakry,"nbinom")

  # Secondary cases from Liberia

  index.liberia <-c(5)

  lib1 <- c(3,0,0,1,0,1,0,1,2,0,0,5,1,0,1,0,1,0,0,0,0)
  nlib <- 22-length(index.liberia)

  nonindex.lib <- lib1

  fit.casesL <- fitdist(nonindex.lib,"nbinom")

  par(mfrow=c(1,3))

  plot.negbin.distribution(fit.cases,"A")
  plot.negbin.distribution(fit.casesB,"B")
  plot.negbin.distribution(fit.casesL,"C")

  dev.copy(png,paste(save_path,sep=""),units="cm",width=20,height=10,res=300)
  dev.off()

}
