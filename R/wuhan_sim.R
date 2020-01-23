#' Title
#'
#' @param n.cores
#' @param n.sim
#' @param wvaccYN
#' @param define_6m
#' @param initial.cases.pcluster
#' @param initial.clusters
#' @param prop.ascertain
#' @param cap_cases
#' @param cap_max_days
#' @param r0within
#' @param r0Am
#' @param overkkmiss
#' @param overkk
#' @param vefficacy
#' @param vuptake
#' @param ring.size
#' @param time_to_protection
#' @param time_to_isolation
#' @param incub_mean
#' @param incub_var
#' @param inf_mean
#' @param inf_var
#' @param delay_shape
#' @param delay_rate
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom foreach foreach %dopar%
#' @importFrom doMC registerDoMC
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
wuhan_sim <- function(n.cores=6,n.sim=1,wvaccYN,define_6m,initial.cases.pcluster,initial.clusters,prop.ascertain,
                      cap_cases,cap_max_days,r0within,r0Am,overkkmiss,overkk,vefficacy,vuptake,ring.size,
                      time_to_protection,time_to_isolation,incub_mean,incub_var,inf_mean,inf_var,delay_shape,delay_rate){

  # n.cores = number of cores for parallel processing
  # n.sim = number of simulation runs
  # wvaccYN = Yes/No include intervention in ring
  # define_6m =  Period when weekly average is below 10  # NEEDS UPDATING #
  # initial.cases.pcluster = Initial cases per ring (in ring = treatment) # NEEDS UPDATING #
  # initial.clusters = Initial number of different case clusters # NEEDS UPDATING #
  # prop.ascertain = proportion of cases identified # NEEDS UPDATING #
  # cap_cases = Limit number of cases in simulation # NEEDS UPDATING #
  # cap_max_days = days since start of outbreak where model is capped # NEEDS UPDATING #
  # r0within = R0 of cases in treatment (within ring) # NEEDS UPDATING #
  # r0Am = R0 for missed (i.e. index cases) # NEEDS UPDATING #
  # overkkmiss = Dispersion parameter for negative binomial distribution for missed cases # NEEDS UPDATING #
  # overkk = Dispersion parameter for negative binomial distribution for cases within ring # NEEDS UPDATING #
  # vefficacy = efficacy of intervention in ring # NEEDS UPDATING #
  # vuptake = uptake of intervention in ring # NEEDS UPDATING #
  # ring.size = Maximum ring size limit # NEEDS UPDATING #
  # time_to_protection = Average time until intervention is effective?
  # incub_mean = mean of gamma distribution for incubation period
  # incub_var = variance of gamma distribution for incubation period
  # inf_mean = mean of gamma distribution for infectiousness
  # inf_var = variance of gamma distribution for infectiousness
  # delay_shape = shape parameter of gamma distribution for delay from symptom onset to isolation
  # delay_rate = rate parameter of gamma distribution for delay from symptom onset to isolation


  # Set number of cores for parallel processing ----------------------------------------------------
  clust1<-registerDoMC(n.cores)

  # Empty things to store run results in ----------------------------------------------------
  outbreak_size_all <-numeric()
  outbreak_size <-numeric()
  outbreak_n_clusters<-numeric()

  # Store onsets
  date_seq <- seq(0,define_6m,1)
  outbreak_onsets <- NULL

  # Store monthly cases
  onsets_by_week <- NULL

  # Intervention effectiveness
  veff <- vefficacy*vuptake

  # Reduction in R0 as result of intervention
  r0Bwithin <- r0within*(1-veff)

  # Incubation period distribution
  incub_param=c(incub_mean,incub_var)
  incubfn<-function(x){rgamma(x,shape=incub_param[1]/(incub_param[2]^2/incub_param[1]),
                              scale=(incub_param[2]^2/incub_param[1]))}

  # Infectiousness distribution
  inf_param=c(inf_mean,inf_var)
  infecfn<-function(x){rgamma(x,shape=inf_param[1]/(inf_param[2]^2/inf_param[1]),
                              scale=(inf_param[2]^2/inf_param[1]))}

  # Distribution for delay from symptom onset to isolation
  rep_fn <- function(x){ #0 # Set to zero, as incorporated using function below instead
    rgamma(x,shape=delay_shape, rate=delay_rate)
  }

  # Intervention in health centers set to 0
  r0Cm <- 0
  hc.vacc <- 0

  output_par <- foreach(i = c(1:n.sim) ) %dopar% { # Parallel code

    epidata_test = outbreak_model(nrun = i,
                                  initc = initial.cases.pcluster,
                                  initclust = initial.clusters,
                                  r0Amiss = r0Am, # R0 for missed (i.e. index) cases
                                  r0Cmiss = r0Cm, # R0 for missed (i.e. index) cases vaccinated in FOSA
                                  r0A = r0within, # R0 within ring
                                  r0B = r0Bwithin, # R0 within ring after vaccination
                                  pr.id = prop.ascertain, # Proportion ascertained
                                  wvacc = wvaccYN, # Use ring vaccination
                                  massvac = 0, # Use mass vaccination?
                                  chain.output = F, # output detailed chain data as CSV? Here set to false to minimise computation time
                                  cap_max_days = cap_max_days,
                                  cap_cases = cap_cases,
                                  rep_fn = rep_fn,
                                  time_to_isolation = time_to_isolation,
                                  time_to_protection= time_to_protection,
                                  ring.size = ring.size,
                                  hc.vacc = hc.vacc,
                                  overkkmiss = overkkmiss,
                                  overkk = overkk,
                                  infecfn = infecfn,
                                  incubfn = incubfn
    )
    outbreak_n_clusters0 <-nrow(epidata_test$clusertab) # how many clusters
    outbreak_n_clusters0 <- nrow(epidata_test$clusertab %>%
                                   filter(endt<define_6m) ) # Clusters in first 6m
    outbreak_size_all0 <-sum(epidata_test$clusertab$size)# Cases in first 6m
    outbreak_size0 <- sum(epidata_test$onsets<define_6m) # Cases in first 6m
    outbreak_onsets0 <- sapply(date_seq,function(x){
      sum(round(epidata_test$onsets)==x)
    })

    list(outbreak_n_clusters = outbreak_n_clusters0,
         outbreak_size = outbreak_size0,
         outbreak_size_all = outbreak_size_all0,
         outbreak_onsets = outbreak_onsets0)
  }


  # Gather outputs
  for(i in 1:n.sim){
    outbreak_n_clusters[i] <- output_par[[i]]$outbreak_n_clusters
    outbreak_size[i] <- output_par[[i]]$outbreak_size
    outbreak_size_all[i] <- output_par[[i]]$outbreak_size_all
    outbreak_onsets <- rbind(outbreak_onsets, cbind(i, date_seq, output_par[[i]]$outbreak_onsets))
  }

  outbreak_onsets <- as_tibble(outbreak_onsets)
  names(outbreak_onsets) = c("n.sim","day","number")
  output_df <- data.frame(n.sim = 1:n.sim, outbreak_n_clusters, outbreak_size, outbreak_size_all)

  return(output_df)
}
