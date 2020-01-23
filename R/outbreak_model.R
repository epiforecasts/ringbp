# Outbreak trajectories

#' Outbreak trajectory branching process model
#' @author Adam Kucharski
#' @param nrun
#' @param initc
#' @param initclust
#' @param r0Amiss
#' @param r0Cmiss
#' @param r0A
#' @param r0B
#' @param pr.id
#' @param wvacc
#' @param massvac
#' @param chain.output
#' @param vacc_cf
#' @param vacc_onsets
#' @param cap_max_days
#' @param cap_cases
#' @param rep_fn
#' @param time_to_isolation
#' @param time_to_protection
#' @param ring.size
#' @param hc.vacc
#' @param overkkmiss
#' @param overkk
#' @param infecfn
#' @param incubfn
#'
#' @return
#' @export
#'
#' @examples
#'
outbreak_model<-function(nrun,initc, initclust,r0Amiss,r0Cmiss,r0A,r0B,pr.id,wvacc,massvac=0,chain.output=F,vacc_cf=F,
                         vacc_onsets=NULL,cap_max_days,cap_cases,rep_fn,time_to_isolation,time_to_protection,ring.size,hc.vacc,
                         overkkmiss,overkk,infecfn,incubfn){

  # if ring vaccination (wvacc==1), reduced transmission, otherwise no reduction
  if(wvacc==1){r0Ba=r0B}else{r0Ba=r0A}

  # applying mass vaccination if needed
  r0A <- r0A*(1-massvac)
  r0B <- r0B*(1-massvac)
  r0Amiss <- r0Amiss*(1-massvac)
  r0Cmiss <- r0Cmiss*(1-massvac)

  # Parameters - cluster
  number.clust.onset=rep(initc,initclust) # vector with number of cases in each cluster at t=0. IC: add extra set of clusters randomly one incubation period later

  # In vaccination counter-factual, use onset times (default is vacc_onsets==NULL, therefore onset times of cluster = 0)
  if(is.null(vacc_onsets)){
    timesclust.onset=c(rep(0,initclust)) #,incubfn(initclust))
  }else{
    timesclust.onset = vacc_onsets #,incubfn(initclust))
  }

  clustindex <- 1
  nclusters <- initclust
  latest.onset <- 0

  # For each cluster, record size and end time
  onset.store.tab=NULL
  clust.tab=data.frame(matrix(NA,nrow=0,ncol=4))
  clust.chain.all=NULL
  clust.track=data.frame(matrix(NA,nrow=0,ncol=3))

  # - - - - - - - -
  # ITERATE ACROSS CLUSTERS

  while(nclusters > 0 & clustindex <= nclusters & latest.onset < cap_max_days & length(onset.store.tab) < cap_cases) { # Start cluster loop
    # nclusters is the number of clusters, changes through loop, starts = initclust
    # clustindex is the cluster for which calculations are being made, it change through loop, starts = 1
    # latest.onset changes through loop, starts = 0
    # cap_max_days is CAP at X days, fixed and called (from param script) before function called
    # onset.store.tab changes through loop, starts = NULL
    # cap_cases is CAP at Y cases, fixed and called (from param script) before function called
    # (need to adjust for censoring in likelihood)

    # decide whether cluster missed or not
    #if((1-pr.id)>runif(1)){r0mainA=r0Amiss;r0Bmain=r0Amiss}else{r0mainA=r0A;r0Bmain=r0Ba}
    r0mainA=r0A;r0Bmain=r0Ba

    # record secondary cases
    secondary.record=NULL
    secondary.record.miss=NULL
    secondary.record.all=NULL

    # define cluster onset
    ncases <- number.clust.onset[clustindex] # number of cases in index cluster

    # print(ncases)

    cluster_onset.inf=timesclust.onset[clustindex] # onset time of index cluster


    if(clustindex>initclust){ # if cluster is not amongst the initial clusters

      # If not amongth intial clusters, then generate new onset from on infecting case onset
      clust.exp.new <- cluster_onset.inf + infecfn(ncases)
      clust.onset.new <- clust.exp.new + incubfn(length(clust.exp.new))

      # define initial case times in cluster
      tcases <- clust.onset.new
      times.onset <- tcases
      initial.cases<-length(times.onset)

      # delay in implementing vaccination - include ascertainment delay (based on duration of infection?)
      first.case=min(clust.onset.new)
      ascertain.delay=rep_fn(1)
      index.ascertained = first.case+ascertain.delay
      tvacc=index.ascertained+time_to_isolation+time_to_protection # THIS IS VACCINATION DELAY

    }else{ # if cluster is amongst the initial clusters
      clust.onset.new <-cluster_onset.inf

      # define initial case times in cluster
      tcases <- clust.onset.new
      times.onset <- rep(tcases,initc)
      initial.cases<-length(times.onset)

      # delay in implementing vaccination - include ascertainment delay
      first.case=min(clust.onset.new)
      ascertain.delay=rep_fn(1)
      index.ascertained = first.case+ascertain.delay
      tvacc=index.ascertained+time_to_isolation+time_to_protection # THIS IS VACCINATION DELAY
    }

    missed.tally <- numeric()
    missed.tally.sum <- numeric()

    case.id=1
    case.total=initial.cases

    # create a data frame to store outputs from chain, starting by the first case in first cluster

    clust.chain=data.frame(matrix(NA,nrow=ring.size,ncol=8))

    # Useful values to store:
    if(chain.output==T){

      names(clust.chain)=c("initial","case","secondary","infector","missed","cluster.new","onset.t","post.vac")
      clust.chain$case=c(1:ring.size)
      if(clustindex>initclust){
        clust.chain[1:case.total,"initial"]=2 #cluster is not amonst initial clusters
      }else{
        clust.chain[1:case.total,"initial"]=1 #cluster is amongst initial clusters
      }
      clust.chain[1:case.total,"missed"]=0
      clust.chain[1:case.total,"onset.t"]=clust.onset.new
      clust.chain[1:case.total,"post.vac"]=tvacc
    }

    # - - - - - - - -
    # ITERATE ACROSS CASES

    while(ncases > 0 & ncases < ring.size) {
      # ring.size = people within cluster

      # - - -
      # generate new exposures within cluster

      # In vaccine counterfactual, identify index links to add cases (the cases generated
      # directly before vaccination generate more cases than if no vaccination had been in place)
      if(clustindex<=initclust & vacc_cf==T & tcases[1] ==cluster_onset.inf){
        secondary <- offspring(tcases,hc.vacc,r0mainA,r0Bmain,r0Amiss,r0Cmiss,index.ascertained,
                               tvacc,overkk,overkkmiss,ncases,first.case,vacc_cf=T,vacc_cf_index=T)
      }

      # In vaccine counterfactual, generate cases without vaccine reduction
      if(clustindex<=initclust & vacc_cf==T & tcases[1] !=cluster_onset.inf){
        secondary <- offspring(tcases,hc.vacc,r0mainA,r0Bmain,r0Amiss,r0Cmiss,index.ascertained,
                               tvacc,overkk,overkkmiss,ncases,first.case,vacc_cf=T,vacc_cf_index=F)
      }

      # Normal scenario for secondary cases
      if(clustindex>initclust | vacc_cf==F){
        secondary <- offspring(tcases,hc.vacc,r0mainA,r0Bmain,r0Amiss,r0Cmiss,index.ascertained,
                               tvacc,overkk,overkkmiss,ncases,first.case,vacc_cf)
      }

      # Need to edit for depletion of susceptibles
      if(sum(secondary)>0 & min(cumsum(secondary)<(ring.size-length(times.onset)))==0){
        overshoot=sum((cumsum(secondary)<(ring.size-length(times.onset))))+1
        secondary0=secondary*(cumsum(secondary)<(ring.size-length(times.onset))) # Stop overshoot
        secondary0[overshoot]=(ring.size-length(times.onset)-sum(secondary0[1:(overshoot-1)]))
        secondary=secondary0
      }

      secondary.record=c(secondary.record,secondary) # record Reproduction number
      secondary.record.all=c(secondary.record.all,secondary) # record Reproduction number

      exp.new <- numeric()
      onsett.new <- numeric()
      pickMissed<-NULL
      miss.1<-NULL

      # - - -
      # generate new clusters from non-ascertained cases - pick which are ascertained

      for(j in 1:length(secondary)) {
        exp.new <- tcases[j] + infecfn(secondary[j])

        onsett.new <- c(onsett.new,sort(exp.new + incubfn(length(exp.new)))) # generate corresponding new onsets

        pickMissed0<-if(secondary[j]==0){NULL}else{(runif(secondary[j])>pr.id)} #pr.id= proportion ascertained
        miss.1<-c(miss.1,if(secondary[j]==0){0}else{sum(pickMissed0)})
        pickMissed<-c(pickMissed,pickMissed0)
      }

      # Record missed cases
      missed.tally=c(missed.tally,miss.1)
      missed.tally.sum=c(missed.tally.sum,sum(miss.1))

      # Collect transmission chain data
      if(chain.output==T & sum(secondary)>0){

        # Record onset time and whether pre vaccination
        miss.A=(clust.chain[case.id:case.total,"missed"]==0)
        clust.chain[case.id:case.total,"secondary"][miss.A]=secondary
        clust.chain[case.id:case.total,"secondary"][miss.A]=secondary
        case.k=case.total+1
        case.k0=1

        for(j in 1:length(secondary)){
          # Identify infector and whether missed or now
          infect.id=sum(cumsum(miss.A)<j)+1
          if(secondary[j]>0){
            clust.chain[case.k:(case.k+secondary[j]-1),"onset.t"]=onsett.new[case.k0:(case.k0+secondary[j]-1)]
            clust.chain[case.k:(case.k+secondary[j]-1),"infector"]=case.id+infect.id-1
            clust.chain[case.k:(case.k+secondary[j]-1),"missed"]=as.numeric(pickMissed[case.k0:(case.k0+secondary[j]-1)])
            case.k=case.k+secondary[j]
            case.k0=case.k0+secondary[j]
          }

        }

        case.id=case.total+1
        case.total=case.total+sum(secondary)

        missB=clust.chain[case.id:case.total,"case"][clust.chain[case.id:case.total,"missed"]==1]
      }

      # Calculate new clusters from missed cases

      if(length(onsett.new)>0 & sum(pickMissed)>0){

        # Pick ascertained
        secondaryclust <- newclustn(onsett.new[pickMissed],r0Amiss,overkkmiss) # Calculate secondary cases
        secondaryclustID <- secondaryclust[secondaryclust>0]
        onset_above0<-onsett.new[pickMissed][secondaryclust>0] # Those that generate secondary cases

        secondary.record.all=c(secondary.record.all,secondaryclust) # Record secondary cases here too
        secondary.record.miss=c(secondary.record.miss,secondaryclust) # Record secondary cases here too

        # For simplicity, store onset of infector
        clust.onset.new <- numeric()
        if(length(secondaryclustID)>0){
          for(j in 1:length(secondaryclustID)) {
            clust.onset.new <- c(clust.onset.new,onset_above0[j])
          }
        }

        #clust.onset.new <- rep(onsett.new,sum(secondaryclust>0))

        # Collect transmission chain data for clusters
        if(chain.output==T & length(secondaryclustID)>0){
          so.far=length(number.clust.onset)+1
          newclusterdata=cbind(
            rep(clustindex,length(secondaryclustID)),c(so.far:(so.far+length(secondaryclustID)-1)),missB[secondaryclust>0]
          )
          clust.track=rbind(clust.track,newclusterdata)

        }

        # collect new cluster data
        number.clust.onset <- c(number.clust.onset,secondaryclustID) # Add index cases
        timesclust.onset <- c(timesclust.onset,clust.onset.new)
        nclusters<-length(timesclust.onset)


        # within cluster case tracking
        tcases <- onsett.new[pickMissed==0]
        ncases<-length(onsett.new[pickMissed==0])
        times.onset <- c(times.onset,onsett.new[pickMissed==0])

      }else{

        # within cluster case tracking
        tcases <- onsett.new
        ncases<-length(onsett.new)
        times.onset <- c(times.onset,onsett.new)

      }

      if(is.na(ncases)){print(times.onset)}

      latest.onset = max(times.onset)


    } # - - - End cases loop

    clust.chain=clust.chain[1:ring.size,]
    clust.chain.all=rbind(clust.chain.all,clust.chain)


    times.onsetF = times.onset #[times.onset<270]

    # move onto next cluster
    clustindex=clustindex+1
    clust.tab=rbind(clust.tab,c(length(times.onsetF),max(times.onset),sum(missed.tally),
                                ifelse(length(secondary.record.all)==0,NA,mean(secondary.record.all))))

    onset.store.tab=c(onset.store.tab,times.onset)

  } # - - - End cluster loop

  names(clust.tab)=c("size","endt","missed","mean.R")
  names(clust.track)=c("inf.cluster","new.cluster","inf.id")

  # NEED TO OUTPUT ONSET DATES

  if(chain.output==T){
    write.csv(clust.chain.all,file=paste(chains,"/cluster_chainsV",wvacc,"_P",100*pr.id,"_nrun",10000+nrun,".csv",sep=""))
    write.csv(clust.track,file=paste(chains,"/cluster_trackV",wvacc,"_P",100*pr.id,"_nrun",10000+nrun,".csv",sep=""))
  }


  return(list(clusertab=clust.tab,onsets=onset.store.tab,chains_all=clust.chain.all,clusters_tracked=clust.track))

  #sum(clust.tab$size)


}
