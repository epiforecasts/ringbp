# Individual offspring distribution
offspring<-function(x,hc.vacc,r0A,r0B,r0Amiss,r0Cmiss,index.ascertained,tvacc,overkk,overkkmiss,ncases,first.case,vacc_cf=F,vacc_cf_index=F){
  sapply(x,function(xx){

    # Vaccination counter-factual: initial cases generate excess over vaccination scenario
    if(vacc_cf==T & vacc_cf_index==T){case_out <- rnbinom(1,size=overkk,mu=(r0A-r0B))}

    # Vaccination counter-factual: no drop in reproduction number
    if(vacc_cf==T & vacc_cf_index==F){case_out <- rnbinom(1,size=overkk,mu=r0A)}

    # Non-counterfactual skeleton:
    # 1. Index case infects all - other cases scaled by their infectious period (as R might drop while infectious) & vacc_cf==F
    if(xx<tvacc & xx==0 & vacc_cf==F ){
      case_out <- round((1-hc.vacc)*rnbinom(1,size=overkkmiss,mu=r0Amiss))+round(hc.vacc*rnbinom(1,size=overkkmiss,mu=r0Cmiss)) }

    # 2. Non index cases have a lower reproduction numver
    if(xx<tvacc & xx!=0){case_out <- rnbinom(1,size=overkk,mu=r0A)}

    #if(xx<tvacc & xx>=index.ascertained){case_out <- rnbinom(1,size=overkk,mu=r0A)}

    # 3. Reproduction number drops after vaccination introduced
    if(xx>=tvacc  & vacc_cf==F){ case_out <- rnbinom(1,size=overkk,mu=r0B)}

    # case_out <- 0 # DEBUG
    case_out
  } )
}
