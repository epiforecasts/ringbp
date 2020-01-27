
incubfn <- function(x){
  incub_param=c(6.7,2)

  rgamma(x,shape=incub_param[1]/(incub_param[2]^2/incub_param[1]),
                              scale=(incub_param[2]^2/incub_param[1]))
  }

# Infectiousness distribution
infecfn <- function(x){
  inf_param=c(6.7,2)

  rgamma(x,shape=inf_param[1]/(inf_param[2]^2/inf_param[1]),
                              scale=(inf_param[2]^2/inf_param[1]))
  }


rep_fn <- function(x){

  delay_param=c(3,1)

  rgamma(x,shape=delay_param[1]/(delay_param[2]^2/delay_param[1]),
                             scale=(delay_param[2]^2/delay_param[1]))

  }


secondary_draw <- function(index_case,r0isolated,r0community,disp.iso,disp.com){

  vect_isTRUE <- function(x) {
    purrr::map_lgl(x, isTRUE)
  }

  index_case <- dplyr::mutate(index_case,
                new.cases = purrr::map2_dbl(ifelse(vect_isTRUE(isolated), disp.iso, disp.com),
                                            ifelse(vect_isTRUE(isolated), r0isolated, r0community),
                                             ~ rnbinom(1, size = .x,
                                    mu = .y)),
                isolated = TRUE)


  new_cases <- dplyr::filter(index_case, new.cases > 0)


  if (nrow(new_cases) > 0) {

    new_cases <- dplyr::group_by(new_cases, caseid)

    new_cases <- dplyr::group_split(new_cases)

    new_cases <- map(new_cases,
      ~  ## whether these cases remain in cluster or are missed
        mutate(data.frame(exposure = rep(.$latent, times = .$new.cases), # exposure is taken from infector data
                          missed = rbernoulli(n = .$new.cases, p = prop.ascertain)),
               onset = exposure + incubfn(.$new.cases), # onset of new cases
               latent = exposure + infecfn(.$new.cases), # when new cases infect people
               cluster=ifelse(vect_isTRUE(missed), NA, .$cluster), # remain within cluster with prob = prop.ascertain
               # set isolated time from onset, same as infector if you remain within cluster
               isolated_time = ifelse(vect_isTRUE(missed), onset + rep_fn(.$new.cases), .$isolated_time + rep_fn(.$new.cases)),
               isolated = !vect_isTRUE(missed)))


    index_case <- dplyr::bind_rows(dplyr::select(index_case, -new.cases),
                                    new_cases)
  }else{
    index_case <- dplyr::select(index_case, -new.cases)
  }

  return(index_case)
} # End function

new_cluster <- function(index_case,r0community,disp.com,total.clusters){


  if(index_case$cluster < total.clusters){
    return(index_case) # filters out old clusters and returns them unchanged
  }

  index_case$isolated <- TRUE # this case has had a chance to infect, so now it is isolated

  new_cases <- rnbinom(1,size=disp.com,mu=r0community) # number of new cases in the cluster formed from missed case

  if(new_cases==0){
    return(index_case)
  }else{

    cluster_data <- data.frame(cluster = rep(index_case$cluster,new_cases), # set cluster number to index case cluster number
                               onset = index_case$latent + incubfn(new_cases), # onset time from when infector infected + incubation period
                               exposure = rep(index_case$latent,new_cases), # exposure time is when infector infected
                               latent = index_case$latent + infecfn(new_cases), # time when these cases will infect
                               missed = rep(FALSE,new_cases)) %>% # none missed since they are all in this new cluster

      mutate(isolated_time = min(onset) + rep_fn(1), # cluster is isolated at minimum onset time + delay
             isolated=ifelse(latent>isolated_time,TRUE,FALSE)) %>% # cases that don't infect before isolation are marked isolated
      bind_rows(index_case) # add index case of cluster back to dataset


    return(cluster_data)
  }
}





##############################################
# STEP 1 - SET UP INITIAL CLUSTERS AND CASES #
##############################################

num.initial.cases <- 15
num.initial.clusters <- 15
total.clusters <- num.initial.clusters
total.cases <- num.initial.cases*num.initial.clusters
prop.ascertain <- 0.5
latest.onset <- 0

case_data <- data.frame(exposure = rep(0,num.initial.cases*num.initial.clusters), # Exposure time of 0 for all initial cases
                        onset = incubfn(num.initial.cases*num.initial.clusters), # Draw symptom onset for all initial
                        latent = infecfn(num.initial.cases*num.initial.clusters), # Draw when initial cases infect people
                        cluster = rep(1:num.initial.clusters, rep(num.initial.cases,num.initial.clusters)), # Set cluster number
                        missed = rep(FALSE, num.initial.cases*num.initial.clusters), # All initial cases are known
                        # infector = rep(0,num.initial.cases*num.initial.clusters), # caseid of initial infector
                        caseid = 1:(num.initial.cases*num.initial.clusters)) %>% # set case id

  group_by(cluster) %>% mutate(isolated_time=min(onset)+rep_fn(1)) %>% # set isolation time for initial clusters based of earliest symptom onset + delay
  ungroup() %>% mutate(isolated = ifelse(latent>isolated_time,TRUE,FALSE)) # set cases to isolated if their infect time is after isolation time for cluster



cap_max_days <- 365
cap_cases <- 5000
extinct <- FALSE

while(latest.onset < cap_max_days & total.cases<cap_cases & !extinct){

  ########################################################
  # STEP 2 - DRAW SECONDARY CASES FOR NON-ISOLATED CASES #
  ########################################################
  case_data <- secondary_draw(index_case = case_data,
                              r0isolated = 0,
                              r0community = 2,
                              disp.iso = 0.1,
                              disp.com = 0.5) # add a new generation


  ########################################################
  # STEP 3 - UDPATE CLUSTER NUMBERS FOR NEW MISSED CASES #
  ########################################################

  case_data <- mutate(case_data, caseid=1:nrow(case_data)) # ungroup and reset caseid

  num.new.clusters <- sum(is.na(case_data$cluster)) # number of new clusters
  case_data$cluster[is.na(case_data$cluster)==TRUE] <- (total.clusters+1):(num.new.clusters+total.clusters) # set new cluster numbers

  case_data <- dplyr::group_by(case_data, caseid)

  case_data <- group_map(case_data, ~new_cluster(index_case = .,
                                                    total.clusters = total.clusters,
                                                    r0community = 2,
                                                    disp.com = 0.5), keep = FALSE) # set new numbers of cases in each new cluster


  case_data <- dplyr::bind_rows(case_data)

  case_data <- mutate(case_data, caseid=1:nrow(case_data)) # ungroup and reset caseid


  total.clusters <- total.clusters + num.new.clusters # update total cluster variable
  total.cases <- nrow(case_data) # update total number of cases
  latest.onset <- max(case_data$onset)
  extinct <- all(case_data$isolated)
  print(all(case_data$isolated))
  print(total.cases)
  print(latest.onset)
}

######################################
# STEP 4 - PRODUCE SIMULATION OUTPUT #
######################################

case_data %>% mutate(day=round(onset)) %>% group_by(day) %>% summarise(daily_cases=sum(n())) %>%
  ggplot(aes(x=day,y=daily_cases)) + geom_line() + theme_bw()

case_data %>% mutate(week=floor(onset/7)) %>% group_by(week) %>% summarise(weekly_cases=sum(n())) %>%
  ggplot(aes(x=week,y=weekly_cases)) + geom_line() + theme_bw()

case_data %>% mutate(week=floor(onset/7)) %>% group_by(week) %>% summarise(weekly_cases=sum(n())) %>%
  mutate(cs=cumsum(weekly_cases)) %>% ggplot(aes(x=week,y=cs)) + geom_line() + theme_bw()


