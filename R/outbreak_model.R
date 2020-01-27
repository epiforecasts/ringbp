

incubfn <- function(x) {
  incub_param = c(6.7, 2)

  rgamma(
    x,
    shape = incub_param[1] / (incub_param[2] ^ 2 / incub_param[1]),
    scale = (incub_param[2] ^ 2 / incub_param[1])
  )
}

# Infectiousness distribution
infecfn <- function(x) {
  inf_param = c(6.7, 2)

  rgamma(
    x,
    shape = inf_param[1] / (inf_param[2] ^ 2 / inf_param[1]),
    scale = (inf_param[2] ^ 2 / inf_param[1])
  )
}


rep_fn <- function(x) {
  delay_param = c(3, 1)
  rgamma(x,
    shape = delay_param[1] / (delay_param[2] ^ 2 / delay_param[1]),
    scale = (delay_param[2] ^ 2 / delay_param[1]))
}

dist_setup <- function(dist_mean,dist_var){
  dist_param <- c(dist_mean,dist_var)
  out <- partial(rgamma,
                 shape = dist_param[1] / (dist_param[2] ^ 2 / dist_param[1]),
                 scale = (dist_param[2] ^ 2 / dist_param[1]))
  return(out)
}

outbreak_model <- function(num.initial.cases, num.initial.clusters, prop.ascertain,
                           cap_max_days, cap_cases, r0isolated, r0community, disp.iso, disp.com) {

  ##############################################
  # STEP 1 - SET UP INITIAL CLUSTERS AND CASES #
  ##############################################
  total.clusters <- num.initial.clusters
  total.cases <- num.initial.cases * num.initial.clusters
  latest.onset <- 0
  extinct <- FALSE

  case_data <- data.frame(exposure = rep(0, num.initial.cases * num.initial.clusters), # Exposure time of 0 for all initial cases
                          onset = incubfn(num.initial.cases * num.initial.clusters), # Draw symptom onset for all initial
                          latent = infecfn(num.initial.cases * num.initial.clusters), # Draw when initial cases infect people
                          cluster = rep(1:num.initial.clusters, rep(num.initial.cases, num.initial.clusters)), # Set cluster number
                          missed = rep(FALSE, num.initial.cases * num.initial.clusters), # All initial cases are known
                          caseid = 1:(num.initial.cases * num.initial.clusters)) %>% # set case id

    group_by(cluster) %>% mutate(isolated_time = min(onset) + rep_fn(1)) %>% # set isolation time for initial clusters based of earliest symptom onset + delay
    ungroup() %>% mutate(isolated = ifelse(latent > isolated_time, TRUE, FALSE)) # set cases to isolated if their infect time is after isolation time for cluster





  while (latest.onset < cap_max_days & total.cases < cap_cases & !extinct) {

    ########################################################
    # STEP 2 - DRAW SECONDARY CASES FOR NON-ISOLATED CASES #
    ########################################################
    case_data <- secondary_draw(
      index_case = case_data,
      r0isolated = r0isolated,
      r0community = r0community,
      disp.iso = disp.iso,
      disp.com = disp.com,
      prop.ascertain = prop.ascertain) # add a new generation


    ########################################################
    # STEP 3 - UDPATE CLUSTER NUMBERS FOR NEW MISSED CASES #
    ########################################################

    case_data <- mutate(case_data, caseid = 1:nrow(case_data)) # ungroup and reset caseid

    num.new.clusters <- sum(is.na(case_data$cluster)) # number of new clusters
    # set new cluster numbers
    case_data$cluster[is.na(case_data$cluster) == TRUE] <- (total.clusters + 1):(num.new.clusters + total.clusters)

    case_data <- dplyr::group_by(case_data, caseid)

    case_data <- group_map(case_data,
                           ~ new_cluster(index_case = .,
                                         total.clusters = total.clusters,
                                         r0community = r0community,
                                         disp.com = disp.com),keep = FALSE) # set new numbers of cases in each new cluster


    case_data <- dplyr::bind_rows(case_data)

    case_data <- mutate(case_data, caseid = 1:nrow(case_data)) # ungroup and reset caseid


    total.clusters <- total.clusters + num.new.clusters # update total cluster variable
    total.cases <- nrow(case_data) # update total number of cases
    latest.onset <- max(case_data$onset)
    extinct <- all(case_data$isolated)
  } # while loop end

  ######################################
  # STEP 4 - PRODUCE SIMULATION OUTPUT #
  ######################################

  daily_cases <- case_data %>% mutate(day = round(onset)) %>% group_by(day) %>%
    summarise(daily_cases = sum(n()))

  weekly_cases <- case_data %>% mutate(week = floor(onset / 7)) %>% group_by(week) %>%
    summarise(weekly_cases = sum(n())) %>% mutate(cumulative = cumsum(weekly_cases))

  return(list(daily_cases, weekly_cases))
}
