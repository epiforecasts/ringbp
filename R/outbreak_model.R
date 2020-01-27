#' Run branching process
#' @author Joel Hellewell
#' @param num.initial.cases Initial number of cases in each initial cluster
#' @param num.initial.clusters Number of initial clusters
#' @param prop.ascertain Probability that cases are ascertained by contact tracing
#' @param cap_max_days Maximum number of days to run process for
#' @param cap_cases Maximum number of cases to run process for
#' @param r0isolated basic reproduction number for isolated cases
#' @param r0community basic reproduction number for non-isolated cases
#' @param disp.iso dispersion parameter for negative binomial distribution for isolated cases
#' @param disp.com dispersion parameter for negative binomial distribution for non-isolated cases
#' @param incub_mean Mean of incubation period distribution
#' @param incub_var Variance of incubation period distribution
#' @param inf_mean Mean of time until infectious distribution
#' @param inf_var Variance of time until infectious distribution
#' @param delay_mean Mean of distribution for delay between symptom onset and isolation
#' @param delay_var Variance of distribution for delay between symptom onset and isolation
#'
#' @return
#' @export
#' @importFrom dplyr group_by mutate ungroup group_map summarise
#' @examples
#'
outbreak_model <- function(num.initial.cases, num.initial.clusters, prop.ascertain,
                           cap_max_days, cap_cases, r0isolated, r0community, disp.iso, disp.com,
                           incub_mean, incub_var, inf_mean, inf_var, delay_mean, delay_var) {

  ##############################################
  # STEP 1 - SET UP INITIAL CLUSTERS AND CASES #
  ##############################################

  # Set up functions to sample from distributions
  incubfn <- dist_setup(incub_mean, incub_var)
  infecfn <- dist_setup(inf_mean, inf_var)
  delayfn <- dist_setup(delay_mean, delay_var)

  # Set initial values for loop indices
  total.clusters <- num.initial.clusters
  total.cases <- num.initial.cases * num.initial.clusters
  latest.onset <- 0
  extinct <- FALSE

  # Create intial case data frame
  case_data <- data.frame(exposure = rep(0, num.initial.cases * num.initial.clusters), # Exposure time of 0 for all initial cases
                          onset = incubfn(num.initial.cases * num.initial.clusters), # Draw symptom onset for all initial
                          latent = infecfn(num.initial.cases * num.initial.clusters), # Draw when initial cases infect people
                          cluster = rep(1:num.initial.clusters, rep(num.initial.cases, num.initial.clusters)), # Set cluster number
                          missed = rep(FALSE, num.initial.cases * num.initial.clusters), # All initial cases are known
                          caseid = 1:(num.initial.cases * num.initial.clusters)) %>% # set case id

    group_by(cluster) %>% dplyr::mutate(isolated_time = min(onset) + delayfn(1)) %>% # set isolation time for initial clusters based of earliest symptom onset + delay
    dplyr::ungroup() %>% dplyr::mutate(isolated = ifelse(latent > isolated_time, TRUE, FALSE)) # set cases to isolated if their infect time is after isolation time for cluster


  # Run simulation loop
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
      prop.ascertain = prop.ascertain,
      incubfn = incubfn,
      infecfn = infecfn,
      delayfn = delayfn) # add a new generation


    ########################################################
    # STEP 3 - UDPATE CLUSTER NUMBERS FOR NEW MISSED CASES #
    ########################################################

    case_data <- dplyr::mutate(case_data, caseid = 1:nrow(case_data)) # ungroup and reset caseid

    num.new.clusters <- sum(is.na(case_data$cluster)) # number of new clusters
    # set new cluster numbers
    case_data$cluster[is.na(case_data$cluster) == TRUE] <- (total.clusters + 1):(num.new.clusters + total.clusters)

    case_data <- dplyr::group_by(case_data, caseid)

    case_data <- dplyr::group_map(case_data,
                           ~ new_cluster(index_case = .,
                                         total.clusters = total.clusters,
                                         r0community = r0community,
                                         disp.com = disp.com,
                                         incubfn = incubfn,
                                         infecfn = infecfn,
                                         delayfn = delayfn),keep = FALSE) # set new numbers of cases in each new cluster


    case_data <- dplyr::bind_rows(case_data)

    case_data <- dplyr::mutate(case_data, caseid = 1:nrow(case_data)) # ungroup and reset caseid


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
