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
#' @param incub_shape shape of incubation period distribution
#' @param incub_scale scale of incubation period distribution
#' @param inf_shape shape of time until infectious distribution
#' @param inf_scale scale of time until infectious distribution
#' @param delay_shape shape of distribution for delay between symptom onset and isolation
#' @param delay_scale scale of distribution for delay between symptom onset and isolation
#'
#' @return
#' @export
#' @importFrom dplyr group_by mutate ungroup group_map summarise n
#' @examples
#'
outbreak_model <- function(num.initial.cases, prop.ascertain,
                           cap_max_days, cap_cases, r0isolated, r0community, disp.iso, disp.com,
                           k, delay_shape, delay_scale, prop.asym) {

  # Set up functions to sample from distributions
  incfn <- dist_setup(dist_shape = 2.322737,dist_scale = 6.492272) # incubation period sampling function
  delayfn <- dist_setup(delay_shape, delay_scale)

  # Set initial values for loop indices
  total.cases <- num.initial.cases
  latest.onset <- 0
  extinct <- FALSE

  # Initial setup
  case_data <- branch_setup(num.initial.cases = num.initial.cases,
                            incfn = incfn,
                            prop.asym= prop.asym,
                            delayfn = delayfn,
                            k = k)

  # Model loop
  while (latest.onset < cap_max_days & total.cases < cap_cases & !extinct) {

    case_data <- branch_step_single(case_data=case_data,
                             total.cases = total.cases,
                             extinct = extinct,
                             disp.iso = disp.iso,
                             disp.com = disp.com,
                             r0isolated = r0isolated,
                             r0community = r0community,
                             incfn = incfn,
                             delayfn = delayfn,
                             prop.ascertain = prop.ascertain,
                             k = k,quarantine = FALSE,
                             prop.asym = prop.asym)

    total.cases <- nrow(case_data)
    latest.onset <- max(case_data$onset)
    extinct <- all(case_data$isolated)
  }

  # Prepare output, group into weeks
  weekly_cases <- case_data[,week := floor(onset / 7)
                            ][, .(weekly_cases = .N),by=week
                              ]
  # maximum outbreak week
  max_week <- floor(cap_max_days/7)
  # weeks with 0 cases in 0:max_week
  missing_weeks <- (0:max_week)[!(0:max_week %in% weekly_cases$week)]
  # add in missing weeks if any are missing
  if(length(missing_weeks>0)){
    weekly_cases <- rbindlist(list(weekly_cases,data.table(week=missing_weeks,weekly_cases=0)))
  }
  # order and sum up
  weekly_cases <- weekly_cases[order(week)][,cumulative := cumsum(weekly_cases)]
  # cut at max_week
  weekly_cases <- weekly_cases[week <= max_week]
  # return
  return(weekly_cases)
}
