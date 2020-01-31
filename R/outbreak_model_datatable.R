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
outbreak_model <- function(num.initial.cases, num.initial.clusters, prop.ascertain,
                           cap_max_days, cap_cases, r0isolated, r0community, disp.iso, disp.com,
                           mu_si, mu_ip, sd_si, sd_ip, k, delay_shape, delay_scale) {

  # Set up functions to sample from distributions
  intervalfn <- dist_setup_mvt(mu_ip = mu_ip, mu_si = mu_si, sd_ip = sd_ip, sd_si = sd_si, k = k)
  delayfn <- dist_setup(delay_shape, delay_scale)

  # Set initial values for loop indices
  total.clusters <- num.initial.clusters
  total.cases <- num.initial.cases * num.initial.clusters
  latest.onset <- 0
  extinct <- FALSE

  # Initial setup
  case_data <- branch_setup(num.initial.cases = num.initial.cases,
                            num.initial.clusters = num.initial.clusters,
                            intervalfn = intervalfn,
                            delayfn = delayfn)

  # Model loop
  while (latest.onset < cap_max_days & total.cases < cap_cases & !extinct) {

    case_data <- branch_step(case_data=case_data,
                             total.clusters = total.clusters,
                             total.cases = total.cases,
                             extinct = extinct,
                             disp.iso = disp.iso,
                             disp.com = disp.com,
                             r0isolated = r0isolated,
                             r0community = r0community,
                             intervalfn = intervalfn,
                             delayfn = delayfn,
                             prop.ascertain = prop.ascertain)

    total.cases <- nrow(case_data)
    total.clusters <- max(case_data$cluster)
    latest.onset <- max(case_data$onset)
    extinct <- all(case_data$isolated)
  }

  # Prepare output
  weekly_cases <- case_data[,week := floor(onset / 7),
                            ][, .(weekly_cases = .N),by=week
                              ][order(week)
                                ][,cumulative:=cumsum(weekly_cases)]

  max_week <- floor(cap_max_days/7)
  outbreak_length <- nrow(weekly_cases)
  outbreak_max <- max(weekly_cases$cumulative)

  if(max_week > nrow(weekly_cases)){
    # Adds on extra weeks of 0 weekly cases and same cumulative cases until end of specified maximum time period
    weekly_cases <- dplyr::bind_rows(weekly_cases,
                                     data.frame(week = ((outbreak_length+1):max_week),
                                                weekly_cases = rep(0,(max_week-outbreak_length)),
                                                cumulative = rep(outbreak_max,(max_week-outbreak_length))))
  }else{
    # chop weekly cases down to size required for wuhan_sim to bind rows
    weekly_cases <- weekly_cases[1:max_week,]
  }

  return(weekly_cases)
}
