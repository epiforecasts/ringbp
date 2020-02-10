
#' Run a single instance of the branching process model
#' @author Joel Hellewell
#' @inheritParams outbreak_step
#' @param delay_shape numeric shape parameter of delay distribution
#' @param delay_scale numeric scale parameter of delay distribution
#'
#' @return data.table of cases by week, cumulative cases, and the effective reproduction number of the outreak
#' @export
#'
#' @importFrom data.table rbindlist
#'
#' @examples
#'
#'\dontrun{
#'outbreak_model(num.initial.cases = 5,
#'cap_max_days = 365,
#'cap_cases = 2000,
#'r0isolated = 0,
#'r0community = 2.5,
#'disp.iso = 1
#'disp.com = 0.16
#'k = 0.7
#'delay_shape = 2.5
#'delay_scale = 5
#'prop.asym = 0)
#'}
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
  case_data <- outbreak_setup(num.initial.cases = num.initial.cases,
                            incfn = incfn,
                            prop.asym= prop.asym,
                            delayfn = delayfn,
                            k = k)

  # Preallocate
  effective_r0_vect <- c()
  cases_in_gen_vect <- c()


  # Model loop
  while (latest.onset < cap_max_days & total.cases < cap_cases & !extinct) {

    out <- outbreak_step(case_data=case_data,
                             disp.iso = disp.iso,
                             disp.com = disp.com,
                             r0isolated = r0isolated,
                             r0community = r0community,
                             incfn = incfn,
                             delayfn = delayfn,
                             prop.ascertain = prop.ascertain,
                             k = k,quarantine = FALSE,
                             prop.asym = prop.asym)


    case_data <- out[[1]]
    effective_r0_vect <- c(effective_r0_vect, out[[2]])
    cases_in_gen_vect <- c(cases_in_gen_vect, out[[3]])
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
    weekly_cases <- data.table::rbindlist(list(weekly_cases,data.table(week=missing_weeks,weekly_cases=0)))
  }
  # order and sum up
  weekly_cases <- weekly_cases[order(week)][,cumulative := cumsum(weekly_cases)]
  # cut at max_week
  weekly_cases <- weekly_cases[week <= max_week]

  # Add effective R0
  weekly_cases <- weekly_cases[,   `:=`(effective_r0 = mean(effective_r0_vect, na.rm = TRUE),
                                        cases_per_gen = list(cases_in_gen_vect))]
  # return
  return(weekly_cases)
}
