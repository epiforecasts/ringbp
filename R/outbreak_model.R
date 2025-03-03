
#' Run a single instance of the branching process model
#' @author Joel Hellewell
#' @param disp.iso a positive `numeric` scalar: dispersion parameter for
#'   isolated cases (must be >0)
#' @param disp.com a positive `numeric` scalar: dispersion parameter for
#'   non-isolated cases (must be >0)
#' @param disp.subclin a positive `numeric` scalar: dispersion parameter for
#'   sub-clincial non-isolated cases (must be >0)
#' @param r0isolated a positive `numeric` scalar: reproduction number for
#'   isolated cases (must be >0)
#' @param r0community a positive `numeric` scalar: reproduction number for
#'   non-isolated cases (must be >0)
#' @param r0subclin a positive `numeric` scalar: reproduction number for
#'   sub-clinical non-isolated cases (must be >0)
#' @param prop.ascertain a nonnegative `numeric` scalar: proportion of
#'   infectious contacts ascertained by contact tracing (must be 0<=x<=1)
#' @param k a `numeric` scalar: skew parameter for sampling the serial
#'   interval from the incubation period
#' @param quarantine a `logical` scalar: whether quarantine is in effect, if
#'   `TRUE` then traced contacts are isolated before symptom onset
#' @param prop.asym a nonnegative `numeric` scalar: proportion of cases that
#'   are completely asymptomatic (sublinical) (between 0 and 1)
#' @param delay_shape a positive `numeric` scalar: shape parameter of delay
#'   distribution
#' @param delay_scale a positive `numeric` scalar: scale parameter of delay
#'   distribution
#' @param num.initial.cases a nonnegative `integer` scalar: number of initial
#'   or starting cases which are all assumed to be missed.
#' @param cap_cases a positive `integer` scalar: number of cumulative cases at
#'   which the branching process (simulation) was terminated
#' @param cap_max_days a positive `integer` scalar: stop the simulation when
#'   this many days is reached.
#'
#'
#' @return `data.table` of cases by week, cumulative cases, and the effective
#' reproduction number of the outbreak. `data.table` columns are:
#' * `$week`: `numeric`
#' * `$weekly_cases`: `numeric`
#' * `$cumulative`: `numeric`
#' * `$effective_r0`: `numeric`
#' * `$cases_per_gen`: `list`
#' @export
#'
#' @importFrom data.table rbindlist
#'
#' @examples
#'
#'\dontrun{
#' incfn <- dist_setup(dist_shape = 2.322737,dist_scale = 6.492272)
#' # delay distribution sampling function
#' delayfn <- dist_setup(2, 4)
#' # generate initial cases
#' case_data <- outbreak_setup(num.initial.cases = 5,
#'                             incfn=incfn,
#'                             delayfn = delayfn,
#'                             k=1.95,
#'                             prop.asym=0)
#' # generate next generation of cases
#' case_data <- outbreak_step(case_data = case_data,
#'                            disp.iso = 1,
#'                            disp.com = 0.16,
#'                            disp.subclin = 0.16,
#'                            r0isolated = 0,
#'                            r0community = 2.5,
#'                            r0subclin = 1.25,
#'                            prop.asym = 0,
#'                            incfn = incfn,
#'                            delayfn = delayfn,
#'                            prop.ascertain = 0,
#'                            k = 1.95,
#'                            quarantine = FALSE)
#'}
outbreak_model <- function(num.initial.cases = NULL, prop.ascertain = NULL,
                           cap_max_days = NULL, cap_cases = NULL,
                           r0isolated = NULL, r0community = NULL,
                           r0subclin = NULL, disp.iso = NULL,
                           disp.com = NULL, disp.subclin = NULL,
                           k, delay_shape = NULL,
                           delay_scale = NULL, prop.asym = NULL,
                           quarantine = NULL) {

  # Set up functions to sample from distributions
  # incubation period sampling function
  incfn <- dist_setup(dist_shape = 2.322737,
                      dist_scale = 6.492272)
  # incfn <- dist_setup(dist_shape = 3.303525,dist_scale = 6.68849) # incubation function for ECDC run
  # onset to isolation delay sampling function
  delayfn <- dist_setup(delay_shape,
                        delay_scale)

  # Set initial values for loop indices
  total.cases <- num.initial.cases
  latest.onset <- 0
  extinct <- FALSE

  # Initial setup
  case_data <- outbreak_setup(num.initial.cases = num.initial.cases,
                            incfn = incfn,
                            prop.asym = prop.asym,
                            delayfn = delayfn,
                            k = k)

  # Preallocate
  effective_r0_vect <- c()
  cases_in_gen_vect <- c()


  # Model loop
  while (latest.onset < cap_max_days & total.cases < cap_cases & !extinct) {

    out <- outbreak_step(case_data = case_data,
                             disp.iso = disp.iso,
                             disp.com = disp.com,
                             disp.subclin = disp.subclin,
                             r0isolated = r0isolated,
                             r0community = r0community,
                             r0subclin = r0subclin,
                             incfn = incfn,
                             delayfn = delayfn,
                             prop.ascertain = prop.ascertain,
                             k = k,
                             quarantine = quarantine,
                             prop.asym = prop.asym)


    case_data <- out[[1]]
    effective_r0_vect <- c(effective_r0_vect, out[[2]])
    cases_in_gen_vect <- c(cases_in_gen_vect, out[[3]])
    total.cases <- nrow(case_data)
    latest.onset <- max(case_data$onset)
    extinct <- all(case_data$isolated)
  }

  # Prepare output, group into weeks
  weekly_cases <- case_data[, week := floor(onset / 7)
                            ][, .(weekly_cases = .N), by = week
                              ]
  # maximum outbreak week
  max_week <- floor(cap_max_days / 7)
  # weeks with 0 cases in 0:max_week
  missing_weeks <- (0:max_week)[!(0:max_week %in% weekly_cases$week)]

  # add in missing weeks if any are missing
  if (length(missing_weeks > 0)) {
    weekly_cases <- data.table::rbindlist(list(weekly_cases,
                                               data.table(week = missing_weeks,
                                                          weekly_cases = 0)))
  }
  # order and sum up
  weekly_cases <- weekly_cases[order(week)
                               ][, cumulative := cumsum(weekly_cases)]
  # cut at max_week
  weekly_cases <- weekly_cases[week <= max_week]

  # Add effective R0
  weekly_cases <- weekly_cases[, `:=`(effective_r0 = mean(effective_r0_vect,
                                                          na.rm = TRUE),
                                        cases_per_gen = list(cases_in_gen_vect))]
  # return
  return(weekly_cases)
}
