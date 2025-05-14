
#' Run a single instance of the branching process model
#' @author Joel Hellewell
#' @param disp_isolated a positive `numeric` scalar: dispersion parameter for
#'   isolated cases (must be >0)
#' @param disp_community a positive `numeric` scalar: dispersion parameter for
#'   non-isolated cases (must be >0)
#' @param disp_asymptomatic a positive `numeric` scalar: dispersion parameter
#'   for sub-clincial non-isolated cases (must be >0)
#' @param r0isolated a positive `numeric` scalar: reproduction number for
#'   isolated cases (must be >0)
#' @param r0community a positive `numeric` scalar: reproduction number for
#'   non-isolated cases (must be >0)
#' @param r0asymptomatic a positive `numeric` scalar: reproduction number for
#'   sub-clinical non-isolated cases (must be >0)
#' @param prop_ascertain a nonnegative `numeric` scalar: proportion of
#'   infectious contacts ascertained by contact tracing (must be 0<=x<=1)
#' @param k a `numeric` scalar: skew parameter for sampling the serial
#'   interval from the incubation period
#' @param quarantine a `logical` scalar: whether quarantine is in effect, if
#'   `TRUE` then traced contacts are isolated before symptom onset; defaults to
#'   `FALSE`
#' @param prop_asymptomatic a nonnegative `numeric` scalar: proportion of cases
#'   that are completely asymptomatic (sublinical) (between 0 and 1)
#' @param onset_to_isolation a `function`: a random number generating
#'   `function` that accepts a single `integer` argument specifying the
#'   length of the `function` output.
#' @param incubation_period a `function`: a random number generating
#'   `function` that samples from incubation period distribution, the
#'   `function` accepts a single `integer` argument specifying the number of
#'   times to sample the incubation period (i.e. length of the `function`
#'   output).
#' @param num_initial_cases a nonnegative `integer` scalar: number of initial
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
#' @autoglobal
#' @export
#'
#' @importFrom data.table rbindlist
#'
#' @examples
#' set.seed(1)
#' out <- outbreak_model(
#'   num_initial_cases = 1,
#'   prop_ascertain = 0.2,
#'   cap_max_days = 350,
#'   cap_cases = 4500,
#'   r0isolated = 0.5,
#'   r0community = 2.5,
#'   r0asymptomatic = 2.5,
#'   disp_isolated = 1,
#'   disp_community = 0.16,
#'   disp_asymptomatic = 0.16,
#'   k = 0,
#'   onset_to_isolation = \(x) rweibull(n = x, shape = 1.65, scale = 4.28),
#'   incubation_period = \(x) rweibull(n = x, shape = 2.32, scale = 6.49),
#'   prop_asymptomatic = 0,
#'   quarantine = FALSE
#' )
#' out
outbreak_model <- function(num_initial_cases = NULL, prop_ascertain = NULL,
                           cap_max_days = NULL, cap_cases = NULL,
                           r0isolated = NULL, r0community = NULL,
                           r0asymptomatic = NULL, disp_isolated = NULL,
                           disp_community = NULL, disp_asymptomatic = NULL,
                           k, onset_to_isolation, incubation_period,
                           prop_asymptomatic = NULL, quarantine = FALSE) {

  # Set initial values for loop indices
  total_cases <- num_initial_cases
  latest_onset <- 0
  extinct <- FALSE

  # Initial setup
  case_data <- outbreak_setup(num_initial_cases = num_initial_cases,
                              incubation_period = incubation_period,
                              onset_to_isolation = onset_to_isolation,
                              k = k,
                              prop_asymptomatic = prop_asymptomatic)

  # Preallocate
  effective_r0_vect <- c()
  cases_in_gen_vect <- c()


  # Model loop
  while (latest_onset < cap_max_days & total_cases < cap_cases & !extinct) {

    out <- outbreak_step(case_data = case_data,
                             disp_isolated = disp_isolated,
                             disp_community = disp_community,
                             disp_asymptomatic = disp_asymptomatic,
                             r0isolated = r0isolated,
                             r0community = r0community,
                             r0asymptomatic = r0asymptomatic,
                             incubation_period = incubation_period,
                             onset_to_isolation = onset_to_isolation,
                             prop_ascertain = prop_ascertain,
                             k = k,
                             quarantine = quarantine,
                             prop_asymptomatic = prop_asymptomatic)


    case_data <- out[[1]]
    effective_r0_vect <- c(effective_r0_vect, out[[2]])
    cases_in_gen_vect <- c(cases_in_gen_vect, out[[3]])
    total_cases <- nrow(case_data)
    latest_onset <- max(case_data$onset)
    extinct <- all(case_data$isolated)
  }

  # Prepare output, group into weeks
  weekly_cases <- case_data[, week := floor(onset / 7)
                            ][, list(weekly_cases = .N), by = week
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
  return(weekly_cases[])
}
