#' Run a single instance of the branching process model
#' @author Joel Hellewell
#'
#' @param num_initial_cases `integer` The number of initial or starting cases
#' which are all assumed to be missed.
#' @param prop_ascertain numeric proportion of infectious contacts ascertained
#' by contact tracing (must be 0<=x<=1)
#' @param cap_max_days Stop the simulation when this many days is reached.
#' @param cap_cases Stop the simulation when this many cases is reached.
#' @param r0isolated numeric reproduction number for isolated cases (must be >0)
#' @param r0community numeric reproduction number for non-isolated cases
#' (must be >0)
#' @param r0subclin numeric reproduction number for sub-clinical non-isolated
#' cases (must be >0). Default of `r0subclin` is to be equal to clinical
#' cases (`r0community`), this unless specified infectiousnes of subclinical
#' cases will be equal to clinical cases.
#' @param disp_iso numeric dispersion parameter for isolated cases (must be >0)
#' @param disp_com numeric dispersion parameter for non-isolated cases
#' (must be >0)
#' @param disp_subclin numeric dispersion parameter for sub-clincial
#' non-isolated cases (must be >0). Default of `disp_subclin` is to be equal to
#' clinical cases (`disp_com`), this unless specified dispersion for
#' sub-clinical cases will be equal to non-isolated cases.
#' @param k numeric skew parameter for sampling the serial interval from the
#' incubation period
#' @param onset_to_isolation A `function` to generate the onset-to-isolation
#' delay. The function can be defined or anonymous. The function must have a
#' single argument which accepts an integer for the number of
#' onset-to-isolation delays to simulate, and returns the onset-to-isolation
#' delays.
#' @param incubation_period A `function` to generate the incubation period for
#' cases. The function can be defined or anonymous. The function must have a
#' single argument which accepts an integer for the number of incubation periods
#' to simulate, and returns the incubation period(s).
#' @param prop_asym `Numeric` proportion of cases that are completely
#' asymptomatic (subclinical) (between 0 and 1).
#' @param quarantine logical whether quarantine is in effect, if TRUE then
#' traced contacts are isolated before symptom onset
#'
#' @return data.table of cases by week, cumulative cases, and the effective
#' reproduction number of the outreak
#' @export
#'
#' @importFrom data.table rbindlist
#'
#' @examples
#' set.seed(1)
#' out <- outbreak_model(
#'   num_initial_cases = 2,
#'   prop_ascertain = 0.2,
#'   cap_max_days = 350,
#'   cap_cases = 4500,
#'   r0isolated = 0.5,
#'   r0community = 2.5,
#'   r0subclin = 2.5,
#'   disp_iso = 1,
#'   disp_com = 0.16,
#'   disp_subclin = 0.16,
#'   k = 0,
#'   onset_to_isolation = function(x) rweibull(n = x, shape = 1.651524, scale = 4.287786),
#'   incubation_period = function(x) rweibull(n = x, shape = 2.322737, scale = 6.492272),
#'   prop_asym = 0,
#'   quarantine = FALSE
#' )
#' out[]
outbreak_model <- function(num_initial_cases,
                           prop_ascertain,
                           cap_max_days,
                           cap_cases,
                           r0isolated,
                           r0community,
                           r0subclin = r0community,
                           disp_iso,
                           disp_com,
                           disp_subclin = disp_com,
                           k,
                           onset_to_isolation,
                           incubation_period,
                           prop_asym,
                           quarantine) {

  checkmate::assert_integerish(num_initial_cases, len = 1)
  checkmate::assert_number(prop_ascertain, lower = 0, upper = 1)
  checkmate::assert_number(cap_max_days)
  checkmate::assert_integerish(cap_cases, len = 1)
  checkmate::assert_number(r0isolated, lower = 0)
  checkmate::assert_number(r0community, lower = 0)
  checkmate::assert_number(r0subclin, lower = 0)
  checkmate::assert_number(disp_iso, 0)
  checkmate::assert_number(disp_com, 0)
  checkmate::assert_number(disp_subclin, 0)
  checkmate::assert_number(k)
  checkmate::assert_function(onset_to_isolation, nargs = 1)
  checkmate::assert_function(incubation_period, nargs = 1)
  checkmate::assert_number(prop_asym, lower = 0, upper = 1)
  checkmate::assert_logical(quarantine, len = 1)

  # TODO: replace all instances of incfn with incubation_period
  incfn <- incubation_period

  # TODO: replace all instances of delayfn with onset_to_isolation
  delayfn <- onset_to_isolation

  # incfn <- dist_setup(dist_shape = 3.303525,dist_scale = 6.68849) # incubation function for ECDC run

  # Set initial values for loop indices
  total_cases <- num_initial_cases
  latest_onset <- 0
  extinct <- FALSE

  # Initial setup
  case_data <- outbreak_setup(num_initial_cases = num_initial_cases,
                              incfn = incfn,
                              prop_asym = prop_asym,
                              delayfn = delayfn,
                              k = k)

  # Preallocate
  effective_r0_vect <- c()
  cases_in_gen_vect <- c()


  # Model loop
  while (latest_onset < cap_max_days && total_cases < cap_cases && !extinct) {

    out <- outbreak_step(case_data = case_data,
                         disp_iso = disp_iso,
                         disp_com = disp_com,
                         disp_subclin = disp_subclin,
                         r0isolated = r0isolated,
                         r0community = r0community,
                         r0subclin = r0subclin,
                         incfn = incfn,
                         delayfn = delayfn,
                         prop_ascertain = prop_ascertain,
                         k = k,
                         quarantine = quarantine,
                         prop_asym = prop_asym)


    case_data <- out[[1]]
    effective_r0_vect <- c(effective_r0_vect, out[[2]])
    cases_in_gen_vect <- c(cases_in_gen_vect, out[[3]])
    total_cases <- nrow(case_data)
    latest_onset <- max(case_data$onset)
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
