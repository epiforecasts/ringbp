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
#' non-isolated cases (must be >0)
#' @param k numeric skew parameter for sampling the serial interval from the
#' incubation period
#' @param delay_shape numeric shape parameter of delay distribution
#' @param delay_scale numeric scale parameter of delay distribution
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
#'
#'\dontrun{
#' incfn <- dist_setup(dist_shape = 2.322737,dist_scale = 6.492272)
#' # delay distribution sampling function
#' delayfn <- dist_setup(2, 4)
#' # generate initial cases
#' case_data <- outbreak_setup(num_initial_cases = 5,
#'                             incfn=incfn,
#'                             delayfn = delayfn,
#'                             k=1.95,
#'                             prop_asym=0)
#' # generate next generation of cases
#' case_data <- outbreak_step(case_data = case_data,
#'                            disp_iso = 1,
#'                            disp_com = 0.16,
#'                            disp_subclin = 0.16,
#'                            r0isolated = 0,
#'                            r0community = 2.5,
#'                            r0subclin = 1.25,
#'                            prop_asym = 0,
#'                            incfn = incfn,
#'                            delayfn = delayfn,
#'                            prop_ascertain = 0,
#'                            k = 1.95,
#'                            quarantine = FALSE)
#'}
outbreak_model <- function(num_initial_cases,
                           prop_ascertain,
                           cap_max_days,
                           cap_cases,
                           r0isolated,
                           r0community,
                           r0subclin = r0community,
                           disp_iso,
                           disp_com,
                           disp_subclin,
                           k,
                           delay_shape,
                           delay_scale,
                           prop_asym,
                           quarantine) {

  # Set up functions to sample from distributions
  # incubation period sampling function
  incfn <- dist_setup(dist_shape = 2.322737,
                      dist_scale = 6.492272)
  # incfn <- dist_setup(dist_shape = 3.303525,dist_scale = 6.68849) # incubation function for ECDC run
  # onset to isolation delay sampling function
  delayfn <- dist_setup(delay_shape,
                        delay_scale)

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
  while (latest_onset < cap_max_days & total_cases < cap_cases & !extinct) {

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
