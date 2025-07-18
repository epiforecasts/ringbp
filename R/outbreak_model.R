#' Run a single instance of the branching process model
#' @author Joel Hellewell
#'
#' @inheritParams outbreak_setup
#' @inheritParams outbreak_step
#' @param sim a `list` with class `<ringbp_sim_opts>`: the simulation control
#'   options for the \pkg{ringbp} model, returned by [sim_opts()]
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
#' offspring <- offspring_opts(
#'   community = \(n) rnbinom(n = n, mu = 2.5, size = 0.16),
#'   isolated = \(n) rnbinom(n = n, mu = 0.5, size = 1),
#'   asymptomatic = \(n) rnbinom(n = n, mu = 2.5, size = 0.16)
#' )
#' delays <- delay_opts(
#'   incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
#'   onset_to_isolation = \(n) rweibull(n = n, shape = 1.65, scale = 4.28)
#' )
#' event_probs <- event_prob_opts(
#'   asymptomatic = 0,
#'   presymptomatic_transmission = 0.5,
#'   symptomatic_ascertained = 0.2
#' )
#' interventions <- intervention_opts(quarantine = FALSE)
#' out <- outbreak_model(
#'   initial_cases = 1,
#'   offspring = offspring,
#'   delays = delays,
#'   event_probs = event_probs,
#'   interventions = interventions,
#'   sim = sim_opts()
#' )
#' out
outbreak_model <- function(initial_cases,
                           offspring,
                           delays,
                           event_probs,
                           interventions,
                           sim) {

  checkmate::assert_number(initial_cases, lower = 1, finite = TRUE)
  checkmate::assert_class(offspring, "ringbp_offspring_opts")
  checkmate::assert_class(delays, "ringbp_delay_opts")
  checkmate::assert_class(event_probs, "ringbp_event_prob_opts")
  checkmate::assert_class(interventions, "ringbp_intervention_opts")
  checkmate::assert_class(sim, "ringbp_sim_opts")

  # Set initial values for loop indices
  total_cases <- initial_cases
  latest_onset <- 0
  extinct <- FALSE

  # Initial setup
  case_data <- outbreak_setup(
    initial_cases = initial_cases,
    delays = delays,
    event_probs = event_probs
  )

  # Preallocate
  effective_r0_vect <- c()
  cases_in_gen_vect <- c()


  # Model loop
  while (latest_onset < sim$cap_max_days &&
         total_cases < sim$cap_cases && !extinct) {

    out <- outbreak_step(
      case_data = case_data,
      offspring = offspring,
      delays = delays,
      event_probs = event_probs,
      interventions = interventions
    )

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
  max_week <- floor(sim$cap_max_days / 7)
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
  weekly_cases[]
}
