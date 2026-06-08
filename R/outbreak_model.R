#' Run a single instance of the branching process model
#'
#' @inheritParams outbreak_setup
#' @inheritParams outbreak_step
#' @param sim a `list` with class `<ringbp_sim_opts>`: the simulation control
#'   options for the \pkg{ringbp} model, returned by [sim_opts()]
#'
#' @return A `list` with 2 `data.table` elements:
#' 1. `$outbreak_ts`: the results for a single outbreak simulation. The
#'    `data.table` has columns:
#'    * `week`: the week in the simulation (`integer`)
#'    * `weekly_cases`: the number of new cases that week (`integer`)
#'    * `cumulative`: the cumulative cases (`integer`)
#' 2. `$outbreak_stats`: the summary statistics for the outbreak simulation. The
#'    `data.table` has columns:
#'    * `effective_r0`: the effective reproduction rate for the
#'    whole simulation (`numeric`)
#'    * `cases_per_gen`: the cases per generation (`list`)
#'
#' The `$outbreak_ts` element also carries an `extinct` attribute: a `logical`
#' recording whether the outbreak went extinct. See [extinction] functions for
#' the definition of extinction.
#' @autoglobal
#' @export
#'
#' @importFrom data.table rbindlist setattr
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
#'   symptomatic_traced = 0.2
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
  cross_check_opts(delays, event_probs)

  # Initial setup
  case_data <- outbreak_setup(
    initial_cases = initial_cases,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions
  )

  # Preallocate
  effective_r0_vect <- c()
  cases_in_gen_vect <- c()


  # Model loop
  while (outbreak_continue(case_data, sim)) {

    out <- outbreak_step(
      case_data = case_data,
      offspring = offspring,
      delays = delays,
      event_probs = event_probs,
      interventions = interventions
    )

    case_data <- out$cases
    effective_r0_vect <- c(effective_r0_vect, out$effective_r0)
    cases_in_gen_vect <- c(cases_in_gen_vect, out$cases_in_gen)
  }

  # only warn if non-zero latent period and any transmission
  if (delays$latent_period > 0 && nrow(case_data) > 1) {
    # self-join to compare exposure and infector onset times by row
    prop_presymptomatic_transmission <- case_data[
      , list(exposure, caseid, infector, onset)
    ][case_data[, list(infector_id = seq_len(.N), onset)],
      on = c("infector" = "infector_id"),
      nomatch = NULL
    ][, mean(exposure < i.onset)]

    warning(
      "The proportion of presymptomatic transmission supplied is: ",
      event_probs$presymptomatic_transmission, "\n",
      "The realised proportion of presymptomatic transmission is: ",
      signif(prop_presymptomatic_transmission, digits = 3),
      call. = FALSE
    )
  }

  # Prepare output, group into weeks
  weekly_cases_outbreak <- case_data[, week := floor(onset / 7)
                            ][, list(weekly_cases = .N), by = week
                              ]
  # maximum outbreak week
  max_week <- floor(sim$cap_max_days / 7)
  # null data.table for zero cases from week zero to max week
  weekly_cases <- data.table::data.table(week = 0:max_week, weekly_cases = 0L)
  # splice in/replace with outbreak data for weeks with cases
  weekly_cases[weekly_cases_outbreak, on = "week", weekly_cases := i.weekly_cases]

  # order and sum up
  weekly_cases <- weekly_cases[order(week)
                               ][, cumulative := cumsum(weekly_cases)]
  # cut at max_week
  weekly_cases <- weekly_cases[week <= max_week]

  # effective_r0_vect and cases_in_gen_vect grow together so only check one
  if (length(effective_r0_vect) == 0) {
    warning(
      "The outbreak simulation ran for zero generations (i.e. no ",
      "transmission from initial cases) because `initial_cases` is ",
      "greater than or equal to `cap_cases`.\nSee `?sim_opts()` for help.",
      call. = FALSE
    )
    effective_r0_vect <- NA_real_
    cases_in_gen_vect <- NA_real_
  }

  setattr(weekly_cases, name = "extinct", value = all(case_data$sampled))

  outbreak_stats <- data.table(
    effective_r0 = mean(effective_r0_vect, na.rm = TRUE),
    cases_per_gen = list(cases_in_gen_vect)
  )

  # return
  list(
    outbreak_ts = weekly_cases[],
    outbreak_stats = outbreak_stats
  )
}
