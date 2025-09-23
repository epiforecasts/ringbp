#' Set up initial cases for branching process
#'
#' @param initial_cases a non-negative `integer` scalar: number of initial
#'   or starting cases which are all assumed to be missed.
#' @inheritParams outbreak_step
#'
#' @return `data.table` of cases in outbreak so far. `data.table` columns are:
#' * `$exposure`: `numeric`
#' * `$asymptomatic`: `logical`
#' * `$caseid`: `integer`
#' * `$infector`: `numeric`
#' * `$missed`: `logical`
#' * `$onset`: `numeric`
#' * `$new_cases`: `logical`
#' * `$isolated_time`: `numeric`
#' * `$isolated`: `logical`
#' @autoglobal
#' @export
#' @importFrom data.table data.table
#' @importFrom stats runif
#'
#' @examples
#' delays <- delay_opts(
#'   incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
#'   onset_to_isolation = \(n) rweibull(n = n, shape = 1.65, scale = 4.28)
#' )
#' event_probs <- event_prob_opts(
#'   asymptomatic = 0,
#'   presymptomatic_transmission = 0.15,
#'   symptomatic_ascertained = 0
#' )
#'
#' # generate initial cases
#' case_data <- outbreak_setup(
#'   initial_cases = 5,
#'   delays = delays,
#'   event_probs = event_probs
#' )
#' case_data
outbreak_setup <- function(initial_cases, delays, event_probs) {

  checkmate::assert_number(initial_cases, lower = 1, finite = TRUE)
  checkmate::assert_class(delays, "ringbp_delay_opts")
  checkmate::assert_class(event_probs, "ringbp_event_prob_opts")

  # Set up table of initial cases
  case_data <- data.table(
    exposure = 0, # Exposure time of 0 for all initial cases
    asymptomatic = runif(initial_cases) < event_probs$asymptomatic,
    caseid = seq_len(initial_cases), # set case id
    infector = 0,
    isolated = FALSE,
    missed = TRUE,
    onset = delays$incubation_period(initial_cases),
    new_cases = NA,
    isolated_time = Inf,
    sampled = FALSE
  )

  # isolate each symptomatic case after an onset-to-isolation delay after
  # their symptom onset time, each case seeds an independent outbreak
  case_data <- case_data[
    asymptomatic == FALSE,
    isolated_time := onset + delays$onset_to_isolation(.N)
  ]

  case_data[]
}
