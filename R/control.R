#' Outbreak control and extinction functions
#'
#' @description
#' Summarise outbreaks simulated by [scenario_sim()] by whether they were
#' controlled or went extinct. An outbreak is _controlled_ when the number of
#' cases in every time interval of a time window is at or below
#' `control_threshold`, and _extinct_ when there are no new cases
#' (`control_threshold = 0`). See _Definition of control_ in Details.
#' A time window can be defined by either just a starting time, in which case
#' the window is all periods from that time on, or as a `[start, end]` interval
#'
#' These functions answer questions such as:
#' * Did the outbreak go extinct in the simulation? Use
#'   `extinct_prob(scenario)`.
#' * Has the outbreak gone extinct by time `t`, with no new cases from `t`
#'   until the end of the simulation? Use
#'   `extinct_prob(scenario, extinction_time = t)`.
#' * Has the outbreak been controlled by time `t`, with at most `n` cases in
#'   each time interval from `t` until the end of the simulation? Use
#'   `control_prob(scenario, control_time = t, control_threshold = n)`.
#' * Were there at most `n` cases in each time interval between times `a` and
#'   `b`, regardless of the number of cases after `b`? Use
#'   `control_prob(scenario, control_time = c(a, b), control_threshold = n)`.
#'
#' The `*_prob()` functions return the proportion of simulation replicates
#' for which the answer is yes, and the `detect_*()` functions return the
#' answer for each simulation replicate:
#'
#' `control_prob()` / `extinct_prob()`: Calculate proportion of runs that have
#'   a controlled / extinct outbreak (`extinct_prob()` is a special case of
#'   `control_prob()` with `control_threshold = 0`)
#'
#' `detect_control()` / `detect_extinct()`: Calculate whether outbreaks were
#'   controlled / went extinct or not (`detect_extinct()` is a special case of
#'   `detect_control()` with `control_threshold = 0`)
#'
#' @details
#' ## Definition of control
#'
#' An outbreak is controlled in a time window if the number of cases in
#' **every** time interval of the window is at or below `control_threshold`,
#' and the outbreak has not reached `cap_cases` (see [sim_opts()]) by the end
#' of the window. Control is a count below a threshold, it does not require
#' the outbreak to end, and extinction is the special case of no new cases
#' (`control_threshold = 0`). If the number of cases falls below
#' `control_threshold` and then rises above it again within the window, the
#' outbreak is not controlled.
#'
#' The window is set by `control_time` / `extinction_time`, and its form
#' determines whether cases rising above `control_threshold` after a period
#' below it means the outbreak is not controlled:
#' * `NULL` (default, only valid for `control_threshold = 0`): uses the
#'   extinction status from the simulation, where all infectious cases have
#'   had the opportunity to transmit but no new cases were generated, so the
#'   number of cases cannot rise again.
#' * A single `integer` `t`: the window is from `t` until the end of the
#'   simulation, so cases rising above `control_threshold` at any time from
#'   `t` means the outbreak is not controlled. This tests whether the
#'   outbreak _stayed_ controlled from `t`, but only until the end of the
#'   simulation (`cap_max_days`, see [sim_opts()]), not indefinitely.
#' * An `integer` vector, e.g. `c(a, b)` or `12:16`: the window is only these
#'   times, so cases rising above `control_threshold` within the window means
#'   the outbreak is not controlled, but cases rising above it after the
#'   window does not. This tests whether the outbreak was controlled
#'   _throughout_ the window.
#'
#' ## Time intervals
#'
#' The `control_time` and `extinction_time` are in the time interval the
#' `scenario` is aggregated to (set by the `aggregation` argument in
#' [sim_opts()], or by [`aggregate()`][aggregate.ringbp()]), for example weeks
#' for the default `"weekly"` aggregation, or days for `"daily"` aggregation.
#' The `control_threshold` is the number of cases in each of these time
#' intervals, for example weekly cases for `"weekly"` aggregation. To use a
#' `control_threshold` for a different time interval, aggregate the `scenario`
#' with [`aggregate()`][aggregate.ringbp()] before calculating control. If the
#' `scenario` is not aggregated (`aggregation = "none"`) it is aggregated to
#' daily cases, with a warning, and `control_time`/`extinction_time` is in
#' days and `control_threshold` is in daily cases.
#'
#' ## Input data
#'
#' The data passed to `scenario` has to be produced by [scenario_sim()].
#' The output of [outbreak_model()] is not supported, as it does not contain
#' the `sim` column, which is only appended in [scenario_sim()].
#'
#' ***Warning***: control and extinction are calculated from the
#' `$outbreak_ts` element of the [scenario_sim()] output and its attributes
#' (`extinct`, `aggregation`, `cap_cases` and `cap_max_days`), so it must not
#' be modified before it is passed to these functions. These functions error
#' if they detect that `$outbreak_ts` has been modified, for example by
#' filtering, reordering or combining rows, changing values, or dropping
#' attributes. To change the time interval of the outbreak data use
#' [`aggregate()`][aggregate.ringbp()].
#'
#' @param scenario a `<ringbp>` object: the output of [scenario_sim()],
#'   containing the `$outbreak_ts` `data.table` of cases used to determine
#'   control/extinction.
#'
#' @param control_time,extinction_time a non-negative `integer` scalar,
#'   `integer` vector, or `NULL` (default): the time window (zero indexed)
#'   over which to test whether an outbreak has been controlled / gone
#'   extinct, in the time interval of the aggregated `scenario` (e.g. weeks
#'   for weekly aggregated data). `control_time` is used by [control_prob()]
#'   and [detect_control()], and `extinction_time` is used by
#'   [extinct_prob()] and [detect_extinct()]. Both arguments accept:
#'   * `NULL` (default): use the extinction status from the simulation. Only
#'     valid when `control_threshold = 0`; when `control_threshold > 0` a
#'     `control_time` is required and the default will error.
#'   * A single `integer`: the window is from this time until the end of the
#'     simulation.
#'   * An `integer` vector of length two: the lower and upper bounds
#'     (inclusive) of the window.
#'   * An `integer` vector of length _n_ (e.g. `12:16`): the times in the
#'     window. These will most likely be contiguous but non-contiguous times
#'     are allowed.
#'
#'   See _Definition of control_ in Details for how each form of window is
#'   used to classify control / extinction.
#'
#' @param control_threshold a non-negative `integer` scalar: the threshold
#'   number of cases in each time interval of the aggregated `scenario` (e.g.
#'   weekly cases for weekly aggregated data) at or below which the outbreak
#'   is classified as controlled. By default it is `0`, which is outbreak
#'   extinction. Values greater than zero correspond to control defined under
#'   outbreak suppression strategies, e.g. if weekly cases do not exceed 100
#'   (`control_threshold = 100`), then the outbreak is said to be under
#'   control in that period. Outbreaks that have reached `cap_cases` (see
#'   [sim_opts()]) by the end of the window are not classified as controlled.
#'
#' @importFrom data.table setDT fifelse data.table
#'
#' @return
#' `control_prob()` / `extinct_prob()`: a single `numeric` with the
#' probability of control / extinction
#'
#' `detect_control()` / `detect_extinct()`: a `data.table`, with two `integer`
#' columns `sim` and `control` / `extinct`, for a binary classification of
#' whether the outbreak was controlled / went extinct in each simulation
#' replicate. `1` is an outbreak that was controlled / went extinct, `0` if
#' not.
#'
#' @examples
#' res <- scenario_sim(
#'   n = 10,
#'   initial_cases = 1,
#'   offspring = offspring_opts(
#'     community = \(n) rnbinom(n = n, mu = 2.5, size = 0.16),
#'     isolated = \(n) rnbinom(n = n, mu = 0.5, size = 1)
#'   ),
#'   delays = delay_opts(
#'     incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
#'     onset_to_isolation = \(n) rweibull(n = n, shape = 1.65, scale = 4.28)
#'   ),
#'   event_probs = event_prob_opts(
#'     asymptomatic = 0,
#'     presymptomatic_transmission = 0.5,
#'     symptomatic_traced = 0.2
#'   ),
#'   interventions = intervention_opts(quarantine = FALSE),
#'   sim = sim_opts(cap_max_days = 350, cap_cases = 4500)
#' )
#'
#' # Did the outbreak go extinct in the simulation?
#' # proportion of simulation replicates
#' extinct_prob(res)
#' # each simulation replicate
#' detect_extinct(res)
#'
#' # Has the outbreak gone extinct by week 20, with no new cases from week 20
#' # until the end of the simulation?
#' extinct_prob(res, extinction_time = 20)
#'
#' # Has the outbreak been controlled by week 20, with at most 100 cases in
#' # each week from week 20 until the end of the simulation?
#' control_prob(res, control_time = 20, control_threshold = 100)
#'
#' # Were there at most 100 cases in each week between weeks 10 and 20,
#' # regardless of the number of cases after week 20?
#' control_prob(res, control_time = c(10, 20), control_threshold = 100)
#'
#' # Were there at most 100 cases in each fortnight between fortnights 5 and
#' # 10, regardless of the number of cases after fortnight 10? For each
#' # simulation replicate, after aggregating the weekly data to fortnights
#' fortnightly <- aggregate(res, interval = 14)
#' detect_control(fortnightly, control_time = c(5, 10), control_threshold = 100)
#' @name control
NULL

#' @rdname control
#' @export
control_prob <- function(scenario,
                         control_time = NULL,
                         control_threshold = 0) {
  control_runs <- detect_control(
    scenario = scenario,
    control_time = control_time,
    control_threshold = control_threshold
  )
  mean(control_runs$control)
}

#' @rdname control
#' @autoglobal
#' @export
detect_control <- function(scenario,
                           control_time = NULL,
                           control_threshold = 0) {
  check_scenario(scenario)
  checkmate::assert_integerish(
    control_threshold, len = 1, lower = 0, any.missing = FALSE
  )

  outbreak <- scenario
  scenario <- scenario$outbreak_ts
  extinct <- attr(scenario, which = "extinct", exact = TRUE)
  if (is.null(control_time)) {
    if (control_threshold > 0) {
      stop(
        "`control_threshold` > 0 but `control_time` is `NULL`.\n Please ",
        "specify `control_time`.",
        call. = FALSE
      )
    }
    message(
      "Calculating extinction using the extinction status from ",
      "the simulation."
    )
    return(
      data.table(
        # one extinct value per replicate, including replicates without
        # cases in a case line list
        sim = seq_along(extinct),
        control = as.integer(extinct)
      )
    )
  }

  checkmate::assert_integerish(control_time, min.len = 1, any.missing = FALSE)

  aggregation <- attr(scenario, which = "aggregation", exact = TRUE)
  # control/extinction for case line list is calculated on daily cases
  if (identical(aggregation, "none")) {
    warning(
      "`scenario` is not aggregated (`aggregation = \"none\"`), it will be ",
      "aggregated to daily cases to calculate control/extinction.\n",
      "`control_time`/`extinction_time` is in days.",
      call. = FALSE
    )
    aggregation <- "daily"
    scenario <- aggregate(outbreak, interval = aggregation)$outbreak_ts
  }
  cap_cases <- attr(scenario, which = "cap_cases", exact = TRUE)

  cols <- aggregation_cols(aggregation)
  time_col <- cols[["time"]]
  cases_col <- cols[["cases"]]
  time_unit <- if (is.numeric(aggregation)) {
    paste0(aggregation, "-day periods")
  } else {
    paste0(time_col, "s")
  }

  if (length(control_time) == 1) {
    control_time <- control_time:max(scenario[[time_col]])
  } else if (length(control_time) == 2) {
    control_time <- min(control_time):max(control_time)
  }
  stopifnot(
    "`control_time`/`extinction_time` not in simulated outbreak data" =
      all(control_time %in% scenario[[time_col]])
  )
  if (control_threshold == 0) {
    message(
      "Calculating extinction as no new cases within ", time_unit, ": ",
      min(control_time), " to ", max(control_time), " (inclusive)."
    )
  } else {
    message(
      "Calculating control as cases <= ", control_threshold, " within ",
      time_unit, ": ", min(control_time), " to ", max(control_time),
      " (inclusive)."
    )
  }

  scenario <- setDT(scenario)
  scenario <- scenario[get(time_col) %in% control_time]
  scenario[, list(
    control = fifelse(
      all(get(cases_col) <= control_threshold & cumulative < cap_cases), 1L, 0L
    )
  ), by = sim][]
}

#' @rdname control
#' @export
extinct_prob <- function(scenario,
                         extinction_time = NULL) {
  control_prob(
    scenario = scenario,
    control_time = extinction_time,
    control_threshold = 0
  )
}

#' @rdname control
#' @export
detect_extinct <- function(scenario,
                           extinction_time = NULL) {
  extinct <- detect_control(
    scenario = scenario,
    control_time = extinction_time,
    control_threshold = 0
  )
  data.table::setnames(extinct, old = "control", new = "extinct")[]
}
