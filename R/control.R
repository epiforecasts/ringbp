#' Outbreak control and extinction functions
#'
#' @description
#' `control_prob()`: Calculate proportion of runs that have a controlled
#'   outbreak
#'
#' `detect_control()`: Calculate whether outbreaks were controlled or not
#'
#' `extinct_prob()`: Calculate proportion of runs with an extinct outbreak (a
#'   special case of `control_prob()` with `control_threshold = 0`)
#'
#' `detect_extinct()`: Calculate whether outbreaks went extinct or not (a
#'   special case of `detect_control()` with `control_threshold = 0`)
#'
#' @details
#' `extinct_prob()` and `detect_extinct()` were added to the package before
#' `control_prob()` and `detect_control()`. The extinction functions are
#' special cases of the more general control functions, fixing
#' `control_threshold = 0`. They remain in the package for backwards
#' compatibility and convenience to calculate extinction metrics.
#'
#' The data passed to `scenario` has to be produced by [scenario_sim()].
#' The output of [outbreak_model()] is not supported, as it does not contain
#' the `sim` column, which is only appended in [scenario_sim()].
#'
#' ***Warning***: the `$outbreak_ts` element of the [scenario_sim()] output
#' carries attributes which are used by all four functions: `extinct` is used
#' when `control_time`/`extinction_time` is `NULL`, and `aggregation` and
#' `cap_cases` are used when `control_time`/`extinction_time` is specified.
#' Therefore if you modify the output of [scenario_sim()] before passing it to
#' any of these functions be careful not to drop the attributes (e.g. from
#' subsetting the `data.table`).
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
#' @param scenario a `<ringbp>` object: the output of [scenario_sim()],
#'   containing the `$outbreak_ts` `data.table` of cases used to determine
#'   control/extinction.
#'
#' @param control_time,extinction_time a non-negative `integer` scalar,
#'   `integer` vector, or `NULL` (default): the time (zero indexed) by which to
#'   test if an outbreak has been controlled / gone extinct, in the time
#'   interval of the aggregated `scenario` (e.g. weeks for weekly aggregated
#'   data).
#'   `control_time` is used by [control_prob()] and [detect_control()] and
#'   tests whether the cases in each time interval are at most
#'   `control_threshold`; `extinction_time` is used by [extinct_prob()]
#'   and [detect_extinct()] and is the same test fixed to
#'   `control_threshold = 0` (i.e. no new cases). Both arguments accept:
#'   * By default (`NULL`) the extinction status is stored in the output of
#'     `scenario_sim()` which is supplied to the `scenario` argument. If
#'     `control_time`/`extinction_time` is not specified or specified as
#'     `NULL` then the pre-computed extinction status from the outbreak
#'     simulation will be used. This is defined as all infectious cases have
#'     had the opportunity to transmit but no new cases are generated. This is
#'     only valid when `control_threshold = 0`; when `control_threshold > 0` a
#'     `control_time` is required and the default will error.
#'   * A single `integer` to test if control/extinction has occurred by this
#'     time. For example, for weekly data, `extinction_time = 5` tests whether
#'     the outbreak is extinct by week 5 (inclusive) until the end of the
#'     outbreak, and `control_time = 5` with `control_threshold = 10` tests
#'     whether weekly cases stay at or below 10 from week 5 until the end of
#'     the outbreak.
#'   * An `integer` vector of length two can be supplied to provide the lower
#'     and upper bounds (inclusive) of the time range to test for whether
#'     control/extinction occurred by this window. For example, for weekly
#'     data, `extinction_time = c(5, 10)` will test whether the outbreak went
#'     extinct by week 5 and there were no new cases between weeks 5 and 10
#'     (inclusive).
#'   * An `integer` vector of length _n_ can be supplied to provide the times
#'     to test for whether control/extinction occurred by this window. For
#'     example, for weekly data, `extinction_time = 12:16` will test that there
#'     are no new infections between 12 and 16 weeks after the initial cases
#'     (inclusive). These integer sequences will most likely be contiguous but
#'     the function does allow non-contiguous integer sequences.
#'
#'   If control/extinction occurs before the `control_time`/`extinction_time`
#'   window then the outbreak is considered controlled/extinct, however, if
#'   control/extinction occurs within the window it is not, because at least
#'   one time interval within the window exceeded the `control_threshold`.
#'   Therefore, a single `integer` can be thought of as "_has the outbreak
#'   been controlled / gone extinct by time X_".
#'
#' @param control_threshold a non-negative `integer` scalar: the threshold
#'   number of cases in each time interval of the aggregated `scenario` (e.g.
#'   weekly cases for weekly aggregated data) at or below which the outbreak
#'   is classified as controlled. By default it is `0`, which is outbreak
#'   extinction. Values greater than zero correspond to control defined under
#'   outbreak suppression strategies, e.g. if weekly cases do not exceed 100
#'   (`control_threshold = 100`), then the outbreak is said to be under
#'   control in that period. Outbreaks that reach the `cap_cases` (see
#'   [sim_opts()]) are never classified as controlled.
#'
#' @importFrom data.table setDT fifelse data.table
#'
#' @return
#' `control_prob()`: a single `numeric` with the probability of control
#'
#' `detect_control()`: a `data.table`, with two `integer` columns `sim` and
#' `control`, for a binary classification of whether the outbreak was
#' controlled in each simulation replicate. `1` is an outbreak that was
#' controlled, `0` if not.
#'
#' `extinct_prob()`: a single `numeric` with the probability of extinction
#'
#' `detect_extinct()`: a `data.table`, with two `integer` columns `sim` and
#' `extinct`, for a binary classification of whether the outbreak went extinct
#' in each simulation replicate. `1` is an outbreak that went extinct, `0` if
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
#' # calculate probability of extinction
#' extinct_prob(res)
#'
#' # determine if each outbreak simulation replicate has gone extinct
#' detect_extinct(res)
#'
#' # calculate extinction in the last 2 weeks of the simulated outbreak
#' # (i.e. the penultimate and last week of the outbreak)
#' extinct_prob(res, extinction_time = max(res$outbreak_ts$week) - 1)
#'
#' # calculate extinction as no new cases between weeks 12 and 16 of outbreak
#' extinct_prob(res, extinction_time = 12:16)
#'
#' # calculate probability of control under a suppression strategy: weekly
#' # cases at or below 100 in the last 2 weeks of the outbreak
#' control_prob(
#'   res,
#'   control_time = max(res$outbreak_ts$week) - 1,
#'   control_threshold = 100
#' )
#'
#' # determine if each outbreak simulation replicate has been controlled,
#' # defined as fortnightly cases at or below 100 from fortnight 20 onwards
#' fortnightly <- aggregate(res, interval = 14)
#' detect_control(fortnightly, control_time = 20, control_threshold = 100)
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
  sum(control_runs$control) / max(control_runs$sim)
}

#' @rdname control
#' @autoglobal
#' @export
detect_control <- function(scenario,
                           control_time = NULL,
                           control_threshold = 0) {
  checkmate::assert_class(scenario, "ringbp")
  checkmate::assert_names(
    colnames(scenario$outbreak_ts),
    must.include = "sim",
    .var.name = "scenario$outbreak_ts"
  )
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
    if (!is.null(extinct)) {
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
    } else {
      stop(
        "`control_time`/`extinction_time` not specified and `scenario` is ",
        "missing the `extinct` attribute.\n Use `scenario_sim()` to simulate ",
        "`scenario`, or specify `control_time`/`extinction_time`.",
        call. = FALSE
      )
    }
  }

  checkmate::assert_integerish(control_time, min.len = 1, any.missing = FALSE)

  aggregation <- attr(scenario, which = "aggregation", exact = TRUE)
  stopifnot(
    "`scenario` is missing the `aggregation` attribute.
    Use `scenario_sim()` to simulate `scenario`" = !is.null(aggregation)
  )
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
  stopifnot(
    "`scenario` is missing the `cap_cases` attribute.
    Use `scenario_sim()` to simulate `scenario`" = !is.null(cap_cases)
  )

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
  data.table::setnames(extinct, old = "control", new = "extinct")
  extinct
}
