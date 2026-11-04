#' Aggregate \pkg{ringbp} outbreak simulations by time interval
#'
#' @description
#' Aggregates the simulated cases in the `$outbreak_ts` element of a
#' `<ringbp>` object (returned by [outbreak_model()] or [scenario_sim()]) into
#' the number of new cases, by symptom onset time, in each time interval.
#'
#' This is called internally by [outbreak_model()] using the `aggregation`
#' argument of [sim_opts()], and can also be called by users to aggregate
#' simulation output to a different time interval after the simulation.
#'
#' @details
#' Time in the \pkg{ringbp} model is measured in days since the exposure of
#' the initial cases (day 0), there are no calendar dates. The time intervals
#' are therefore fixed-length blocks of days counted from day 0, a `"weekly"`
#' interval is 7 days and a `"monthly"` interval is 30 days, and are not
#' aligned to calendar weeks or months.
#'
#' Aggregated output contains every interval from 0 up to the interval
#' containing `cap_max_days` (see [sim_opts()]), intervals with no cases are
#' included with zero cases. Cases with symptom onset in a later interval are
#' excluded.
#'
#' Aggregation cannot be undone, so a `<ringbp>` object can only be aggregated
#' if it is disaggregated (`"none"`, i.e. a case line list), or if each new
#' interval is made up of whole intervals of the current aggregation, for
#' example `"daily"` can be aggregated to `"weekly"` or `"monthly"`, and
#' `"weekly"` can be aggregated to `14` (days), but `"weekly"` cannot be
#' aggregated to `"monthly"` or `"daily"`. To be able to aggregate to any
#' interval use `aggregation = "none"` in [sim_opts()] when running the
#' simulation.
#'
#' @param x a `<ringbp>` object: the output of [outbreak_model()] or
#'   [scenario_sim()].
#' @param interval the time interval to aggregate the outbreak simulation to,
#'   accepts the same values as the `aggregation` argument of [sim_opts()].
#' @param ... [dots] not used, extra arguments supplied will be ignored with a
#'   warning.
#'
#' @return A `<ringbp>` object with the `$outbreak_ts` element aggregated to
#'   `interval`. The `$outbreak_stats` element is unchanged. See
#'   [outbreak_model()] for a description of the `$outbreak_ts` columns for
#'   each time interval.
#' @autoglobal
#' @export
#' @importFrom stats aggregate
#'
#' @examples
#' set.seed(1)
#' res <- scenario_sim(
#'   n = 2,
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
#'   interventions = intervention_opts(),
#'   sim = sim_opts(cap_max_days = 70, aggregation = "none")
#' )
#'
#' # case line list
#' res$outbreak_ts
#'
#' # aggregate to daily cases
#' aggregate(res, interval = "daily")
#'
#' # aggregate to fortnightly cases
#' aggregate(res, interval = 14)
aggregate.ringbp <- function(x, interval, ...) {
  chkDots(...)
  new_interval <- match_aggregation(interval)
  outbreak_ts <- x$outbreak_ts

  current_interval <- attr(outbreak_ts, which = "aggregation", exact = TRUE)
  cap_max_days <- attr(outbreak_ts, which = "cap_max_days", exact = TRUE)
  extinct <- attr(outbreak_ts, which = "extinct", exact = TRUE)
  missing_attr <- is.null(current_interval) || is.null(cap_max_days) ||
    is.null(extinct)
  if (missing_attr) {
    stop(
      "`x$outbreak_ts` is missing the `aggregation`, `cap_max_days` or ",
      "`extinct` attribute.\nUse `outbreak_model()` or `scenario_sim()` to ",
      "simulate the outbreak.",
      call. = FALSE
    )
  }

  if (identical(current_interval, new_interval)) {
    return(x)
  }
  if (identical(new_interval, "none")) {
    stop(
      "Cannot disaggregate outbreak data that has already been aggregated.\n",
      "Use `aggregation = \"none\"` in `sim_opts()` to keep the case data.",
      call. = FALSE
    )
  }

  width <- aggregation_as_int(new_interval)
  if (identical(current_interval, "none")) {
    # each row in the line list is a single case
    onset <- outbreak_ts$onset
    cases <- rep(1L, nrow(outbreak_ts))
  } else {
    current_width <- aggregation_as_int(current_interval)
    if (width %% current_width != 0) {
      stop(
        "Cannot aggregate from `", current_interval, "` to `", new_interval,
        "` as each new interval is not made up of whole `", current_interval,
        "` intervals.\n",
        "Use `aggregation = \"none\"` in `sim_opts()` to be able to aggregate ",
        "to any time interval.",
        call. = FALSE
      )
    }
    current_cols <- aggregation_cols(current_interval)
    # first day of each interval
    onset <- outbreak_ts[[current_cols[["time"]]]] * current_width
    cases <- outbreak_ts[[current_cols[["cases"]]]]
  }

  has_sim <- "sim" %in% colnames(outbreak_ts)
  sim_idx <- if (has_sim) outbreak_ts$sim else rep(1L, nrow(outbreak_ts))

  outbreak_cases <- data.table(
    sim = sim_idx,
    time = floor(onset / width),
    cases = cases
  )[, list(cases = sum(cases)), by = c("sim", "time")]

  # maximum outbreak interval
  max_time <- floor(cap_max_days / width)
  # null data.table for zero cases from interval zero to max interval for
  # every replicate (one extinct value per replicate), including replicates
  # without cases in the line list
  agg_cases <- data.table::CJ(
    sim = seq_along(extinct),
    time = 0:max_time,
    sorted = FALSE
  )
  agg_cases[, cases := 0L]
  # splice in/replace with outbreak data for intervals with cases, cases in
  # intervals after max interval are dropped
  agg_cases[outbreak_cases, on = c("sim", "time"), cases := i.cases]
  agg_cases[, cumulative := cumsum(cases), by = sim]

  cols <- aggregation_cols(new_interval)
  data.table::setnames(
    agg_cases,
    old = c("time", "cases"),
    new = c(cols[["time"]], cols[["cases"]])
  )
  if (!has_sim) agg_cases[, sim := NULL]

  # carry over attributes
  for (attr_name in c("extinct", "cap_cases", "cap_max_days")) {
    setattr(
      agg_cases,
      name = attr_name,
      value = attr(outbreak_ts, which = attr_name, exact = TRUE)
    )
  }
  setattr(agg_cases, name = "aggregation", value = new_interval)

  x$outbreak_ts <- agg_cases[]
  x
}

#' Check and standardise a time interval for aggregation
#'
#' @param aggregation a time interval, see the `aggregation` argument of
#'   [sim_opts()].
#'
#' @return A `character` string (`"none"`, `"daily"`, `"weekly"` or
#'   `"monthly"`) or an `integer` scalar (number of days). An error is thrown
#'   if `aggregation` is invalid.
#' @keywords internal
match_aggregation <- function(aggregation) {
  if (is.numeric(aggregation)) {
    checkmate::assert_int(aggregation, lower = 1)
    return(as.integer(aggregation))
  }
  aliases <- c(
    none = "none",
    day = "daily", days = "daily", daily = "daily",
    week = "weekly", weeks = "weekly", weekly = "weekly",
    month = "monthly", months = "monthly", monthly = "monthly"
  )
  checkmate::assert_choice(aggregation, choices = names(aliases))
  aliases[[aggregation]]
}

#' Number of days in an aggregation time interval specified as a `character`
#'
#' @param aggregation a time interval, standardised by [match_aggregation()],
#'   excluding `"none"`.
#'
#' @return An `integer` scalar.
#' @keywords internal
aggregation_as_int <- function(aggregation) {
  if (is.numeric(aggregation)) {
    return(aggregation)
  }
  c(daily = 1L, weekly = 7L, monthly = 30L)[[aggregation]]
}

#' Column names of the aggregated outbreak time series
#'
#' @param aggregation a time interval, standardised by [match_aggregation()],
#'   excluding `"none"`.
#'
#' @return A named `character` vector with the time interval (`time`) and
#'   case count (`cases`) column names.
#' @keywords internal
aggregation_cols <- function(aggregation) {
  if (is.numeric(aggregation)) {
    return(c(time = "period", cases = "period_cases"))
  }
  time <- c(daily = "day", weekly = "week", monthly = "month")[[aggregation]]
  c(time = time, cases = paste0(aggregation, "_cases"))
}
