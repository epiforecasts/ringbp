#' Check a distribution function returns valid output and has the correct number
#' of arguments
#'
#' @param func a [function].
#' @param dist_name a `character` string: the name of the distribution function
#'   being passed (e.g. `"incubation_period"`)
#' @param n_req_args a single `numeric`: the number of required arguments
#' @param func_eval_min a single `numeric`: the lower bound of valid numeric
#'   output by the `func` argument (i.e. the minimum of the acceptable value in
#'   the function's codomain). The default is `0` so `func` must return
#'   non-negative values.
#' @param finite Check for only finite values in distribution output, passed
#'   to [checkmate::test_numeric()]. Default is `TRUE`.
#'
#' @return `TRUE` if all the checks pass or an error is thrown if the
#'   distribution function is invalid.
#' @keywords internal
check_dist_func <- function(func,
                            dist_name,
                            n_req_args = 1,
                            func_eval_min = 0,
                            finite = TRUE) {

  checkmate::assert_function(func)
  checkmate::assert_count(n_req_args, positive = TRUE)
  checkmate::assert_count(func_eval_min)
  # using formals(args(fn)) to allow checking args of builtin primitives
  # for which formals(fn) would return NULL and cause the check to error
  # errors non-informatively for specials such as `if`
  valid_func <- checkmate::test_function(func) &&
    sum(mapply(function(x, y) { # nolint undesirable function
      is.name(x) && y != "..."
    }, formals(args(func)), names(formals(args(func))))) == n_req_args &&
    # offspring distribution, incubation_period and onset_to_isolation are
    # non-negative
    checkmate::test_numeric(
      func(1e5),
      lower = func_eval_min,
      finite = finite,
      any.missing = FALSE,
      len = 1e5
    )

  if (!valid_func) {
    stop(
      dist_name, " must be a function with ", n_req_args, " argument(s) that ",
      "returns a vector of non-negative numbers with length equal to the ",
      "input argument."
    )
  }

  TRUE
}

#' Cross-check `*_opts()` lists to run the \pkg{ringbp} model
#'
#' @details Currently the only cross-checking is between
#'   `onset_to_self_isolation` from [delay_opts()] and
#'   `symptomatic_self_isolate` from [event_prob_opts()].
#'
#'   If `delays$onset_to_self_isolation` carries a `cross_checked` attribute
#'   set to `TRUE`, the checks are skipped and `TRUE` is returned immediately.
#'   This lets [scenario_sim()] cross-check the options once and then tag
#'   `delays` so that the per-replicate [outbreak_model()] calls do not
#'   repeat the check (and re-emit its warning) for every simulation replicate.
#'   The attribute is set on a local copy of `delays` (a single
#'   [scenario_sim()] or [outbreak_model()]) call), and does not persist, so
#'   the warning will be repeated in separate [scenario_sim()] or
#'   [outbreak_model()]) calls.
#'
#' @inheritParams outbreak_step
#'
#' @return `TRUE` if all the checks pass or an error or warning is thrown if
#'   the simulation options are incompatible.
#' @keywords internal
cross_check_opts <- function(delays, event_probs) {
  cross_checked <- isTRUE(attr(
    x = delays$onset_to_self_isolation,
    which = "cross_checked"
  ))
  if (cross_checked) {
    return(TRUE)
  }
  no_self_isolation <- all(is.infinite(delays$onset_to_self_isolation(1e5)))
  if (no_self_isolation && event_probs$symptomatic_self_isolate > 0) {
    stop(
      "A non-zero `symptomatic_self_isolate` has been specified in ",
      "`event_prob_opts()`,\n but `onset_to_self_isolation` is generating ",
      "`Inf`.\nPlease specify an `onset_to_self_isolation` function for a ",
      "proportion of symptomatic infections\n to self-isolate.",
      call. = FALSE
    )
  }
  if (!no_self_isolation && event_probs$symptomatic_self_isolate == 0) {
    warning(
      "An `onset_to_self_isolation` delay has been specified in ",
      "`delay_opts()`, but the `symptomatic_self_isolate` in ",
      "`event_prob_opts()` is zero.\nIgnoring `onset_to_self_isolation`.\n",
      "Please specify a non-zero value to `symptomatic_self_isolate` for a ",
      "proportion of symptomatic infections to self-isolate.",
      call. = FALSE
    )
  }
  TRUE
}

#' Check the outbreak data in a `<ringbp>` object has not been invalidated
#'
#' @description
#' Checks that the `$outbreak_ts` element of a `<ringbp>` object is the
#' output of [scenario_sim()] (optionally aggregated with
#' [`aggregate()`][aggregate.ringbp()]), so that outbreak control and
#' extinction can be calculated. An error is thrown if the outbreak data has
#' been invalidated, for example by filtering, reordering or combining rows,
#' changing values, or dropping attributes.
#'
#' @inheritParams detect_control
#'
#' @return `TRUE` (invisibly) if all the checks pass or an error is thrown.
#' @autoglobal
#' @keywords internal
check_scenario <- function(scenario) {
  checkmate::assert_class(scenario, "ringbp")
  outbreak_ts <- scenario$outbreak_ts
  checkmate::assert_data_table(outbreak_ts, .var.name = "scenario$outbreak_ts")

  attr_names <- c("extinct", "aggregation", "cap_cases", "cap_max_days")
  attr_missing <- vapply(
    attr_names,
    function(x) is.null(attr(outbreak_ts, which = x, exact = TRUE)),
    FUN.VALUE = logical(1)
  )
  if (any(attr_missing)) {
    stop(
      "`scenario$outbreak_ts` is missing the ",
      toString(paste0("`", attr_names[attr_missing], "`")), " attribute(s).\n",
      "Use the unmodified output of `scenario_sim()`.",
      call. = FALSE
    )
  }

  n_sim <- length(attr(outbreak_ts, which = "extinct", exact = TRUE))
  aggregation <- attr(outbreak_ts, which = "aggregation", exact = TRUE)
  cap_max_days <- attr(outbreak_ts, which = "cap_max_days", exact = TRUE)
  # column names are determined by the aggregation
  cols <- if (identical(aggregation, "none")) {
    c(time = "onset")
  } else {
    aggregation_cols(aggregation)
  }
  checkmate::assert_names(
    colnames(outbreak_ts),
    identical.to = c("sim", unname(cols), "cumulative"),
    .var.name = "scenario$outbreak_ts"
  )
  if (identical(aggregation, "none")) {
    # case line list: replicates without cases have no rows
    valid <- all(outbreak_ts$sim %in% seq_len(n_sim)) &&
      all(outbreak_ts$onset < cap_max_days) &&
      all(outbreak_ts[, identical(cumulative, seq_len(.N)), by = sim]$V1)
  } else {
    # aggregated data: every time interval for every replicate, in order
    n_time <- cap_max_days / aggregation_as_int(aggregation)
    sim_time <- data.table::CJ(sim = seq_len(n_sim), time = 0:(n_time - 1L))
    valid <- isTRUE(all.equal(
      outbreak_ts[, c("sim", cols[["time"]]), with = FALSE],
      sim_time,
      check.attributes = FALSE
    )) &&
      all(
        outbreak_ts[
          , cumulative == cumsum(get(cols[["cases"]])), by = sim
        ]$V1
      )
  }
  if (!valid) {
    stop(
      "`scenario$outbreak_ts` has been modified, control and extinction ",
      "cannot be calculated on modified outbreak data.\n",
      "Use the unmodified output of `scenario_sim()` or `aggregate()`.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
