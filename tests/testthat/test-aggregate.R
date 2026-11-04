offspring <- offspring_opts(
  community = \(n) rnbinom(n = n, mu = 2.5, size = 0.16),
  isolated = \(n) rnbinom(n = n, mu = 0.5, size = 1)
)
delays <- delay_opts(
  incubation_period = \(n) rweibull(n = n, shape = 2.32, scale = 6.49),
  onset_to_isolation = \(n) rweibull(n = n, shape = 1.65, scale = 4.28)
)
event_probs <- event_prob_opts(
  asymptomatic = 0.1,
  presymptomatic_transmission = 0.5,
  symptomatic_traced = 0.2
)
interventions <- intervention_opts()

# case line list with 12 cases in replicate 1 (symptom onset days 4 to 29) and
# 7 cases in replicate 2 (symptom onset days 3 to 12)
set.seed(118)
line_list <- scenario_sim(
  n = 2,
  initial_cases = 2,
  offspring = offspring,
  delays = delays,
  event_probs = event_probs,
  interventions = interventions,
  sim = sim_opts(cap_max_days = 30, aggregation = "none")
)

test_that("aggregate returns a <ringbp> object", {
  res <- aggregate(line_list, interval = "weekly")
  expect_s3_class(res, "ringbp", exact = TRUE)
  expect_named(res, c("outbreak_ts", "outbreak_stats"))
  expect_s3_class(
    res$outbreak_ts, class = c("data.table", "data.frame"), exact = TRUE
  )
  # outbreak_stats is unchanged
  expect_identical(res$outbreak_stats, line_list$outbreak_stats)
  # output columns are integers
  for (interval in list("daily", "weekly", "monthly", 14)) {
    res <- aggregate(line_list, interval = interval)$outbreak_ts
    expect_identical(
      unname(vapply(res, class, FUN.VALUE = character(1))),
      rep("integer", 4)
    )
  }
  # attributes are set and preserved
  res <- aggregate(line_list, interval = "daily")$outbreak_ts
  expect_identical(attr(res, "aggregation"), "daily")
  expect_identical(attr(res, "extinct"), c(FALSE, TRUE))
  expect_identical(attr(res, "cap_cases"), 5000)
  expect_identical(attr(res, "cap_max_days"), 30)
  res <- aggregate(line_list, interval = 14)$outbreak_ts
  expect_identical(attr(res, "aggregation"), 14L)
})

test_that("aggregate to daily works as expected", {
  res <- aggregate(line_list, interval = "daily")$outbreak_ts
  expect_named(res, c("sim", "day", "daily_cases", "cumulative"))
  expect_identical(res$sim, rep(1:2, each = 31))
  expect_identical(res$day, rep(0:30, times = 2))
  daily_cases_sim1 <- as.integer(c(
    0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
    1, 0, 0, 3, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0
  ))
  daily_cases_sim2 <- as.integer(c(
    0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, rep(0, 18)
  ))
  expect_identical(res[sim == 1, daily_cases], daily_cases_sim1)
  expect_identical(res[sim == 2, daily_cases], daily_cases_sim2)
  expect_identical(res[sim == 1, cumulative], cumsum(daily_cases_sim1))
  expect_identical(res[sim == 2, cumulative], cumsum(daily_cases_sim2))
})

test_that("aggregate to weekly works as expected", {
  res <- aggregate(line_list, interval = "weekly")$outbreak_ts
  expect_named(res, c("sim", "week", "weekly_cases", "cumulative"))
  expect_identical(res$sim, rep(1:2, each = 5))
  expect_identical(res$week, rep(0:4, times = 2))
  expect_identical(res$weekly_cases, c(2L, 1L, 5L, 2L, 2L, 3L, 4L, 0L, 0L, 0L))
  expect_identical(res$cumulative, c(2L, 3L, 8L, 10L, 12L, 3L, 7L, 7L, 7L, 7L))
})

test_that("aggregate to monthly works as expected", {
  res <- aggregate(line_list, interval = "monthly")$outbreak_ts
  expect_named(res, c("sim", "month", "monthly_cases", "cumulative"))
  expect_identical(res$sim, rep(1:2, each = 2))
  expect_identical(res$month, rep(0:1, times = 2))
  expect_identical(res$monthly_cases, c(12L, 0L, 7L, 0L))
  expect_identical(res$cumulative, c(12L, 12L, 7L, 7L))
})

test_that("aggregate to integer number of days works as expected", {
  res <- aggregate(line_list, interval = 14)$outbreak_ts
  expect_named(res, c("sim", "period", "period_cases", "cumulative"))
  expect_identical(res$sim, rep(1:2, each = 3))
  expect_identical(res$period, rep(0:2, times = 2))
  expect_identical(res$period_cases, c(3L, 7L, 2L, 7L, 0L, 0L))
  expect_identical(res$cumulative, c(3L, 10L, 12L, 7L, 7L, 7L))
})

test_that("aggregate accepts interval aliases", {
  expect_identical(aggregate(line_list, "day"), aggregate(line_list, "daily"))
  expect_identical(aggregate(line_list, "days"), aggregate(line_list, "daily"))
  expect_identical(aggregate(line_list, "week"), aggregate(line_list, "weekly"))
  expect_identical(
    aggregate(line_list, "weeks"), aggregate(line_list, "weekly")
  )
  expect_identical(
    aggregate(line_list, "month"), aggregate(line_list, "monthly")
  )
  expect_identical(
    aggregate(line_list, "months"), aggregate(line_list, "monthly")
  )
})

test_that("aggregate includes replicates missing from the case line list", {
  set.seed(5)
  res <- scenario_sim(
    n = 5,
    initial_cases = 1,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions,
    sim = sim_opts(cap_max_days = 3, aggregation = "none")
  )
  # only replicates 3 and 4 have a case with symptom onset before
  # cap_max_days
  expect_identical(res$outbreak_ts$sim, 3:4)
  agg <- aggregate(res, interval = "weekly")$outbreak_ts
  expect_identical(agg$sim, 1:5)
  expect_identical(agg$weekly_cases, c(0L, 0L, 1L, 1L, 0L))
})

# the final interval can be undercounted when re-aggregating already
# aggregated data as the final interval of the input can end before the final
# interval of the output, so the final interval is dropped for comparison
drop_final_interval <- function(x) {
  time_col <- setdiff(names(x$outbreak_ts), "sim")[1]
  x$outbreak_ts[get(time_col) < max(get(time_col))]
}

test_that("aggregate can re-aggregate to a coarser interval", {
  daily <- aggregate(line_list, interval = "daily")
  weekly <- aggregate(line_list, interval = "weekly")
  # daily -> weekly, monthly and integer
  for (interval in list("weekly", "monthly", 14)) {
    expect_identical(
      drop_final_interval(aggregate(daily, interval)),
      drop_final_interval(aggregate(line_list, interval))
    )
  }
  # weekly -> integer multiple of 7
  for (interval in list(14, 28)) {
    expect_identical(
      drop_final_interval(aggregate(weekly, interval)),
      drop_final_interval(aggregate(line_list, interval))
    )
  }
})

test_that("aggregating the line list excludes cases after cap_max_days", {
  set.seed(118)
  weekly <- scenario_sim(
    n = 2,
    initial_cases = 2,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions,
    sim = sim_opts(cap_max_days = 30)
  )
  # replicate 1 has 3 cases in week 4 with symptom onset after cap_max_days,
  # which are in the weekly simulation output but not in the case line list
  expect_identical(weekly$outbreak_ts[sim == 1 & week == 4, weekly_cases], 5L)
  res <- aggregate(line_list, "weekly")$outbreak_ts
  expect_identical(res[sim == 1 & week == 4, weekly_cases], 2L)
})

test_that("aggregate to the same interval returns the input unchanged", {
  expect_identical(aggregate(line_list, "none"), line_list)
  weekly <- aggregate(line_list, "weekly")
  expect_identical(aggregate(weekly, "weekly"), weekly)
  expect_identical(aggregate(weekly, "week"), weekly)
})

test_that("aggregate errors when aggregating to a finer interval", {
  weekly <- aggregate(line_list, interval = "weekly")
  expect_error(
    aggregate(weekly, interval = "daily"),
    regexp = "Cannot aggregate from `weekly` to `daily`"
  )
  expect_error(
    aggregate(weekly, interval = "none"),
    regexp = "Cannot disaggregate outbreak data"
  )
})

test_that("aggregate errors when intervals are not nested", {
  weekly <- aggregate(line_list, interval = "weekly")
  expect_error(
    aggregate(weekly, interval = "monthly"),
    regexp = "Cannot aggregate from `weekly` to `monthly`"
  )
  expect_error(
    aggregate(weekly, interval = 10),
    regexp = "Cannot aggregate from `weekly` to `10`"
  )
})

test_that("aggregate errors with invalid input", {
  expect_error(
    aggregate(line_list, interval = "fortnightly"),
    regexp = "Must be element"
  )
  expect_error(aggregate(line_list, interval = 0), regexp = "not >= 1")
  expect_error(aggregate(line_list), regexp = "interval")
  expect_warning(
    aggregate(line_list, "weekly", extra = 1),
    regexp = "extra argument"
  )
})

test_that("aggregate errors when attributes are missing", {
  for (attr_name in c("aggregation", "cap_max_days", "extinct")) {
    x <- line_list
    # copy to not modify line_list by reference
    x$outbreak_ts <- data.table::copy(line_list$outbreak_ts)
    setattr(x$outbreak_ts, name = attr_name, value = NULL)
    expect_error(
      aggregate(x, interval = "weekly"),
      regexp = "missing the `aggregation`, `cap_max_days` or `extinct`"
    )
  }
})

test_that("outbreak_model returns a case line list with aggregation = 'none'", {
  set.seed(1)
  res <- outbreak_model(
    initial_cases = 5,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions,
    sim = sim_opts(cap_max_days = 70, aggregation = "none")
  )
  expect_s3_class(res, "ringbp", exact = TRUE)
  expect_named(res$outbreak_ts, c("onset", "cumulative"))
  expect_type(res$outbreak_ts$onset, "double")
  expect_false(is.unsorted(res$outbreak_ts$onset))
  expect_true(all(res$outbreak_ts$onset < 70))
  expect_identical(res$outbreak_ts$cumulative, seq_len(nrow(res$outbreak_ts)))
  expect_identical(attr(res$outbreak_ts, "aggregation"), "none")
  expect_identical(attr(res$outbreak_ts, "cap_max_days"), 70)
  expect_type(attr(res$outbreak_ts, "extinct"), "logical")
})

test_that("outbreak_model aggregation matches aggregate of case line list", {
  for (interval in list("daily", "weekly", "monthly", 14)) {
    set.seed(1)
    none <- outbreak_model(
      initial_cases = 5,
      offspring = offspring,
      delays = delays,
      event_probs = event_probs,
      interventions = interventions,
      sim = sim_opts(cap_max_days = 70, aggregation = "none")
    )
    set.seed(1)
    agg <- outbreak_model(
      initial_cases = 5,
      offspring = offspring,
      delays = delays,
      event_probs = event_probs,
      interventions = interventions,
      sim = sim_opts(cap_max_days = 70, aggregation = interval)
    )
    expect_s3_class(agg, "ringbp", exact = TRUE)
    # the case line list is truncated at cap_max_days so the final interval
    # can differ
    res <- aggregate(none, interval = interval)
    expect_identical(drop_final_interval(res), drop_final_interval(agg))
    expect_identical(attributes(res$outbreak_ts), attributes(agg$outbreak_ts))
    expect_identical(res$outbreak_stats, agg$outbreak_stats)
  }
})

test_that("scenario_sim output can be aggregated after simulation", {
  set.seed(1)
  none <- scenario_sim(
    n = 3,
    initial_cases = 5,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions,
    sim = sim_opts(cap_max_days = 70, aggregation = "none")
  )
  set.seed(1)
  weekly <- scenario_sim(
    n = 3,
    initial_cases = 5,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions,
    sim = sim_opts(cap_max_days = 70)
  )
  expect_s3_class(none, "ringbp", exact = TRUE)
  expect_named(none$outbreak_ts, c("sim", "onset", "cumulative"))
  expect_identical(unique(none$outbreak_ts$sim), 1:3)
  expect_identical(attr(none$outbreak_ts, "cap_cases"), 5000)
  expect_length(attr(none$outbreak_ts, "extinct"), 3)
  expect_identical(
    drop_final_interval(aggregate(none, interval = "weekly")),
    drop_final_interval(weekly)
  )
  # daily -> weekly is equivalent to simulating weekly (except final week)
  expect_identical(
    drop_final_interval(aggregate(aggregate(none, "daily"), "weekly")),
    drop_final_interval(weekly)
  )
})
