offspring <- offspring_opts(
  community = \(n) rep(2, n),
  isolated = \(n) rep(0, n)
)
delays <- delay_opts(
  incubation_period = \(n) rep(2, n),
  onset_to_isolation = \(n) rep(5, n)
)
event_probs <- event_prob_opts(
  asymptomatic = 0.1,
  presymptomatic_transmission = 0.2,
  symptomatic_traced = 0.2
)
interventions <- intervention_opts()

# cap_max_days = 210 as the smallest cap that divides into daily, weeks,
# months, and 14-day period
set.seed(1)
line_list <- scenario_sim(
  n = 2,
  initial_cases = 1,
  offspring = offspring,
  delays = delays,
  event_probs = event_probs,
  interventions = interventions,
  sim = sim_opts(cap_max_days = 210, aggregation = "none")
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
  expect_identical(attr(res, "extinct"), c(FALSE, FALSE))
  expect_identical(attr(res, "cap_cases"), 5000)
  expect_identical(attr(res, "cap_max_days"), 210)
  res <- aggregate(line_list, interval = 14)$outbreak_ts
  expect_identical(attr(res, "aggregation"), 14L)
})

test_that("aggregate to daily works as expected", {
  res <- aggregate(line_list, interval = "daily")$outbreak_ts
  expect_named(res, c("sim", "day", "daily_cases", "cumulative"))
  expect_identical(res$sim, rep(1:2, each = 210))
  expect_identical(res$day, rep(0:209, times = 2))
  expect_type(res$daily_cases, "integer")
  expect_identical(
    res[sim == 1, cumulative],
    cumsum(res[sim == 1, daily_cases])
  )
  expect_identical(
    res[sim == 2, cumulative],
    cumsum(res[sim == 2, daily_cases])
  )
})

test_that("aggregate to weekly works as expected", {
  res <- aggregate(line_list, interval = "weekly")$outbreak_ts
  expect_named(res, c("sim", "week", "weekly_cases", "cumulative"))
  expect_identical(res$sim, rep(1:2, each = 30))
  expect_identical(res$week, rep(0:29, times = 2))
  expect_type(res$weekly_cases, "integer")
  expect_identical(
    res[sim == 1, cumulative],
    cumsum(res[sim == 1, weekly_cases])
  )
  expect_identical(
    res[sim == 2, cumulative],
    cumsum(res[sim == 2, weekly_cases])
  )
})

test_that("aggregate to monthly works as expected", {
  res <- aggregate(line_list, interval = "monthly")$outbreak_ts
  expect_named(res, c("sim", "month", "monthly_cases", "cumulative"))
  expect_identical(res$sim, rep(1:2, each = 7))
  expect_identical(res$month, rep(0:6, times = 2))
  expect_type(res$monthly_cases, "integer")
  expect_identical(
    res[sim == 1, cumulative],
    cumsum(res[sim == 1, monthly_cases])
  )
  expect_identical(
    res[sim == 2, cumulative],
    cumsum(res[sim == 2, monthly_cases])
  )
})

test_that("aggregate to integer number of days works as expected", {
  res <- aggregate(line_list, interval = 14)$outbreak_ts
  expect_named(res, c("sim", "period", "period_cases", "cumulative"))
  expect_identical(res$sim, rep(1:2, each = 15))
  expect_identical(res$period, rep(0:14, times = 2))
  expect_type(res$period_cases, "integer")
  expect_identical(
    res[sim == 1, cumulative],
    cumsum(res[sim == 1, period_cases])
  )
  expect_identical(
    res[sim == 2, cumulative],
    cumsum(res[sim == 2, period_cases])
  )
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
    sim = sim_opts(cap_max_days = 2, aggregation = "none")
  )
  # the initial case in every replicate has symptom onset on day 2 (fixed
  # incubation period), so no replicate has a case with symptom onset before
  # cap_max_days (day 2) and the case line list is empty
  expect_identical(nrow(res$outbreak_ts), 0L)
  agg <- aggregate(res, interval = "daily")$outbreak_ts
  expect_identical(agg$sim, rep(1:5, each = 2))
  expect_identical(agg$day, rep(0:1, times = 5))
  expect_identical(agg$daily_cases, integer(10))
  expect_identical(agg$cumulative, integer(10))
})

test_that("aggregate can re-aggregate to a coarser interval", {
  daily <- aggregate(line_list, interval = "daily")
  weekly <- aggregate(line_list, interval = "weekly")
  # daily -> weekly, monthly and integer
  for (interval in list("weekly", "monthly", 14)) {
    expect_identical(
      aggregate(daily, interval),
      aggregate(line_list, interval)
    )
  }
  # weekly -> integer multiple of 7
  for (interval in list(14, 42, 70)) {
    expect_identical(
      aggregate(weekly, interval),
      aggregate(line_list, interval)
    )
  }
})

test_that("aggregate errors if simulation is not a whole number of intervals", {
  # 210 days is not a whole number of 28 day intervals
  expect_error(
    aggregate(line_list, interval = 28),
    regexp = "simulation \\(210 days\\) cannot be divided exactly into `28`"
  )
  weekly <- aggregate(line_list, interval = "weekly")
  expect_error(
    aggregate(weekly, interval = 28),
    regexp = "simulation \\(210 days\\) cannot be divided exactly into `28`"
  )
  # simulation shorter than the interval
  short <- scenario_sim(
    n = 1,
    initial_cases = 1,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions,
    sim = sim_opts(cap_max_days = 20, aggregation = "daily")
  )
  expect_error(
    aggregate(short, interval = "monthly"),
    regexp = "simulation \\(20 days\\) cannot be divided exactly"
  )
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
  # 210 days is a whole number of months but months are not made up of weeks
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
  set.seed(1)
  none <- outbreak_model(
    initial_cases = 5,
    offspring = offspring,
    delays = delays,
    event_probs = event_probs,
    interventions = interventions,
    sim = sim_opts(cap_max_days = 210, aggregation = "none")
  )
  for (interval in list("daily", "weekly", "monthly", 14)) {
    set.seed(1)
    agg <- outbreak_model(
      initial_cases = 5,
      offspring = offspring,
      delays = delays,
      event_probs = event_probs,
      interventions = interventions,
      sim = sim_opts(cap_max_days = 210, aggregation = interval)
    )
    expect_s3_class(agg, "ringbp", exact = TRUE)
    expect_identical(aggregate(none, interval = interval), agg)
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
  # a case line list aggregated to weekly is the same as simulating weekly,
  # including the final week
  expect_identical(aggregate(none, interval = "weekly"), weekly)
  expect_identical(aggregate(aggregate(none, "daily"), "weekly"), weekly)
})
