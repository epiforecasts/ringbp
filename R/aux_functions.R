#' Samples the generation time for given incubation period samples
#'
#' This is done assuming the generation time distribution of each individual is
#' given by a skew-normal distribution with a location parameter equal to their
#' incubation period.
#'
#' @param incubation_period_samples a positive `numeric` vector: samples from
#'   the incubation period distribution
#' @param alpha a `numeric` scalar: skew parameter of the skew-normal
#'   distribution
#'
#' @return a `numeric` vector of equal length to the vector input to
#'   `incubation_period_samples`
#' @export
#' @importFrom sn rsn
#'
#' @examples
#' incubation_to_generation_time(
#'   incubation_period_samples = c(1, 2, 3, 4, 1),
#'   alpha = 2
#' )
incubation_to_generation_time <- function(incubation_period_samples, alpha) {

  checkmate::assert_numeric(incubation_period_samples, lower = 0, finite = TRUE)
  checkmate::assert_number(alpha, finite = TRUE)

  out <- sn::rsn(n = length(incubation_period_samples),
                 xi = incubation_period_samples,
                 omega = 2,
                 alpha = alpha)

  return(pmax(1, out))
}

#' Estimate skew normal alpha parameter from proportion of presymptomatic
#' transmission
#'
#' @param prop_presymptomatic a `numeric` scalar probability (between 0 and 1
#'   inclusive): proportion of transmission that occurs before symptom onset.
#'
#' @return A `numeric` scalar: The `$minimum` output from [optimise()] to find
#'   the best `alpha` parameter to get the desired proportion of presymptomatic
#'   transmission.
#' @keywords internal
prop_presymptomatic_to_alpha <- function(prop_presymptomatic) {
  objective <- function(alpha) {
    # fix x, xi and omega for optimisation
    p_current <- sn::psn(x = 0, xi = 0, omega = 2, alpha = alpha)
    return((p_current - prop_presymptomatic)^2)
  }
  # alpha domain is (-Inf, Inf), approximate with large numbers
  res <- stats::optimise(f = objective, interval = c(-1e5, 1e5))
  if (res$objective > 1e-5) {
    stop(
      "Estimating the `alpha` parameter from `prop_presymptomatic` ",
      "did not converge."
    )
  }
  return(res$minimum)
}

#' Calculate proportion of runs that have controlled outbreak
#'
#' @inherit detect_extinct details
#'
#' @inheritParams detect_extinct
#'
#' @author Joel Hellewell
#' @return a single `numeric` with the probability of extinction
#' @export
#'
#' @examples
#' res <- scenario_sim(
#'   n = 10,
#'   parameters = parameters(
#'     initial_cases = 1,
#'     r0_community = 2.5,
#'     r0_isolated = 0.5,
#'     disp_community = 0.16,
#'     disp_isolated = 1,
#'     incubation_period = \(x) rweibull(n = x, shape = 2.32, scale = 6.49),
#'     prop_presymptomatic = 0.5,
#'     onset_to_isolation = \(x) rweibull(n = x, shape = 1.65, scale = 4.28),
#'     prop_ascertain = 0.2,
#'     prop_asymptomatic = 0,
#'     quarantine = FALSE
#'   ),
#'   control = control(cap_max_days = 350, cap_cases = 4500)
#' )
#' extinct_prob(res, cap_cases = 4500)
extinct_prob <- function(outbreak_df_week, cap_cases, week_range = 12:16) {

  checkmate::assert_data_frame(outbreak_df_week)
  checkmate::assert_number(cap_cases, lower = 0)
  checkmate::assert_numeric(week_range)

  n <- max(outbreak_df_week$sim)

  extinct_runs <- detect_extinct(outbreak_df_week, cap_cases, week_range)
  out <-  sum(extinct_runs$extinct) / n

  return(out)
}


#' Calculate whether outbreaks went extinct or not
#'
#' @details
#' The `cap_cases` argument should be equal to the value supplied to
#' [outbreak_model()] (possibly passed from [scenario_sim()] or
#' [parameter_sweep()]).
#'
#' @author Joel Hellewell
#' @param outbreak_df_week a `data.table`: weekly cases produced by the
#'   outbreak model
#' @inheritParams control
#' @param week_range a positive `integer` vector: giving the (zero indexed)
#'   week range to test for whether an extinction occurred. Default is `12:16`.
#' @importFrom data.table as.data.table fifelse
#'
#' @return A `data.table`, with two columns `sim` and `extinct`, for a binary
#' classification of whether the outbreak went extinct in each simulation
#' replicate. `1` is an outbreak that went extinct, `0` if not.
#' @autoglobal
#' @export
#'
#' @examples
#' res <- scenario_sim(
#'   n = 10,
#'   parameters = parameters(
#'     initial_cases = 1,
#'     r0_community = 2.5,
#'     r0_isolated = 0.5,
#'     disp_community = 0.16,
#'     disp_isolated = 1,
#'     incubation_period = \(x) rweibull(n = x, shape = 2.32, scale = 6.49),
#'     prop_presymptomatic = 0.5,
#'     onset_to_isolation = \(x) rweibull(n = x, shape = 1.65, scale = 4.28),
#'     prop_ascertain = 0.2,
#'     prop_asymptomatic = 0,
#'     quarantine = FALSE
#'   ),
#'   control = control(cap_max_days = 350, cap_cases = 4500)
#' )
#' detect_extinct(outbreak_df_week = res, cap_cases = 4500)
detect_extinct <- function(outbreak_df_week, cap_cases, week_range = 12:16) {

  checkmate::assert_data_frame(outbreak_df_week)
  checkmate::assert_number(cap_cases, lower = 0)
  checkmate::assert_integerish(week_range)

  outbreak_df_week <- as.data.table(outbreak_df_week)
  outbreak_df_week <- outbreak_df_week[week %in% week_range]
  out <- outbreak_df_week[, list(
    extinct = fifelse(all(weekly_cases == 0 & cumulative < cap_cases), 1, 0)
  ), by = sim]
  return(out)
}

#' Create a list of parameters to run the \pkg{ringbp} model
#'
#' @details
#' If `r0_asymptomatic` or `disp_asymptomatic` are not provided they will be
#' specified as the values of `r0_community` and `disp_community`, respectively.
#'
#' @inheritParams outbreak_setup
#' @inheritParams outbreak_step
#' @inheritParams prop_presymptomatic_to_alpha
#'
#' @param initial_cases a nonnegative `integer` scalar: number of initial
#'   or starting cases which are all assumed to be missed.
#' @param r0_community a positive `numeric` scalar: reproduction number for
#'   non-isolated cases (must be >0)
#' @param r0_isolated a positive `numeric` scalar: reproduction number for
#'   isolated cases (must be >0)
#' @param r0_asymptomatic a positive `numeric` scalar: reproduction number for
#'   sub-clinical non-isolated cases (must be >0)
#' @param disp_community a positive `numeric` scalar: dispersion parameter for
#'   non-isolated cases (must be >0)
#' @param disp_isolated a positive `numeric` scalar: dispersion parameter for
#'   isolated cases (must be >0)
#' @param disp_asymptomatic a positive `numeric` scalar: dispersion parameter
#'   for sub-clincial non-isolated cases (must be >0)
#' @param incubation_period a `function`: a random number generating
#'   `function` that samples from incubation period distribution, the
#'   `function` accepts a single `integer` argument specifying the number of
#'   times to sample the incubation period (i.e. length of the `function`
#'   output).
#' @inheritParams prop_presymptomatic_to_alpha
#' @param onset_to_isolation a `function`: a random number generating
#'   `function` that accepts a single `integer` argument specifying the
#'   length of the `function` output.
#' @param prop_ascertain a `numeric` scalar probability (between 0 and 1
#'   inclusive): proportion of infectious contacts ascertained by contact
#'   tracing
#' @param prop_asymptomatic a `numeric` scalar probability (between 0 and 1
#'   inclusive): proportion of cases that are completely asymptomatic
#'   (subclinical)
#' @param quarantine a `logical` scalar: whether quarantine is in effect, if
#'   `TRUE` then traced contacts are isolated before symptom onset; defaults to
#'   `FALSE`
#'
#' @return A `list` with class `<ringbp_parameters>`.
#' @export
#'
#' @examples
#' parameters(
#'   initial_cases = 10,
#'   r0_community = 2.5,
#'   r0_isolated = 0.5,
#'   r0_asymptomatic = 2.5,
#'   disp_community = 0.16,
#'   disp_isolated = 1,
#'   disp_asymptomatic = 0.16,
#'   incubation_period = \(x) rweibull(n = x, shape = 2.32, scale = 6.49),
#'   prop_presymptomatic = 0.5,
#'   onset_to_isolation = \(x) rweibull(n = x, shape = 1.65, scale = 4.28),
#'   prop_ascertain = 0.2,
#'   prop_asymptomatic = 0.1,
#'   quarantine = FALSE
#' )
parameters <- function(initial_cases,
                       r0_community, r0_isolated, r0_asymptomatic,
                       disp_community, disp_isolated, disp_asymptomatic,
                       incubation_period, prop_presymptomatic,
                       onset_to_isolation,
                       prop_ascertain, prop_asymptomatic,
                       quarantine = FALSE) {

  # Set infectiousness of subclinical cases to be equal to clinical cases
  # unless specified otherwise
  if (missing(r0_asymptomatic)) {
    r0_asymptomatic <- r0_community
  }

  if (missing(disp_asymptomatic)) {
    disp_asymptomatic <- disp_community
  }

  check_outbreak_input(func = "parameters")

  # calculate alpha parameter from prop_presymptomatic
  alpha <- prop_presymptomatic_to_alpha(
    prop_presymptomatic = prop_presymptomatic
  )

  parameters <- list(
    initial_cases = initial_cases,
    r0_community = r0_community,
    r0_isolated = r0_isolated,
    r0_asymptomatic = r0_asymptomatic,
    disp_community = disp_community,
    disp_isolated = disp_isolated,
    disp_asymptomatic = disp_asymptomatic,
    incubation_period = incubation_period,
    alpha = alpha,
    onset_to_isolation = onset_to_isolation,
    prop_ascertain = prop_ascertain,
    prop_asymptomatic = prop_asymptomatic,
    quarantine = quarantine
  )

  class(parameters) <- "ringbp_parameters"

  return(parameters)
}

#' Create a list of control options for the \pkg{ringbp} model
#'
#' @param cap_max_days a positive `integer` scalar: stop the simulation when
#'   this many days is reached.
#' @param cap_cases a positive `integer` scalar: number of cumulative cases at
#'   which the branching process (simulation) was terminated
#'
#' @return A `list` with class `<ringbp_control>`.
#' @export
#'
#' @examples
#' # default control options
#' control()
#'
#' # specifying custom control options
#' control(
#'   cap_max_days = 140,
#'   cap_cases = 1000
#' )
control <- function(cap_max_days = 350, cap_cases  = 5000) {

  check_outbreak_input(func = "control")

  control <- list(
    cap_max_days = cap_max_days,
    cap_cases = cap_cases
  )

  class(control) <- "ringbp_control"

  return(control)
}
