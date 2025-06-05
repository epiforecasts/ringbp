#' Samples the generation time for given incubation period samples
#'
#' @param incubation_period_samples a positive `numeric` vector: samples from
#'   the incubation period distribution
#' @param alpha a `numeric` scalar: skew parameter for sampling the generation
#'   time from the incubation period
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
  checkmate::assert_number(args$alpha, finite = TRUE)

  out <- sn::rsn(n = length(incubation_period_samples),
                 xi = incubation_period_samples,
                 omega = 2,
                 alpha = alpha)

  return(pmax(1, out))
}

#' Calculate skew normal alpha parameter from proportion of presymptomatic
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
  optimise(f = objective, interval = c(-1e5, 1e5))$minimum
}

#' Calculate proportion of runs that have controlled outbreak
#'
#' @inherit detect_extinct details
#'
#' @author Joel Hellewell
#' @return a single `numeric` with the probability of extinction
#' @export
#' @inheritParams detect_extinct
#'
#' @examples
#' res <- scenario_sim(
#'   n = 10,
#'   initial_cases = 1,
#'   prop_asymptomatic = 0,
#'   prop_ascertain = 0.2,
#'   cap_cases = 4500,
#'   cap_max_days = 350,
#'   r0_isolated = 0.5,
#'   r0_community = 2.5,
#'   disp_community = 0.16,
#'   disp_isolated = 1,
#'   onset_to_isolation = \(x) rweibull(n = x, shape = 1.65, scale = 4.28),
#'   incubation_period = \(x) rweibull(n = x, shape = 2.322737, scale = 6.492272),
#'   prop_presymptomatic = 0.5,
#'   quarantine = FALSE
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
#' @inheritParams outbreak_model
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
#'   initial_cases = 1,
#'   prop_asymptomatic = 0,
#'   prop_ascertain = 0.2,
#'   cap_cases = 4500,
#'   cap_max_days = 350,
#'   r0_isolated = 0.5,
#'   r0_community = 2.5,
#'   disp_community = 0.16,
#'   disp_isolated = 1,
#'   onset_to_isolation = \(x) rweibull(n = x, shape = 1.65, scale = 4.28),
#'   incubation_period = \(x) rweibull(n = x, shape = 2.322737, scale = 6.492272),
#'   prop_presymptomatic = 0.5,
#'   quarantine = FALSE
#' )
#' detect_extinct(outbreak_df_week = res, cap_cases = 4500)
detect_extinct <- function(outbreak_df_week, cap_cases, week_range = 12:16) {

  checkmate::assert_data_frame(outbreak_df_week)
  checkmate::assert_number(cap_cases, lower = 0)
  checkmate::assert_integerish(week_range)

  outbreak_df_week <- as.data.table(outbreak_df_week)
  outbreak_df_week <- outbreak_df_week[week %in% week_range]
  outbreak_df_week[, list(
    extinct = fifelse(all(weekly_cases == 0 & cumulative < cap_cases), 1, 0)
  ), by = sim]
}
