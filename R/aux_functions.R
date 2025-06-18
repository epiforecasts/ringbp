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

  pmax(1, out)
}

#' Estimate skew normal alpha parameter from proportion of presymptomatic
#' transmission
#'
#' @details Since there isn't any analytical expression for linking the two,
#' the value of alpha that corresponds to the given proportion presymptomatic
#' is obtained via numeric optimisation.
#'
#' @param presymptomatic_transmission a `numeric` scalar probability
#'   (between 0 and 1 inclusive): proportion of transmission that occurs
#'   before symptom onset.
#'
#' @return A `numeric` scalar: The `$minimum` output from [optimise()] to find
#'   the best `alpha` parameter to get the desired proportion of presymptomatic
#'   transmission.
#' @keywords internal
presymptomatic_transmission_to_alpha <- function(presymptomatic_transmission) {
  objective <- function(alpha) {
    # fix x, xi and omega for optimisation
    p_current <- sn::psn(x = 0, xi = 0, omega = 2, alpha = alpha)
    (p_current - presymptomatic_transmission)^2
  }
  # alpha domain is (-Inf, Inf), approximate with large numbers
  res <- stats::optimise(f = objective, interval = c(-1e5, 1e5))
  if (res$objective > 1e-5) {
    stop(
      "Estimating the `alpha` parameter from `presymptomatic_transmission` ",
      "did not converge."
    )
  }
  res$minimum
}

#' Calculate proportion of runs that have controlled outbreak
#'
#' @inherit detect_extinct details
#'
#' @inheritParams detect_extinct
#'
#' @return a single `numeric` with the probability of extinction
#' @export
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
#'     symptomatic_ascertained = 0.2
#'   ),
#'   interventions = intervention_opts(quarantine = FALSE),
#'   sim = sim_opts(cap_max_days = 350, cap_cases = 4500)
#' )
#' extinct_prob(res)
extinct_prob <- function(outbreak_df_week, week_range = 12:16) {

  checkmate::assert_data_frame(outbreak_df_week)
  checkmate::assert_numeric(week_range)

  n <- max(outbreak_df_week$sim)

  extinct_runs <- detect_extinct(outbreak_df_week, week_range)
  sum(extinct_runs$extinct) / n
}


#' Calculate whether outbreaks went extinct or not
#'
#' @details
#' The data passed to `outbreak_df_week` has to be produced by [scenario_sim()].
#' It cannot be produced by [outbreak_model()] as it requires the `sim` column,
#' which is only appended in [scenario_sim()].
#'
#' ***Warning***: the output from [scenario_sim()] contains an `cap_cases`
#' attribute which is used by [extinct_prob()] and [detect_extinct()],
#' therefore if you modify the output of [scenario_sim()] before passing
#' to [extinct_prob()] be careful not to drop the attribute (e.g.
#' from subsetting the `data.table`).
#'
#' @param outbreak_df_week a `data.table`: weekly cases produced by the
#'   outbreak model
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
#'     symptomatic_ascertained = 0.2
#'   ),
#'   interventions = intervention_opts(quarantine = FALSE),
#'   sim = sim_opts(cap_max_days = 350, cap_cases = 4500)
#' )
#' detect_extinct(outbreak_df_week = res)
detect_extinct <- function(outbreak_df_week, week_range = 12:16) {

  checkmate::assert_data_frame(outbreak_df_week)
  checkmate::assert_integerish(week_range)

  cap_cases <- attr(outbreak_df_week, which = "cap_cases")

  outbreak_df_week <- as.data.table(outbreak_df_week)
  outbreak_df_week <- outbreak_df_week[week %in% week_range]
  outbreak_df_week[, list(
    extinct = fifelse(all(weekly_cases == 0 & cumulative < cap_cases), 1, 0)
  ), by = sim][]
}
