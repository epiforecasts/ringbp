#' Sweep across parameters
#'
#' @description Explore scenarios using gridding with sampling for parameters not in the grid. Parameters that
#' are included in the grid are currently hard coded. Use the `future` package to control parallisation
#' outside of the function.
#'
#' @param scenarios a `data.frame`: containing all gridded scenarios - see the
#'   examples for the required structure. Defaults to `NULL`.
#' @param samples a positive `integer` scalar: the number of samples to take.
#'   Defaults to `1`.
#' @param sim_fn a `function`: defaults to `NULL`. The vectorised model
#'   simulation function - see the examples for usage.
#'
#' @return A nested `data.table` containing the parameters for each scenario and a nested list of output
#' from [scenario_sim()].
#' @autoglobal
#' @export
#' @importFrom future.apply future_lapply
#' @importFrom purrr safely
#' @importFrom data.table .SD
#' @examplesIf requireNamespace("data.table", quietly = TRUE)
#' library(data.table)
#' # Put parameters that are grouped by disease into this data.table
#' scenarios <- data.table(
#'   expand.grid(
#'     delay_group = list(data.table(
#'       delay = c("SARS", "Wuhan"),
#'       onset_to_isolation = c(
#'         \(x) rweibull(n = x, shape = 1.651524, scale = 4.287786),
#'         \(x) rweibull(n = x, shape = 2.305172, scale = 9.483875)
#'       )
#'     )),
#'     k_group = list(data.table(
#'       theta = c("<1%", "15%"),
#'       k = c(1, 0.88)
#'     )),
#'     r0_community = c(1.1, 1.5),
#'     prop_asymptomatic = c(0, 0.1),
#'     prop_ascertain = seq(0, 1, 0.25),
#'     initial_cases = c(5, 10),
#'     quarantine = FALSE
#'   )
#' )
#'
#' list_cols <- grep("_group", colnames(scenarios), value = TRUE)
#' non_list_cols <- setdiff(colnames(scenarios), list_cols)
#'
#' expanded_groups <- scenarios[, rbindlist(delay_group), by = c(non_list_cols)]
#' expanded_k <- scenarios[, rbindlist(k_group), by = c(non_list_cols)]
#'
#' scenarios <- merge(
#'   expanded_groups, expanded_k, by = non_list_cols, allow.cartesian = TRUE
#' )
#' scenarios[, scenario :=  1:.N]
#'
#' incub <- \(x) rweibull(n = x, shape = 1.65, scale = 4.28)
#' scenarios[, incubation_period := rep(list(incub), .N)]
#'
#' ## Parameterise fixed paramters
#' sim_with_params <- purrr::partial(
#'   ringbp::scenario_sim,
#'   cap_max_days = 365,
#'   cap_cases = 5000,
#'   r0_isolated = 0,
#'   disp_isolated = 1,
#'   disp_asymptomatic = 0.16,
#'   disp_community = 0.16
#' )
#'
#' ## parameter_sweep uses the future_lapply() function
#' ## Default is to run sequntially on a single core
#' # future::plan("sequential")
#' ## Set up multicore if using see ?future::plan for details
#' ## Use the workers argument to control the number of cores used.
#' # future::plan("multiprocess")
#'
#' ## Run parameter sweep
#' sweep_results <- parameter_sweep(
#'   scenarios,
#'   sim_fn = sim_with_params,
#'   samples = 1
#' )
#'
#' sweep_results
parameter_sweep <- function(scenarios = NULL, samples = 1,
                            sim_fn = NULL) {

  safe_sim_fn <- purrr::safely(sim_fn)

  ## create list column
  scenario_sims <- scenarios[, list(data = list(.SD)), by = scenario]
  ## Randomise the order of scenarios - helps share the load across cores
  scenario_sims <- scenario_sims[sample(.N), ]
  ## Run simulations
  scenario_sims[, sims := future_lapply(
    data,
    \(x) safe_sim_fn(
      n = samples,
      initial_cases = x$initial_cases,
      r0_community = x$r0_community,
      r0_asymptomatic = ifelse(
        "asymptomatic_R0" %in% names(scenarios), x$asymptomatic_R0, x$r0_community),
      k = x$k,
      onset_to_isolation = x$onset_to_isolation[[1]],
      incubation_period = x$incubation_period[[1]],
      prop_ascertain = x$prop_ascertain,
      quarantine = x$quarantine,
      prop_asymptomatic = x$prop_asymptomatic
    )[[1]],
    future.scheduling = 20,
    future.seed = TRUE
  )]

  return(scenario_sims[])
}
