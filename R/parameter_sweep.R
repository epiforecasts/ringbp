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
#'     index_R0 = c(1.1, 1.5),
#'     prop.asym = c(0, 0.1),
#'     control_effectiveness = seq(0, 1, 0.25),
#'     num.initial.cases = c(5, 10),
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
#'   r0isolated = 0,
#'   disp.iso= 1,
#'   disp.subclin = 0.16,
#'   disp.com = 0.16
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
      n.sim = samples,
      num.initial.cases = x$num.initial.cases,
      r0community = x$index_R0,
      r0subclin = ifelse(
        "subclin_R0" %in% names(scenarios), x$subclin_R0, x$index_R0),
      k = x$k,
      onset_to_isolation = x$onset_to_isolation[[1]],
      incubation_period = x$incubation_period[[1]],
      prop.ascertain = x$control_effectiveness,
      quarantine = x$quarantine,
      prop.asym = x$prop.asym
    )[[1]],
    future.scheduling = 20,
    future.seed = TRUE
  )]

  return(scenario_sims[])
}
