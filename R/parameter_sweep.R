#' Sweep across parameters
#'
#' @description Explore scenarios using gridding with sampling for parameters
#' not in the grid. Parameters that are included in the grid are currently
#' hard coded. Use the `future` package to control parallisation outside of
#' the function.
#'
#' @param scenarios A dataframe containing all gridded  scenarios - see the
#' examples for the required structure. Defaults to NULL.
#' @param samples Numeric, defaults to 1. The number of samples to take.
#' @param sim_fn Function, defaults to NULL. The vectorised model simulation
#' function - see the examples for usage.
#'
#' @return A nested tibble containing the parameters for each scenario and a
#' nested list of output from `wuhan_sim`.
#' @export
#' @importFrom future.apply future_lapply
#' @importFrom purrr safely
#' @importFrom data.table .SD
#' @examples
#'
#'
#'\dontrun{
#' library(data.table)
#'
## Put parameters that are grouped by disease into this data.frame
#' scenarios <- data.table(expand.grid(
#' delay_group = list(data.table(
#'  delay = c("SARS","Wuhan"),
#'  delay_shape = c(1.651524,2.305172),
#'  delay_scale = c(4.287786,9.483875)
#' )),
#' k_group = list(data.table(
#'  theta = c("<1%","15%","30%"),
#'  k = c(1,0.88,0.47)
#' )),
#' index_R0 = c(1.5,2.5,3.5),
#' prop_asym = c(0,  0.1),
#' control_effectiveness = seq(0,1,0.2),
#' num_initial_cases = c(5,20,40))
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
#' ## Parameterise fixed paramters
#' sim_with_params <- purrr::partial(ringbp::scenario_sim,
#'                                  cap_max_days = 365,
#'                                  cap_cases = 5000,
#'                                  r0isolated = 0,
#'                                  disp_iso= 1,
#'                                  disp_subclin = 0.16,
#'                                  disp_com = 0.16,
#'                                  quarantine = FALSE)
#'
#'
#' ## Default is to run sequntially on a single core
#' future::plan("sequential")
#' ## Set up multicore if using see ?future::plan for details
#' ## Use the workers argument to control the number of cores used.
#' future::plan("multiprocess")
#'
#'
#' ## Run paramter sweep
#' sweep_results <- ringbp::parameter_sweep(
#'   scenarios,
#'   sim_fn = sim_with_params,
#'   samples = 1
#' )
#'
#'
#' sweep_results
#' }
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
      n_sim = samples,
      num_initial_cases = x$num_initial_cases,
      r0community = x$index_R0,
      r0subclin = ifelse(
        "subclin_R0" %in% names(scenarios), x$subclin_R0, x$index_R0),
      k = x$k,
      delay_shape = x$delay_shape,
      delay_scale = x$delay_scale,
      prop_ascertain = x$control_effectiveness,
      quarantine = x$quarantine,
      prop_asym = x$prop_asym
    )[[1]],
    future.scheduling = 20,
    future.seed = TRUE
  )]

  return(scenario_sims[])
}
