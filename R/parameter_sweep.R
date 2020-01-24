#' Sweep across parameters
#'
#' @param scenarios A dataframe containing all gridded  scenarios - see the examples for the required structure.
#' Defaults to NULL.
#' @param samples Numeric, defaults to 1. The number of samples to take.
#' @param sim_fn Function, defaults to NULL. The vectorised model simulation function - see the examples
#' for usage.
#' @param show_progress Logical, defaults to `TRUE`. Show the progress of the parameter sweep.
#' @param ... Pass additional arguments to `sim_fn`.
#'
#'
#' @return
#' @export
#' @importFrom dplyr group_by mutate ungroup
#' @importFrom tidyr nest unnest
#' @importFrom furrr future_map
#' @examples
#'
#'
#' ## Set up scenario gird
#' scenarios <- tidyr::expand_grid(
#' index_R0 = c(1.5, 2.5, 3.5),
#' delay_mean = c(1, 2, 3),
#' control_effectiveness = c(0, 0.2, 0.4, 0.6, 0.8, 1),
#' inf_group = list(
#' tibble(inf_mean),
#' tibble(),
#' )
delay_mean: 1, 2, 3
control_effectiveness: 0, 0.2, 0.4, 0.6, 0.8, 1
inf_mean: a group of parameters for infectiousness etc.)) %>%
#' dplyr::mutate(scenario = 1:dplyr::n())
#'
#' ## Parameterise fixed paramters
#' sim_with_params <- purrr::partial(ringbp::wuhan_sim,
#'                                   wvaccYN = 0,define_6m = 239,initial.cases.pcluster = 5,
#'                                   initial.clusters = 5, prop.ascertain = 0.9, cap_cases = 4500, cap_max_days = 350,
#'                                   r0within = 0.5, r0Am = 2, overkkmiss = 1, overkk = 0.19, vefficacy = 1,
#'                                   vuptake = 0.90, ring.size = 100, time_to_protection = 2, incub_mean = 5,
#'                                   incub_var = 1.5, inf_mean = 5, inf_var = 1.5, delay_shape = 2.4114166,
#'                                   delay_rate = 0.3261129,time_to_isolation=1,outbreak_df_out = TRUE)
#'
#'
#' ## Set up multicore if using see ?future::plan for details
#' future::plan(future::sequential)
#'
#'
#' ## Run paramter sweep
#' sweep_results <- ringbp::parameter_sweep(scenarios, sim_fn = sim_with_params, samples = 5)
#'
#'
#' sweep_results
parameter_sweep <- function(scenarios = NULL, samples = 1,
                            sim_fn = NULL, show_progress = TRUE, ...){

  scenario_sims <- scenarios %>%
    dplyr::group_by(scenario) %>%
    tidyr::nest() %>%
    dplyr::mutate(sims = furrr::future_map(
      data,
      ~ sim_fn(n.cores = 1,
               n.sim = samples,
               ...),
      .progress = show_progress
    )) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(cols = "data")

  return(scenario_sims)
}
