#' Sweep across parameters
#'
#' @description Explore scenarios using gridding with sampling for parameters not in the grid. Parameters that
#' are included in the grid are currently hard coded. Use the `future` package to control parallisation
#' outside of the function.
#'
#' @param scenarios A dataframe containing all gridded  scenarios - see the examples for the required structure.
#' Defaults to NULL.
#' @param samples Numeric, defaults to 1. The number of samples to take.
#' @param sim_fn Function, defaults to NULL. The vectorised model simulation function - see the examples
#' for usage.
#' @param show_progress Logical, defaults to `TRUE`. Show the progress of the parameter sweep.
#'
#'
#' @return A nested tibble containing the parameters for each scenario and a nested list of output
#' from `wuhan_sim`.
#' @export
#' @importFrom dplyr group_by mutate ungroup
#' @importFrom tidyr nest unnest
#' @importFrom furrr future_map
#' @examples
#'
#' library(ringbp)
#'
#' ## Set up scenario gird
#' scenarios <- tidyr::expand_grid(
#'     ## Put parameters that are grouped by disease into this data.frame
#'     grouped_parameters = list(tibble::tibble(
#'                          disease = c("SARs", "Transition", "Flu"),
#'                          index_R0 = c(1.5, 2.5, 3.5),
#'                          inf_mean = c(9, 5, 2),
#'                          overkkmiss= c(0.16, 0.5, 1)
#'     )),
#'     delay_mean = seq(3, 7, 1),
#'     control_effectiveness = seq(0, 1, 0.2)) %>%
#'  tidyr::unnest("grouped_parameters") %>%
#'  dplyr::mutate(scenario = 1:dplyr::n())
#'
#' ## Parameterise fixed paramters
#' sim_with_params <- purrr::partial(ringbp::wuhan_sim,
#'                                   n.cores = 1, wvaccYN = 0,define_6m = 239,initial.cases.pcluster = 5,
#'                                   initial.clusters = 5, cap_cases = 4500, cap_max_days = 350,
#'                                   r0within = 0.5, overkk = 0.19, vefficacy = 1,
#'                                   vuptake = 0.90, ring.size = 100, time_to_protection = 2, incub_mean = 5,
#'                                   incub_var = 1.5, inf_var = 1.5, delay_var = 2,
#'                                   time_to_isolation=1, outbreak_df_out = TRUE)
#'
#'
#' ## Default is to run sequntially on a single core
#' future::plan(future::sequential)
#' ## Set up multicore if using see ?future::plan for details
#' #future::plan(future::multiprocess)
#'
#'
#' ## Run paramter sweep
#' sweep_results <- ringbp::parameter_sweep(scenarios, sim_fn = sim_with_params, samples = 1)
#'
#'
#' sweep_results
parameter_sweep <- function(scenarios = NULL, samples = 1,
                            sim_fn = NULL, show_progress = TRUE){

  scenario_sims <- scenarios %>%
    dplyr::group_by(scenario) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(sims = furrr::future_map(
      data,
      ~ sim_fn(n.sim = samples,
               r0community = .$index_R0,
               inf_mean = .$inf_mean,
               disp.com = .$disp.com,
               delay_mean = .$delay_mean,
               prop.ascertain = .$control_effectiveness
      ),
      .progress = show_progress
    )) %>%
    tidyr::unnest(cols = "data")

  return(scenario_sims)
}
