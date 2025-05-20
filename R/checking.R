#' Check the function arguments of the outbreak simulation functions
#'
#' @details If more than one argument is invalid, only a single error is
#'   thrown with the first invalid argument encountered.
#'
#' @param func A `character` string with the name of the function where the
#'   function is called to check the correct set of arguments.
#' @inheritParams scenario_sim
#' @inheritParams outbreak_setup
#' @inheritParams outbreak_step
#' @inheritParams outbreak_model
#'
#' @return `TRUE` if all the checks pass or an error thrown by a \pkg{checkmate}
#' `assert_*()` function if one or more of the inputs is invalid.
#' @keywords internal
check_outbreak_input <- function(func = c("outbreak_setup",
                                          "outbreak_step",
                                          "outbreak_model",
                                          "scenario_sim"),
                                 n,
                                 initial_cases,
                                 r0_community, r0_isolated, r0_asymptomatic,
                                 disp_community, disp_isolated, disp_asymptomatic,
                                 incubation_period, k,
                                 onset_to_isolation,
                                 prop_ascertain, prop_asymptomatic,
                                 cap_max_days, cap_cases,
                                 quarantine,
                                 case_data) {
  func <- match.arg(func)

  checkmate::assert_function(incubation_period)
  checkmate::assert_function(onset_to_isolation)
  checkmate::assert_number(prop_asymptomatic, lower = 0, upper = 1)

  if (func %in% c("outbreak_setup", "outbreak_model", "scenario_sim")) {
    checkmate::assert_number(initial_cases, lower = 1, finite = TRUE)
  }

  if (func %in% c("outbreak_step", "outbreak_model", "scenario_sim")) {
    checkmate::assert_number(r0_community, lower = 0, finite = TRUE)
    checkmate::assert_number(r0_isolated, lower = 0, finite = TRUE)
    checkmate::assert_number(r0_asymptomatic, lower = 0, finite = TRUE)
    checkmate::assert_number(disp_community, lower = 0, finite = TRUE)
    checkmate::assert_number(disp_isolated, lower = 0, finite = TRUE)
    checkmate::assert_number(disp_asymptomatic, lower = 0, finite = TRUE)
    checkmate::assert_number(k)
    checkmate::assert_number(prop_ascertain, lower = 0, upper = 1)
    checkmate::assert_logical(quarantine, any.missing = FALSE, len = 1)
  }

  if (func %in% c("outbreak_model", "scenario_sim")) {
    checkmate::assert_number(cap_max_days, lower = 1)
    checkmate::assert_number(cap_cases, lower = 1)
  }

  if (func == "outbreak_step") {
    checkmate::assert_data_table(case_data)
  }

  if (func == "scenario_sim") {
    checkmate::assert_number(n, lower = 1, finite = TRUE)
  }

  return(TRUE)
}
