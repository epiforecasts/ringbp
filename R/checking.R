#' Check the function arguments of the outbreak simulation functions
#'
#' @details If more than one argument is invalid, only a single error is
#'   thrown with the first invalid argument encountered.
#'
#'   Arguments being checked are taken from the parent environment
#'   ([parent.frame()]) rather than passed via named arguments.
#'
#' @return `TRUE` if all the checks pass or an error thrown by a \pkg{checkmate}
#' `assert_*()` function if one or more of the inputs is invalid.
#' @keywords internal
check_outbreak_input <- function() {
  ## get name of the calling function as a character string
  func <- deparse(as.list(sys.call(-1))[[1]])
  func <- gsub(pattern = "ringbp::", replacement = "", x = func)
  func <- match.arg(func, choices = c(
    "outbreak_setup", "outbreak_step", "outbreak_model", "scenario_sim"
  ))
  args <- as.list(parent.frame())

  checkmate::assert_function(args$incubation_period)
  checkmate::assert_function(args$onset_to_isolation)
  checkmate::assert_number(args$prop_asymptomatic, lower = 0, upper = 1)

  if (func %in% c("outbreak_setup", "outbreak_model", "scenario_sim")) {
    checkmate::assert_number(args$initial_cases, lower = 1, finite = TRUE)
  }

  if (func %in% c("outbreak_step", "outbreak_model", "scenario_sim")) {
    checkmate::assert_number(args$r0_community, lower = 0, finite = TRUE)
    checkmate::assert_number(args$r0_isolated, lower = 0, finite = TRUE)
    checkmate::assert_number(args$r0_asymptomatic, lower = 0, finite = TRUE)
    checkmate::assert_number(args$disp_community, lower = 0, finite = TRUE)
    checkmate::assert_number(args$disp_isolated, lower = 0, finite = TRUE)
    checkmate::assert_number(args$disp_asymptomatic, lower = 0, finite = TRUE)
    checkmate::assert_number(args$k)
    checkmate::assert_number(args$prop_ascertain, lower = 0, upper = 1)
    checkmate::assert_logical(args$quarantine, any.missing = FALSE, len = 1)
  }

  if (func %in% c("outbreak_model", "scenario_sim")) {
    checkmate::assert_int(args$cap_max_days, lower = 1)
    checkmate::assert_int(args$cap_cases, lower = 1)
  }

  if (func == "outbreak_step") {
    checkmate::assert_data_table(args$case_data)
  }

  if (func == "scenario_sim") {
    checkmate::assert_number(args$n, lower = 1, finite = TRUE)
  }

  return(TRUE)
}
