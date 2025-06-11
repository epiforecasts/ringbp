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
check_outbreak_input <- function(func = c("parameters", "control")) {

  func <- match.arg(func)

  args <- as.list(parent.frame())

  if (func == "parameters") {
    checkmate::assert_number(args$initial_cases, lower = 1, finite = TRUE)
    checkmate::assert_number(args$r0_community, lower = 0, finite = TRUE)
    checkmate::assert_number(args$r0_isolated, lower = 0, finite = TRUE)
    checkmate::assert_number(args$r0_asymptomatic, lower = 0, finite = TRUE)
    checkmate::assert_number(args$disp_community, lower = 0, finite = TRUE)
    checkmate::assert_number(args$disp_isolated, lower = 0, finite = TRUE)
    checkmate::assert_number(args$disp_asymptomatic, lower = 0, finite = TRUE)
    checkmate::assert_function(args$incubation_period)
    checkmate::assert_number(args$prop_presymptomatic, lower = 0, upper = 1)
    checkmate::assert_function(args$onset_to_isolation)
    checkmate::assert_number(args$prop_ascertain, lower = 0, upper = 1)
    checkmate::assert_number(args$prop_asymptomatic, lower = 0, upper = 1)
    checkmate::assert_logical(args$quarantine, any.missing = FALSE, len = 1)
  }

  if (func == "control") {
    checkmate::assert_int(args$cap_max_days, lower = 1)
    checkmate::assert_int(args$cap_cases, lower = 1)
  }

  return(TRUE)
}
