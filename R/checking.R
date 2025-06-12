#' Check the function arguments of the outbreak simulation functions
#'
#' @details If more than one argument is invalid, only a single error is
#'   thrown with the first invalid argument encountered.
#'
#'   Arguments being checked are taken from the parent environment
#'   ([parent.frame()]) rather than passed via named arguments.
#'
#' @param parent_func a `character` string: the name of the calling function.
#'
#' @return `TRUE` if all the checks pass or an error thrown by a \pkg{checkmate}
#' `assert_*()` function if one or more of the inputs is invalid.
#' @keywords internal
check_outbreak_input <- function(parent_func = c("parameters", "control")) {

  parent_func <- match.arg(parent_func)

  args <- as.list(parent.frame())

  if (parent_func == "parameters") {
    checkmate::assert_number(args$initial_cases, lower = 1, finite = TRUE)
    checkmate::assert_number(args$r0_community, lower = 0, finite = TRUE)
    checkmate::assert_number(args$r0_isolated, lower = 0, finite = TRUE)
    checkmate::assert_number(args$r0_asymptomatic, lower = 0, finite = TRUE)
    checkmate::assert_number(args$disp_community, lower = 0, finite = TRUE)
    checkmate::assert_number(args$disp_isolated, lower = 0, finite = TRUE)
    checkmate::assert_number(args$disp_asymptomatic, lower = 0, finite = TRUE)
    check_dist_func(args$incubation_period, dist_name = "incubation_period")
    checkmate::assert_number(args$prop_presymptomatic, lower = 0, upper = 1)
    check_dist_func(args$onset_to_isolation, dist_name = "onset_to_isolation")
    checkmate::assert_number(args$prop_ascertain, lower = 0, upper = 1)
    checkmate::assert_number(args$prop_asymptomatic, lower = 0, upper = 1)
    checkmate::assert_logical(args$quarantine, any.missing = FALSE, len = 1)
  }

  if (parent_func == "control") {
    checkmate::assert_int(args$cap_max_days, lower = 1)
    checkmate::assert_int(args$cap_cases, lower = 1)
  }

  return(TRUE)
}

#' Check a distribution function returns valid output and has the correct number
#' of arguments
#'
#' @param func a [function].
#' @param dist_name a `character` string: the name of the distribution function
#'   being passed (e.g. `"incubation_period"`)
#' @param n_req_args a single `numeric`: the number of required arguments
#'
#' @return `TRUE` if all the checks pass or an error is thrown if the
#'   distribution function is invalid.
#' @keywords internal
check_dist_func <- function(func,
                            dist_name,
                            n_req_args = 1) {

  checkmate::assert_function(func)
  checkmate::assert_count(n_req_args, positive = TRUE)
  # using formals(args(fn)) to allow checking args of builtin primitives
  # for which formals(fn) would return NULL and cause the check to error
  # errors non-informatively for specials such as `if`
  valid_func <- checkmate::test_function(func) &&
    sum(mapply(function(x, y) { # nolint undesirable function
      is.name(x) && y != "..."
    }, formals(args(func)), names(formals(args(func))))) == n_req_args &&
    # incubation_period and onset_to_isolation are strictly positive
    checkmate::test_numeric(
      func(1e5), lower = 0, finite = TRUE, any.missing = FALSE, len = 1e5
    )

  if (!valid_func) {
    stop(
      dist_name, " must be a function with ", n_req_args, " argument(s) that ",
      "returns positive numbers."
    )
  }

  return(TRUE)
}

