#' Check a distribution function returns valid output and has the correct number
#' of arguments
#'
#' @param func a [function].
#' @param dist_name a `character` string: the name of the distribution function
#'   being passed (e.g. `"incubation_period"`)
#' @param n_req_args a single `numeric`: the number of required arguments
#' @param func_eval_min a single `numeric`: the lower bound of valid numeric
#'   output by the `func` argument (i.e. the minimum of the acceptable value in
#'   the function's codomain). The default is `0` so `func` must return
#'   non-negative values.
#'
#' @return `TRUE` if all the checks pass or an error is thrown if the
#'   distribution function is invalid.
#' @keywords internal
check_dist_func <- function(func,
                            dist_name,
                            n_req_args = 1,
                            func_eval_min = 0) {

  checkmate::assert_function(func)
  checkmate::assert_count(n_req_args, positive = TRUE)
  checkmate::assert_count(func_eval_min)
  # using formals(args(fn)) to allow checking args of builtin primitives
  # for which formals(fn) would return NULL and cause the check to error
  # errors non-informatively for specials such as `if`
  valid_func <- checkmate::test_function(func) &&
    sum(mapply(function(x, y) { # nolint undesirable function
      is.name(x) && y != "..."
    }, formals(args(func)), names(formals(args(func))))) == n_req_args &&
    # offspring distribution, incubation_period and onset_to_isolation are
    # non-negative
    checkmate::test_numeric(
      func(1e5),
      lower = func_eval_min,
      finite = TRUE,
      any.missing = FALSE,
      len = 1e5
    )

  if (!valid_func) {
    stop(
      dist_name, " must be a function with ", n_req_args, " argument(s) that ",
      "returns non-negative numbers."
    )
  }

  return(TRUE)
}
