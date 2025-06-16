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
    # offspring distribution, incubation_period and onset_to_isolation are
    # strictly positive
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
