#' Draw secondary cases from cases in a cluster
#' @author Joel Hellewell
#' @param index_case data from of cases within cluster
#' @param r0isolated basic reproduction number for isolated cases
#' @param r0community basic reproduction number for non-isolated cases
#' @param disp.iso dispersion parameter for negative binomial distribution for isolated cases
#' @param disp.com dispersion parameter for negative binomial distribution for non-isolated cases
#' @param prop.ascertain probability that contact tracing will discover infected contact
#' @param incubfn function to sample from incubation period distribution
#' @param infecfn function to sample from infectiousness distribution
#' @param delayfn function to sample from distribution for delay from symptom onset to isolation
#'
#' @return
#' @export
#' @importFrom dplyr mutate filter group_by group_split bind_rows select
#' @importFrom purrr map_lgl map
#' @examples
secondary_draw <- function(index_case, r0isolated, r0community, disp.iso, disp.com,prop.ascertain,
                           incubfn, infecfn, delayfn) {

  vect_isTRUE <- function(x) {
    purrr::map_lgl(x, isTRUE)
  }

  index_case <- dplyr::mutate(index_case,
                              new.cases = purrr::map2_dbl(
                                ifelse(vect_isTRUE(isolated), disp.iso, disp.com),
                                ifelse(vect_isTRUE(isolated), r0isolated, r0community),
                                ~ rnbinom(1, size = .x, mu = .y)),isolated = TRUE)


  new_cases <- dplyr::filter(index_case, new.cases > 0)


  if (nrow(new_cases) > 0) {
    new_cases <- dplyr::group_by(new_cases, caseid)

    new_cases <- dplyr::group_split(new_cases)

    new_cases <- purrr::map(new_cases,~mutate(data.frame(exposure = rep(.$latent, times = .$new.cases),
                                                  # exposure is taken from infector data
                                                  missed = rbernoulli(n = .$new.cases, p = prop.ascertain)),
                                       ## whether these cases remain in cluster or are missed
                                       onset = exposure + incubfn(.$new.cases),
                                       # onset of new cases
                                       latent = exposure + infecfn(.$new.cases),
                                       # when new cases infect people
                                       cluster = ifelse(vect_isTRUE(missed), NA, .$cluster),
                                       # remain within cluster with prob = prop.ascertain
                                       # set isolated time from onset, same as infector if you remain within cluster
                                       isolated_time = ifelse(vect_isTRUE(missed),onset + delayfn(.$new.cases),
                                                              .$isolated_time + delayfn(.$new.cases)),isolated = !vect_isTRUE(missed)))

    index_case <- dplyr::bind_rows(dplyr::select(index_case,-new.cases), new_cases)

  } else{
    index_case <- dplyr::select(index_case,-new.cases)
  }

  return(index_case)
} # End function
