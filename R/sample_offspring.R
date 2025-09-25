#' Sample the offspring distributions for cases that can be in either the
#' _community_, _isolated_ or _asymptomatic_ states and transition between
#' states
#'
#' Samples from the offspring distributions (see [offspring_opts()]) and adds
#'   the next generation of transmission events by reference to `case_data`.
#'   The generation times for each infector-infectee pair is also sampled and
#'   is returned from the function.
#'
#' @details The offspring distribution for a case in the community cannot simply
#'   be sampled from the `community` offspring distribution as it might become
#'   isolated before infecting some or all of those cases.
#'   To account for cases that transition between states (for now only
#'   _community_ -> _isolated_) we draw from the offspring from both
#'   distributions, assign all new cases a generation time, and then discard
#'   the ones that have generation time <= isolation time (for those from the
#'   isolated offspring distribution) or generation time > isolation time
#'   (for those from the community offspring distribution), respectively.
#'
#' @inheritParams outbreak_step
#'
#' @autoglobal
#'
#' @return A `numeric` vector with the generation times for the new cases
#'   exposure/infection times.
#'
#'   ***Note*** The `case_data` supplied to the function is modified by
#'   references, see [data.table::set()] for more information.
#' @keywords internal
sample_offspring <- function(case_data, offspring) {

  # subset to cases in current generation
  new_cases <- case_data[sampled == FALSE]

  # logical vectors with the row index of asymptomatic and symptomatic cases
  asymptomatic_idx <- new_cases$asymptomatic
  symptomatic_idx <- !asymptomatic_idx

  # cases cannot be isolated at the start of their generation so both community
  # and isolated offspring distribution are sampled for community cases and
  # are subset by whether the generation time is before or after the isolation
  # time
  community <- offspring$community(sum(symptomatic_idx))
  isolated <- offspring$isolated(sum(symptomatic_idx))

  # asymptomatic cases are known from the start of their generation so their
  # offspring can be sampled directly from the asymptomatic offspring
  # distribution
  asymptomatic <- offspring$asymptomatic(sum(asymptomatic_idx))

  # get generation times for community and isolated cases
  community_exposure <- incubation_to_generation_time(
    rep(new_cases$onset[symptomatic_idx], community), event_probs$alpha
  )
  names(community_exposure) <- rep(new_cases$caseid[symptomatic_idx], community)
  isolated_exposure <- incubation_to_generation_time(
    rep(new_cases$onset[symptomatic_idx], isolated), event_probs$alpha
  )
  names(isolated_exposure) <- rep(new_cases$caseid[symptomatic_idx], isolated)

  # if there is any transmission from asymptomatic cases get generation time
  if (length(asymptomatic) > 0) {
    asymptomatic_exposure <- incubation_to_generation_time(
      rep(new_cases$onset[asymptomatic_idx], asymptomatic), event_probs$alpha
    )
    names(asymptomatic_exposure) <- rep(new_cases$caseid[asymptomatic_idx], asymptomatic)
  } else {
    # create asymptomatic_exposure for all exposure vector below (NULL dropped)
    asymptomatic_exposure <- NULL
  }

  # subset transmission events in community and isolation based on infector
  # isolation time and infectee exposure time
  infect_before_isolate <- community_exposure < rep(new_cases$isolated_time[symptomatic_idx], community)
  community_exposure <- community_exposure[infect_before_isolate]
  infect_after_isolate <- isolated_exposure > rep(new_cases$isolated_time[symptomatic_idx], isolated)
  isolated_exposure <- isolated_exposure[infect_after_isolate]

  # infectee exposure time for all transmission events
  exposure <- c(community_exposure, isolated_exposure, asymptomatic_exposure)

  next_gen <- table(names(exposure))

  # assign next generation of cases by reference
  case_data[sampled == FALSE, new_cases := 0L]
  case_data[as.numeric(names(next_gen)), new_cases := next_gen]

  # return generation times
  exposure
}
