#' Sample the offspring distributions for cases that can be in either the
#' _community_, _isolated_ or _asymptomatic_ states and transition between
#' states
#'
#' Samples from the offspring distributions (see [offspring_opts()]) and adds
#'   the next generation of transmission events by reference to `case_data`.
#'   The generation times for each infector-infectee pair is also sampled and
#'   is returned from the function.
#'
#' @details A case's offspring cannot simply be sampled from a single
#'   offspring distribution, as the case might become isolated before
#'   infecting some or all of those cases. To account for cases that
#'   transition between states (_community_ -> _isolated_ for symptomatic
#'   cases, _asymptomatic_ -> _isolated_ for asymptomatic cases) we sample
#'   offspring from both the pre-isolation and the post-isolation (`isolated`)
#'   distributions, assign all new cases a generation time, and then discard
#'   pre-isolation offspring with a generation time after the isolation time
#'   and post-isolation offspring with a generation time before it.
#'
#' @inheritParams outbreak_step
#' @inheritParams incubation_to_generation_time
#' @inheritParams delay_opts
#'
#' @autoglobal
#'
#' @return A `numeric` vector with the generation times for the new cases
#'   exposure/infection times.
#'
#'   ***Note*** The `case_data` supplied to the function is modified by
#'   references, see [data.table::set()] for more information.
#' @keywords internal
sample_offspring <- function(case_data, offspring, alpha, latent_period) {

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

  # asymptomatic cases transmit via the asymptomatic offspring distribution
  # before isolation and the isolated distribution after isolation, so (as for
  # symptomatic cases) both are sampled and later subset by the isolation time
  asymptomatic <- offspring$asymptomatic(sum(asymptomatic_idx))
  isolated_asymptomatic <- offspring$isolated(sum(asymptomatic_idx))

  # get generation times for community and isolated cases
  community_exposure <- incubation_to_generation_time(
    symptom_onset_time = rep(new_cases$onset[symptomatic_idx], community),
    exposure_time = rep(new_cases$exposure[symptomatic_idx], community),
    alpha = alpha,
    latent_period = latent_period
  )
  names(community_exposure) <- rep(new_cases$caseid[symptomatic_idx], community)
  isolated_exposure <- incubation_to_generation_time(
    symptom_onset_time = rep(new_cases$onset[symptomatic_idx], isolated),
    exposure_time = rep(new_cases$exposure[symptomatic_idx], isolated),
    alpha = alpha,
    latent_period = latent_period
  )
  names(isolated_exposure) <- rep(new_cases$caseid[symptomatic_idx], isolated)

  # get generation times for asymptomatic cases (asymptomatic + isolated);
  # incubation_to_generation_time() returns an empty vector for generations
  # with no asymptomatic cases
  asymptomatic_exposure <- incubation_to_generation_time(
    symptom_onset_time = rep(new_cases$onset[asymptomatic_idx], asymptomatic),
    exposure_time = rep(new_cases$exposure[asymptomatic_idx], asymptomatic),
    alpha = alpha,
    latent_period = latent_period
  )
  names(asymptomatic_exposure) <- rep(new_cases$caseid[asymptomatic_idx], asymptomatic)
  isolated_asymptomatic_exposure <- incubation_to_generation_time(
    symptom_onset_time = rep(new_cases$onset[asymptomatic_idx], isolated_asymptomatic),
    exposure_time = rep(new_cases$exposure[asymptomatic_idx], isolated_asymptomatic),
    alpha = alpha,
    latent_period = latent_period
  )
  names(isolated_asymptomatic_exposure) <-
    rep(new_cases$caseid[asymptomatic_idx], isolated_asymptomatic)

  # subset transmission events based on infector isolation time and infectee
  # exposure time: pre-isolation transmission is kept before the infector's
  # isolation time, post-isolation (isolated) transmission after it
  infect_before_isolate <- community_exposure < rep(new_cases$isolated_time[symptomatic_idx], community)
  community_exposure <- community_exposure[infect_before_isolate]
  infect_after_isolate <- isolated_exposure > rep(new_cases$isolated_time[symptomatic_idx], isolated)
  isolated_exposure <- isolated_exposure[infect_after_isolate]

  # the same split for asymptomatic cases: asymptomatic transmission before
  # isolation, isolated transmission after. When an asymptomatic case is never
  # isolated (isolated_time is Inf) all asymptomatic transmission is retained
  # and none of the isolated transmission is
  asympt_before_isolate <- asymptomatic_exposure < rep(new_cases$isolated_time[asymptomatic_idx], asymptomatic)
  asymptomatic_exposure <- asymptomatic_exposure[asympt_before_isolate]
  asympt_after_isolate <- isolated_asymptomatic_exposure > rep(new_cases$isolated_time[asymptomatic_idx], isolated_asymptomatic)
  isolated_asymptomatic_exposure <- isolated_asymptomatic_exposure[asympt_after_isolate]

  # infectee exposure time for all transmission events
  exposure <- c(
    community_exposure, isolated_exposure,
    asymptomatic_exposure, isolated_asymptomatic_exposure
  )

  next_gen <- table(names(exposure))

  # assign next generation of cases by reference
  case_data[sampled == FALSE, new_cases := 0L]
  case_data[as.numeric(names(next_gen)), new_cases := next_gen]

  # return generation times
  exposure
}
