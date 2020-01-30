#' Draw new cluster cases from index case
#' @author Joel Hellewell
#' @param index_case Index case of cluster
#' @param r0community basic reproduction number for non-isolated cases
#' @param disp.com dispersion parameter for negative binomial distribution for non-isolated cases
#' @param total.clusters current number of clusters
#' @param incubfn function to sample from incubation period distribution
#' @param infecfn function to sample from infectiousness distribution
#' @param delayfn function to sample from distribution for delay from symptom onset to isolation
#'
#' @return
#' @export
#' @importFrom dplyr mutate bind_rows
#' @examples
new_cluster <- function(index_case, r0community, disp.com, total.clusters, incubfn, infecfn, delayfn) {

  if (index_case$cluster < total.clusters) {
    return(index_case) # filters out old clusters and returns them unchanged
  }

  index_case$isolated <- TRUE # this case has had a chance to infect, so now it is isolated

  new_cases <- rnbinom(1, size = disp.com, mu = r0community) # number of new cases in the cluster formed from missed case

  if (new_cases == 0) {
    return(index_case)
  }else{
    cluster_data <-
      data.frame(
        cluster = rep(index_case$cluster, new_cases),
        # set cluster number to index case cluster number
        onset = index_case$latent + incubfn(new_cases),
        # onset time from when infector infected + incubation period
        exposure = rep(index_case$latent, new_cases),
        # exposure time is when infector infected
        latent = index_case$latent + infecfn(new_cases),
        # time when these cases will infect
        missed = rep(FALSE, new_cases)
      ) %>% # none missed since they are all in this new cluster

      dplyr::mutate(isolated_time = min(onset) + delayfn(1), # cluster is isolated at minimum onset time + delay
             isolated = ifelse(latent > isolated_time, TRUE, FALSE)) %>% # cases that don't infect before isolation are marked isolated
      dplyr::bind_rows(index_case) # add index case of cluster back to dataset

    return(cluster_data)
  }
}
