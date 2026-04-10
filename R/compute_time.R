#' Compute total estimated compute time
#'
#' Computes the total estimated compute time for a simulation study given
#' an allocation of datasets and effective posterior samples.
#'
#' @param r A positive integer giving the number of simulation datasets.
#' @param m A positive integer giving the number of effective posterior
#'   samples per dataset.
#' @param burnin_time Average burn-in time per MCMC run (in any consistent
#'   time unit, e.g. seconds).
#' @param eff_sample_time Average time to generate one effective posterior
#'   sample after burn-in (same units as \code{time_burnin}).
#' @param n_cores A positive integer giving the number of cores available for
#'   parallel execution. Defaults to \code{1} (no parallelisation).
#'
#' @return A single positive numeric value giving the total estimated compute
#'   time, calculated as \eqn{r \cdot (t_{\text{burnin}} + m \cdot
#'   t_{\text{per eff sample}})}.
#'
#' @seealso \code{\link{optimize_allocation}},
#'   \code{\link{optimize_allocation_from_ratios}}
#' @export
#'
#' @examples
#' # Single core
#' compute_time(r = 100, m = 20, burnin_time = 10, eff_sample_time = 1)
#'
#' # With 4 cores
#' compute_time(r = 100, m = 20, burnin_time = 10, eff_sample_time = 1,
#'              n_cores = 4)

compute_time <- function(r, m, burnin_time, eff_sample_time, n_cores = 1) {
  if (!is.numeric(r) || r < 1 || !isTRUE(all.equal(r, as.integer(r))))
    stop("'r' must be a positive integer.")
  if (!is.numeric(m) || m < 1 || !isTRUE(all.equal(m, as.integer(m))))
    stop("'m' must be a positive integer.")
  if (!is.numeric(burnin_time) || burnin_time <= 0)
    stop("'burnin_time' must be a positive number.")
  if (!is.numeric(eff_sample_time) || eff_sample_time <= 0)
    stop("'eff_sample_time' must be a positive number.")
  if (!is.numeric(n_cores) || n_cores < 1 || !isTRUE(all.equal(n_cores, as.integer(n_cores))))
    stop("'n_cores' must be a positive integer.")

  return(r * (burnin_time + eff_sample_time * m)/ n_cores)
}
