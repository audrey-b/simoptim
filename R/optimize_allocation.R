#' Compute optimal MCMC sampling allocation (full parameterisation)
#'
#' Determines the optimal number of simulation datasets \eqn{r} and effective
#' posterior samples per dataset \eqn{m} that minimise total computational
#' cost, subject to a precision constraint on the simulation study estimator.
#' This version takes all variance and timing quantities directly. See
#' \code{\link{optimize_allocation_from_ratios}} for a version that accepts ratios instead.
#'
#' @param burnin_time Average burn-in time per MCMC run (in any consistent
#'   time unit, e.g. seconds).
#' @param eff_sample_time Average time to generate one effective posterior sample
#'   after burn-in (same units as \code{burnin_time}).
#' @param var_between Between-dataset variance \eqn{\sigma_1^2}. See
#'   \code{\link{estimate_variances}}.
#' @param var_within Average within-dataset posterior variance \eqn{\sigma_2^2}.
#'   See \code{\link{estimate_variances}}.
#' @param delta The maximum acceptable total MCMCSE (i.e. the precision
#'   target). Exactly one of \code{delta} or \code{p} must be specified.
#' @param p Standardised precision target, defined as
#'   \eqn{p = \Delta / \sigma_1}. Exactly one of \code{Delta} or \code{p}
#'   must be specified. If both are given, they must be mutually consistent.
#' @param min_m Minimum allowable number of effective posterior samples per
#'   dataset. Must be a positive integer. Defaults to \code{1}.
#'
#' @return A named list with two integer-valued elements:
#'   \describe{
#'     \item{r}{Optimal number of simulation datasets.}
#'     \item{m}{Optimal number of effective posterior samples per dataset,
#'       at least \code{min_m}.}
#'   }
#'
#' @seealso \code{\link{optimize_allocation_from_ratios}}, \code{\link{estimate_variances}},
#'   \code{\link{compute_mcmcse}}
#' @export
#'
#' @examples
#' # Using standardised precision p
#' optimize_allocation(burnin_time = 1, eff_sample_time = 1, var_between = 0.95 * 0.05,
#'       var_within = 1, min_m = 20, p = 0.05)
#'
#' # Equivalently, using delta directly
#' optimize_allocation(burnin_time = 1, eff_sample_time = 1, var_between = 0.95 * 0.05,
#'       var_within = 1, min_m = 20, delta = 0.05 * sqrt(0.95 * 0.05))

optimize_allocation <- function(burnin_time,
                                eff_sample_time,
                                var_between,
                                var_within,
                                delta = NULL,
                                p = NULL,
                                min_m = 1){

  if (!is.numeric(burnin_time) || burnin_time <= 0)
    stop("'burnin_time' must be a positive number.")
  if (!is.numeric(eff_sample_time) || eff_sample_time <= 0)
    stop("'eff_sample_time' must be a positive number.")
  if (!is.numeric(var_between) || var_between <= 0)
    stop("'var_between' must be a positive number.")
  if (!is.numeric(var_within) || var_within <= 0)
    stop("'var_within' must be a positive number.")
  if (!isTRUE(all.equal(min_m, as.integer(min_m))) || min_m < 1)
    stop("'min_m' must be a positive integer.")

  if (is.null(p) && is.null(delta))
    stop("One of 'delta' or 'p' must be specified.")
  if (!is.null(delta) && (!is.numeric(delta) || delta <= 0))
    stop("'delta' must be a positive number.")
  if (!is.null(p) && (!is.numeric(p) || p <= 0 || p >= 1))
    stop("'p' must be a number strictly between 0 and 1.")
  if (!is.null(p) && !is.null(delta) && !isTRUE(all.equal(p, delta / sqrt(var_between))))
    stop("Specified values of 'delta' and 'p' are not compatible.")

  if (is.null(p)) p <- delta / sqrt(var_between)

  m_opt <- max(min_m,
               sqrt(burnin_time/eff_sample_time * var_within/var_between))

  m_opt <- ceiling(m_opt)

  r_opt <- 1/p^2 * (1 + var_within/var_between/m_opt)

  return(list(r = ceiling(r_opt),
              m = m_opt))
}
