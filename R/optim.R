#' Compute optimal MCMC sampling allocation (full parameterisation)
#'
#' Determines the optimal number of simulation datasets \eqn{r} and effective
#' posterior samples per dataset \eqn{m} that minimise total computational
#' cost, subject to a precision constraint on the simulation study estimator.
#' This version takes all variance and timing quantities directly. See
#' \code{\link{optim_2}} for a version that accepts ratios instead.
#'
#' @param time_burnin Average burn-in time per MCMC run (in any consistent
#'   time unit, e.g. seconds).
#' @param time_per_es Average time to generate one effective posterior sample
#'   after burn-in (same units as \code{time_burnin}).
#' @param var_1 Between-dataset variance \eqn{\sigma_1^2}. See
#'   \code{\link{estimate_variances}}.
#' @param var_2 Average within-dataset posterior variance \eqn{\sigma_2^2}.
#'   See \code{\link{estimate_variances}}.
#' @param delta The maximum acceptable total MCMCSE (i.e. the precision
#'   target). Exactly one of \code{delta} or \code{p} must be specified.
#' @param min_m Minimum allowable number of effective posterior samples per
#'   dataset. Must be a positive integer. Defaults to \code{1}.
#' @param p Standardised precision target, defined as
#'   \eqn{p = \Delta / \sigma_1}. Exactly one of \code{Delta} or \code{p}
#'   must be specified. If both are given, they must be mutually consistent.
#'
#' @return A named list with two integer-valued elements:
#'   \describe{
#'     \item{r}{Optimal number of simulation datasets.}
#'     \item{m}{Optimal number of effective posterior samples per dataset,
#'       at least \code{min_m}.}
#'   }
#'
#' @seealso \code{\link{optim_2}}, \code{\link{estimate_variances}},
#'   \code{\link{mcmcse_total}}
#' @export
#'
#' @examples
#' # Using standardised precision p
#' optim(time_burnin = 1, time_per_es = 1, var_1 = 0.95 * 0.05,
#'       var_2 = 1, min_m = 20, p = 0.05)
#'
#' # Equivalently, using delta directly
#' optim(time_burnin = 1, time_per_es = 1, var_1 = 0.95 * 0.05,
#'       var_2 = 1, min_m = 20, delta = 0.05 * sqrt(0.95 * 0.05))

optim <- function(time_burnin,
                  time_per_es,
                  var_1,
                  var_2,
                  delta = NULL,
                  min_m = 1,
                  p = NULL){

  if(is.null(p) & is.null(delta)) stop("Error: one of delta or p must be specified")

  if(!is.null(p) & !is.null(delta) & !identical(p, delta/sqrt(var_1))) stop("Error: specified values of delta and p are not compatible")

  if(is.null(p)) p <- delta/sqrt(var_1)

  if(!isTRUE(all.equal(min_m, as.integer(min_m)))) stop("min_m must be an integer")

  m_optim <- max(min_m,
                 sqrt(time_burnin/time_per_es * var_2/var_1))

  m_optim <- ceiling(m_optim)

  r_optim <- 1/p^2 * (1 + var_2/var_1/m_optim)

  return(list(r = ceiling(r_optim), m = m_optim))
}
