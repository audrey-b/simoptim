#' Compute optimal MCMC sampling allocation (ratio parameterisation)
#'
#' Determines the optimal number of simulation datasets \eqn{r} and effective
#' posterior samples per dataset \eqn{m} using ratios of timing and variance
#' components, rather than their absolute values. See \code{\link{optimize_allocation}} for the full
#' parameterisation.
#'
#' @param time_ratio Ratio of average burn-in time to average time per
#'   effective sample (\eqn{\bar t_{\text{1}} / \bar t_{\text{2}}}).
#' @param variance_ratio Ratio of between-dataset posterior variance to
#'   within-dataset variance (\eqn{\sigma_1^2 / \sigma_2^2}).
#' @param p Standardised precision target \eqn{p = \Delta / \sigma_1}, where
#'  \eqn{\Delta} is the maximum acceptable total MCMCSE.
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
#' @seealso \code{\link{optimize_allocation}}, \code{\link{estimate_variances}},
#'   \code{\link{compute_mcmcse}}
#' @export
#'
#' @examples
#' optimize_allocation_from_ratios(time_ratio = 10^5, variance_ratio = 1, min_m = 20, p = 0.05)

optimize_allocation_from_ratios <- function(time_ratio,
                                            variance_ratio,
                                            p,
                                            min_m = 1){

  if (!is.numeric(time_ratio) || time_ratio <= 0)
    stop("'time_ratio' must be a positive number.")
  if (!is.numeric(variance_ratio) || variance_ratio <= 0)
    stop("'variance_ratio' must be a positive number.")
  if (!is.numeric(p) || p <= 0 || p >= 1)
    stop("'p' must be a number strictly between 0 and 1.")
  if (!isTRUE(all.equal(min_m, as.integer(min_m))) || min_m < 1)
    stop("'min_m' must be a positive integer.")

  m_opt <- max(min_m,
               sqrt(time_ratio / variance_ratio))

  m_opt <- ceiling(m_opt)

  r_opt <- 1/p^2 * (1 + 1/variance_ratio/m_opt)

  return(list(r = ceiling(r_opt), m = m_opt))
}
