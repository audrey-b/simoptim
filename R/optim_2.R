#' Compute optimal MCMC sampling allocation (ratio parameterisation)
#'
#' Determines the optimal number of simulation datasets \eqn{r} and effective
#' posterior samples per dataset \eqn{m} using ratios of timing and variance
#' components, rather than their absolute values. See \code{\link{optim}} for the full
#' parameterisation.
#'
#' @param time_ratio Ratio of average burn-in time to average time per
#'   effective sample (\eqn{\bar t_{\text{1}} / \bar t_{\text{2}}}).
#' @param var_ratio Ratio of between-dataset posterior variance to
#'   within-dataset variance (\eqn{\sigma_1^2 / \sigma_2^2}).
#' @param p Standardised precision target \eqn{p = \Delta / \sigma_1}, where
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
#' @seealso \code{\link{optim}}, \code{\link{estimate_variances}},
#'   \code{\link{mcmcse_total}}
#' @export
#'
#' @examples
#' optim_2(time_ratio = 10^5, var_ratio = 1, min_m = 20, p = 0.05)

optim_2 <- function(time_ratio,
                    var_ratio,
                    p,
                    min_m = 1){

  m_optim <- max(min_m,
                 sqrt(time_ratio / var_ratio))

  m_optim <- ceiling(m_optim)

  r_optim <- 1/p^2 * (1 + 1/var_ratio/m_optim)

  return(list(r = ceiling(r_optim), m = m_optim))
}
