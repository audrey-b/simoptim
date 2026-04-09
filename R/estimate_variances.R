#' Estimate between-dataset and within-dataset variances
#'
#' Estimates two variance components from a set of simulation datasets:
#' the between-dataset variance of the estimator (\eqn{\sigma_1^2}) and the
#' average within-dataset posterior variance (\eqn{\sigma_2^2}). These are
#' the key inputs to \code{\link{optim}} and \code{\link{optim_2}} for
#' computing optimal sampling effort allocation.
#'
#' Requires a minimum of 2 datasets. All input vectors must have the same
#' length and be ordered consistently (i.e. element \code{i} of each vector
#' must correspond to the same dataset).
#'
#' @param estimates A numeric vector of posterior means, one per simulation
#'   dataset.
#' @param mcmcses A numeric vector of Monte Carlo standard errors of the
#'   posterior mean, one per simulation dataset.
#' @param posterior.variances A numeric vector of posterior variances, one per
#'   simulation dataset.
#'
#' @return A named list with two numeric elements:
#'   \describe{
#'     \item{estimate_var1}{Estimated between-dataset variance
#'       \eqn{\hat{\sigma}_1^2}, computed as the sample variance of
#'       \code{estimates} minus the mean squared MCMCSE.}
#'     \item{estimate_var2}{Estimated within-dataset posterior variance
#'       \eqn{\hat{\sigma}_2^2}, computed as the mean of
#'       \code{posterior.variances}.}
#'   }
#'
#' @note \code{estimate_var1} can be negative in small samples or when MCMC
#'   error is large relative to between-dataset variation. Consider whether
#'   a non-negativity constraint is appropriate for your use case.
#'
#' @seealso \code{\link{summarize_mcmc}}, \code{\link{optim}},
#'   \code{\link{optim_2}}
#' @export

estimate_variances <- function(estimates,
                               mcmcses,
                               posterior.variances){

  estimate_var2 = mean(posterior.variances)

  estimate_var1 = stats::var(estimates) - mean(mcmcses^2)  #should we guarantee non-negative?

  return(list(estimate_var1 = estimate_var1,
              estimate_var2 = estimate_var2))
}
