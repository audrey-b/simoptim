#' Estimate between-dataset and within-dataset variances
#'
#' Estimates two variance components from a set of simulation datasets:
#' the between-dataset variance of the estimator (\eqn{\sigma_1^2}) and the
#' average within-dataset posterior variance (\eqn{\sigma_2^2}). These are
#' the key inputs to \code{\link{optimize_allocation}} and \code{\link{optimize_allocation_from_ratios}} for
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
#' @param posterior_variances A numeric vector of posterior variances, one per
#'   simulation dataset.
#'
#' @return A named list with two numeric elements:
#'   \describe{
#'     \item{var_between}{Estimated between-dataset variance
#'       \eqn{\hat{\sigma}_1^2}, computed as the sample variance of
#'       \code{estimates} minus the mean squared MCMCSE.}
#'     \item{var_within}{Estimated within-dataset posterior variance
#'       \eqn{\hat{\sigma}_2^2}, computed as the mean of
#'       \code{posterior_variances}.}
#'   }
#'
#' @note \code{var_between} can be negative in small samples or when MCMC
#'   error is large relative to between-dataset variation. Consider whether
#'   a non-negativity constraint is appropriate for your use case.
#'
#' @seealso \code{\link{summarize_mcmc}}, \code{\link{optimize_allocation}},
#'   \code{\link{optimize_allocation_from_ratios}}
#' @export

estimate_variances <- function(estimates,
                               mcmcses,
                               posterior_variances){

  if (!is.numeric(estimates) || !is.numeric(mcmcses) || !is.numeric(posterior_variances))
    stop("'estimates', 'mcmcses' and 'posterior_variances' must all be numeric vectors.")
  if (length(estimates) != length(mcmcses) || length(estimates) != length(posterior_variances))
    stop("'estimates', 'mcmcses' and 'posterior_variances' must all have the same length.")
  if (length(estimates) < 2)
    stop("At least 2 datasets are required.")
  if (any(mcmcses < 0))
    stop("'mcmcses' must be non-negative.")
  if (any(posterior_variances < 0))
    stop("'posterior_variances' must be non-negative.")

  var_within <- mean(posterior_variances)

  var_between <- stats::var(estimates) - mean(mcmcses^2)  #should we guarantee non-negative?

  if (var_between < 0) {
    warning("Estimated between-dataset variance (var_between) is negative.")}

  if (var_within < 0) {
      warning("Estimated within-dataset variance (var_within) is negative.")}

  return(list(var_between = var_between,
              var_within = var_within))
}
