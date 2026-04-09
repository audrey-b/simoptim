#' Compute the total Monte Carlo standard error
#'
#' Computes the total MCMCSE of the simulation study estimator as a function
#' of the number of simulation datasets \eqn{r}, the number of effective
#' posterior samples per dataset \eqn{m}, and the two variance components.
#' This can be used to assess the precision of a given allocation before or
#' after running a study.
#'
#' @param var_between Between-dataset variance \eqn{\sigma_1^2}. See
#'   \code{\link{estimate_variances}}.
#' @param var_within Average within-dataset posterior variance \eqn{\sigma_2^2}.
#'   See \code{\link{estimate_variances}}.
#' @param r A positive integer giving the number of simulation datasets.
#' @param m A positive integer giving the number of effective posterior
#'   samples per dataset.
#'
#' @return A single non-negative numeric value: the total MCMCSE,
#'   \eqn{\sqrt{\sigma_1^2 / r + \sigma_2^2 / (r \cdot m)}}.
#'
#' @seealso \code{\link{optimize_allocation}}, \code{\link{optimize_allocation_from_ratios}}
#' @export
#'
#' @examples
#' compute_mcmcse(var_between = 0.5, var_within = 1.0, r = 100, m = 50)

compute_mcmcse <- function(var_between, #sigma_1^2 in the paper
                           var_within, #sigma_2^2 in the paper
                           r,
                           m){

  if (!is.numeric(var_between) || var_between < 0)
    stop("'var_between' must be a non-negative number.")
  if (!is.numeric(var_within) || var_within < 0)
    stop("'var_within' must be a non-negative number.")
  if (!is.numeric(r) || r < 1 || !isTRUE(all.equal(r, as.integer(r))))
    stop("'r' must be a positive integer.")
  if (!is.numeric(m) || m < 1 || !isTRUE(all.equal(m, as.integer(m))))
    stop("'m' must be a positive integer.")

  return(sqrt(var_between/r + var_within/r/m))
}
