#' Compute the total Monte Carlo standard error
#'
#' Computes the total MCMCSE of the simulation study estimator as a function
#' of the number of simulation datasets \eqn{r}, the number of effective
#' posterior samples per dataset \eqn{m}, and the two variance components.
#' This can be used to assess the precision of a given allocation before or
#' after running a study.
#'
#' @param var_1 Between-dataset variance \eqn{\sigma_1^2}. See
#'   \code{\link{estimate_variances}}.
#' @param var_2 Average within-dataset posterior variance \eqn{\sigma_2^2}.
#'   See \code{\link{estimate_variances}}.
#' @param r A positive integer giving the number of simulation datasets.
#' @param m A positive integer giving the number of effective posterior
#'   samples per dataset.
#'
#' @return A single non-negative numeric value: the total MCMCSE,
#'   \eqn{\sqrt{\sigma_1^2 / r + \sigma_2^2 / (r \cdot m)}}.
#'
#' @seealso \code{\link{optim}}, \code{\link{optim_2}}
#' @export
#'
#' @examples
#' mcmcse_total(var_1 = 0.5, var_2 = 1.0, r = 100, m = 50)

mcmcse_total <- function(var_1, #sigma_1^2 in the paper
                         var_2, #sigma_2^2 in the paper
                         r,
                         m){

  return(sqrt(var_1/r + var_2/r/m))
}
