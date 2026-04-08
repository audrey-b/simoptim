#' optim
#'
#' A function to calculate the optimal allocation by specifying values for all variables.
#'
#' @param time_burnin average burnin time used by the MCMC
#' @param time_per_es average time per effective sample used by the MCMC after burnin
#' @param var_1 sigma_1^2 in the paper
#' @param var_2 sigma_2^2 in the paper
#' @param delta Delta in the paper
#' @param min_m blah
#' @param p Delta/sigma_1 in the paper
#' @return A list of optimal r and m.
#' @export
#'
#' @examples
#' optim(1, 1, 0.95*0.05, 1, min_m = 20, p = 0.05)

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
