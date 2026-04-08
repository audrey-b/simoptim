#' optim_2
#'
#' A function to calculate the optimal allocation by specifying ratios.
#'
#' @param time_ratio average burnin time / average time per effective sample
#' @param var_ratio sigma_1^2/sigma_2^2
#' @param p Delta/sigma_1 in the paper
#' @param min_m blah
#' @return A list of optimal r and m.
#' @export
#'
#' @examples
#' optim_2(10^5, 1, min_m = 20, p = 0.05)

optim_2 <- function(time_ratio, #average burnin time / average time per effective sample
                    var_ratio, #sigma_1^2/sigma_2^2
                    p, #p = Delta/sigma_1 in the paper
                    min_m = 1){

  m_optim <- max(min_m,
                 sqrt(time_ratio / var_ratio))

  m_optim <- ceiling(m_optim)

  r_optim <- 1/p^2 * (1 + 1/var_ratio/m_optim)

  return(list(r = ceiling(r_optim), m = m_optim))
}
