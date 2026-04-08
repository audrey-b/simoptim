#' mcmcse_total
#'
#' A function to calculate the total MCMCSE.
#'
#' @param var_1 sigma_1^2 in the paper
#' @param var_2 sigma_2^2 in the paper
#' @param r blah
#' @param m blah
#' @return A list of optimal r and m.
#' @export

mcmcse_total <- function(var_1, #sigma_1^2 in the paper
                         var_2, #sigma_2^2 in the paper
                         r,
                         m){

  return(sqrt(var_1/r + var_2/r/m))
}
