#' estimate_variances
#'
#' Requires vectors to be of lenght 2 or more. Quantities to input are after transformation by g. All vectors must be ordered by the same datasets.
#'
#' @param estimates a vector of estimates for each dataset in the simulation
#' @param posterior.variances a vector of posterior variances for each dataset in the simulation
#' @param mcmcses a vector of estimated monte carlo standard errors for each dataset in the simulation
#' @return A list of estimated variances.
#' @export

estimate_variances <- function(estimates,
                               mcmcses,
                               posterior.variances){

  estimate_var2 = mean(posterior.variances)

  estimate_var1 = stats::var(estimates) - mean(mcmcses^2)  #should we guarantee non-negative?

  return(list(estimate_var1 = estimate_var1,
              estimate_var2 = estimate_var2))
}
