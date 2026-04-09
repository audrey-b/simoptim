#' Summarize posterior samples for a parameter from an MCMC object
#'
#' Extracts posterior samples for a single parameter from an \code{mcmc.list}
#' object, optionally applies a transformation, and returns the posterior mean,
#' Monte Carlo standard error (MCMCSE), and posterior variance.
#'
#' @param mcmc_object An \code{mcmc.list} object containing posterior samples
#'   (e.g. as returned by JAGS or similar MCMC software).
#' @param parameter A character string giving the name of the parameter to
#'   summarize. Must match a column name in \code{mcmc_object}.
#' @param g A function to apply to the posterior samples before summarizing.
#'   Defaults to \code{identity} (no transformation). Use this to summarize
#'   derived quantities (e.g. \code{g = log} or \code{g = function(x) x^2}).
#'
#' @return A named list with three numeric elements:
#'   \describe{
#'     \item{mean}{Posterior mean of \code{g(parameter)}.}
#'     \item{mcmcse}{Monte Carlo standard error of the posterior mean of \code{g(parameter)}.}
#'     \item{var}{Posterior variance of \code{g(parameter)}.}
#'   }
#'
#' @seealso \code{\link{estimate_variances}}, \code{\link{optim_2}}
#' @export

summarize_mcmc <- function(mcmc_object,
                           parameter,
                           g = identity){


  transform_mcmc_list <- function(mc_list, g) {
    coda::mcmc.list(lapply(mc_list, function(chain) {
      coda::mcmc(apply(as.matrix(chain), 2, g),
                 start = stats::start(chain),
                 thin = coda::thin(chain))
    }))
  }

  transformed <- transform_mcmc_list(mcmc_object[, parameter], g)

  summaries <- summary(transformed, quantiles = NULL)[[1]]

  average <- summaries["Mean"]

  variance <- summaries["SD"] ^ 2 #check if that works

  mcmcse <- summaries["Time-series SE"]

  #ess <- effectiveSize(do.call(g, mcmc_object[, parameter]))

  return(list(mean = as.numeric(average),
              mcmcse = as.numeric(mcmcse),
              var = as.numeric(variance)))
}
