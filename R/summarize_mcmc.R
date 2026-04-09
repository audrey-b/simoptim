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
#' @param transform A function to apply to the posterior samples before summarizing.
#'   Defaults to \code{identity} (no transformation). Use this to summarize
#'   derived quantities (e.g. \code{transform = log} or \code{transform = function(x) x^2}).
#'
#' @return A named list with three numeric elements:
#'   \describe{
#'     \item{mean}{Posterior mean of \code{transform(parameter)}.}
#'     \item{mcmcse}{Monte Carlo standard error of the posterior mean of \code{transform(parameter)}.}
#'     \item{var}{Posterior variance of \code{transform(parameter)}.}
#'   }
#'
#' @seealso \code{\link{estimate_variances}}, \code{\link{optimize_allocation_from_ratios}}
#' @export

summarize_mcmc <- function(mcmc_object,
                           parameter,
                           transform = identity){

  if (!coda::is.mcmc.list(mcmc_object))
    stop("'mcmc_object' must be an mcmc.list object.")
  if (!is.character(parameter) || length(parameter) != 1)
    stop("'parameter' must be a single character string.")
  if (!(parameter %in% colnames(mcmc_object[[1]])))
    stop("'parameter' not found in 'mcmc_object'. ",
         "Available parameters: ", paste(colnames(mcmc_object[[1]]), collapse = ", "))
  if (!is.function(transform))
    stop("'transform' must be a function.")

  transform_mcmc_list <- function(mc_list, transform) {
    coda::mcmc.list(lapply(mc_list, function(chain) {
      coda::mcmc(apply(as.matrix(chain), 2, transform),
                 start = stats::start(chain),
                 thin = coda::thin(chain))
    }))
  }

  transformed <- transform_mcmc_list(mcmc_object[, parameter], transform)

  summaries <- summary(transformed, quantiles = NULL)[[1]]

  average <- summaries["Mean"]

  variance <- summaries["SD"] ^ 2 #check if that works

  mcmcse <- summaries["Time-series SE"]

  #ess <- effectiveSize(do.call(g, mcmc_object[, parameter]))

  return(list(mean = as.numeric(average),
              mcmcse = as.numeric(mcmcse),
              var = as.numeric(variance)))
}
