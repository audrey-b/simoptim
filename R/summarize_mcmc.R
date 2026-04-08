#' summarize_mcmc
#'
#' Requires at least 2 datasets. Quantities to input are after transformation by g. All vectors must be ordered by the same datasets.
#'
#' @param mcmc_object blah
#' @param parameter blah
#' @param g blah
#' @return A list of estimated variances.
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
