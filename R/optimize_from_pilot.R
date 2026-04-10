#' Estimate optimal allocation and precision from a pilot study
#'
#' Uses a bootstrap procedure to estimate the optimal number of simulation
#' datasets \eqn{r} and effective posterior samples per dataset \eqn{m},
#' along with their uncertainty, from a pilot study.
#'
#' @param summaries blah
#' @param burnin_times A numeric vector of burn-in times, one per pilot dataset
#'   (in any consistent time unit, e.g. seconds).
#' @param eff_sample_times A numeric vector of times per effective posterior
#'   sample, one per pilot dataset (same units as \code{burnin_times}).
#' @param delta The maximum acceptable total MCMCSE (i.e. the precision
#'   target). Exactly one of \code{delta} or \code{p} must be specified.
#' @param p Standardised precision target \eqn{p = \Delta / \sigma_1}, where
#'   \eqn{\Delta} is the maximum acceptable total MCMCSE.
#' @param min_m Minimum allowable number of effective posterior samples per
#'   dataset. Must be a positive integer. Defaults to \code{1}.
#' @param n_cores A positive integer giving the number of cores available for
#'   parallel execution. Defaults to \code{1} (no parallelisation).
#' @param n_boot A positive integer giving the number of bootstrap replicates.
#'   Defaults to \code{1000}.
#' @param conf A numeric value in (0, 1) giving the confidence level for the
#'   bootstrap intervals. Defaults to \code{0.95}.
#'
#' @return A named list with three elements:
#'   \describe{
#'     \item{estimates}{A named numeric vector of point estimates (posterior
#'       means across bootstrap replicates) for \code{var_between},
#'       \code{var_within}, \code{r}, \code{m}, \code{burnin_time},
#'       \code{eff_sample_time}, and \code{time_total}.}
#'     \item{se}{A named numeric vector of bootstrap standard errors for each
#'       quantity.}
#'     \item{intervals}{A matrix with two rows (\code{lower} and \code{upper})
#'       and one column per quantity, giving the bootstrap confidence intervals
#'       at the level specified by \code{conf}.}
#'   }
#'
#' @importFrom stats quantile sd
#'
#' @seealso \code{\link{estimate_variances}},
#'   \code{\link{optimize_allocation}}, \code{\link{compute_time}}
#' @export
#'
#' @examples
#' \dontrun{
#' optimize_from_pilot(summaries,
#'                     burnin_times, eff_sample_times,
#'                     min_m = 20, p = 0.05)
#' }

optimize_from_pilot <- function(summaries,
                                burnin_times,
                                eff_sample_times,
                                delta,
                                p,
                                min_m   = 1,
                                n_cores = 1,
                                n_boot  = 1000,
                                conf    = 0.95) {
  if (!is.list(summaries) || length(summaries) < 2)
    stop("'summaries' must be a list of at least 2 summarize_mcmc outputs.")
  if (!all(sapply(summaries, function(s) all(c("mean", "mcmcse", "var") %in% names(s)))))
    stop("Each element of 'summaries' must be a named list with 'mean', 'mcmcse' and 'var'.")
  if (!is.numeric(conf) || conf <= 0 || conf >= 1)
    stop("'conf' must be a number strictly between 0 and 1.")
  if (is.null(p) && is.null(delta))
    stop("One of 'delta' or 'p' must be specified.")
  if (!is.null(delta) && (!is.numeric(delta) || delta <= 0))
    stop("'delta' must be a positive number.")
  if (!is.null(p) && (!is.numeric(p) || p <= 0 || p >= 1))
    stop("'p' must be a number strictly between 0 and 1.")
  if (!is.numeric(n_boot) || n_boot < 1 || !isTRUE(all.equal(n_boot, as.integer(n_boot))))
    stop("'n_boot' must be a positive integer.")
  if (length(summaries) != length(burnin_times) ||
      length(summaries) != length(eff_sample_times))
    stop("'summaries', 'burnin_times' and 'eff_sample_times' must all have the same length.")

  means            <- sapply(summaries, `[[`, "mean")
  mcmcses          <- sapply(summaries, `[[`, "mcmcse")
  vars             <- sapply(summaries, `[[`, "var")
  n_pilot          <- length(summaries)
  probs            <- c((1 - conf) / 2, 1 - (1 - conf) / 2)

  boot_estimates <- replicate(n_boot, {
    idx <- sample(n_pilot, replace = TRUE)
    ve  <- estimate_variances(means[idx], mcmcses[idx], vars[idx])
    opt <- optimize_allocation(
      burnin_time         = mean(burnin_times[idx]),
      eff_sample_time = mean(eff_sample_times[idx]),
      var_between         = ve$var_between,
      var_within          = ve$var_within,
      min_m               = min_m,
      p                   = p,
      delta               = delta
    )
    time <- compute_time(
      r                   = opt$r,
      m                   = opt$m,
      burnin_time         = mean(burnin_times[idx]),
      eff_sample_time = mean(eff_sample_times[idx]),
      n_cores             = n_cores
    )
    c(var_between         = ve$var_between,
      var_within          = ve$var_within,
      r                   = opt$r,
      m                   = opt$m,
      burnin_time         = mean(burnin_times[idx]),
      eff_sample_time = mean(eff_sample_times[idx]),
      time_total          = time)
  }, simplify = TRUE)

  list(
    estimates = apply(boot_estimates, 1, mean),
    se        = apply(boot_estimates, 1, sd),
    intervals = apply(boot_estimates, 1, quantile, probs = probs) |>
      `rownames<-`(c("lower", "upper"))
  )
}
