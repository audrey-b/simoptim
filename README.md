
<!-- README.md is generated from README.Rmd. Please edit that file. -->

# simoptim

`simoptim` is an R package for optimal allocation of sampling effort in
Bayesian simulation studies with MCMC. Given estimates of two variance
components — between-dataset variance and within-dataset posterior
variance — it computes the optimal number of simulation datasets `r` and
effective posterior samples per dataset `m` that minimise total
computational cost subject to a precision target.

> **Note:** This package is under development.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("audrey-b/simoptim")
```

## Background

In a Bayesian simulation study, the precision of the estimator is
affected by two sources of Monte Carlo error:

- **Between-dataset variance** ($\sigma_1^2 / r$): variability across
  the `r` simulated datasets.
- **Within-dataset variance** ($\sigma_2^2 / (rm)$): variability due to
  running MCMC for only `m` effective samples per dataset.

`simoptim` implements the methodology of Beliveau et al. (in
preparation) to choose `r` and `m` optimally given a precision target
$p = \Delta /
\sigma_1$ and the ratio of burn-in time to time per effective sample.

## Usage

The typical workflow has four steps.

### Step 1: Run a pilot study

Run MCMC on a small number of pilot datasets and collect the results as
a list of `mcmc.list` objects. Here we simulate pilot data for
illustration.

``` r
library(simoptim)
library(coda)
set.seed(1234)

simulate_mcmc_list <- function(n_iter = 500, n_burnin = 100, n_chains = 3) {
  # Draw the estimator thetahat ~ N(0, 1)
  thetahat <- rnorm(1, mean = 0, sd = 1)
  
  # Posterior is N(thetahat, 1), same variance
  chains <- lapply(1:n_chains, function(i) {
    coda::mcmc(
      matrix(rnorm(n_iter, mean = thetahat, sd = 1), ncol = 1,
             dimnames = list(NULL, "theta")),
      start = n_burnin + 1
    )
  })
  coda::mcmc.list(chains)
}

analyses_results <- lapply(1:8, function(i) simulate_mcmc_list())
```

### Step 2: Summarise posterior samples

``` r
summaries <- lapply(analyses_results, summarize_mcmc, parameter = "theta")
means  <- sapply(summaries, `[[`, "mean")
mcmcses <- sapply(summaries, `[[`, "mcmcse")
vars   <- sapply(summaries, `[[`, "var")
```

### Step 3: Estimate variance components

``` r
var_ests <- estimate_variances(means, mcmcses, vars)
var_ests
#> $var_between
#> [1] 1.125587
#> 
#> $var_within
#> [1] 0.9791907
```

### Step 4: Compute optimal allocation

``` r
optimize_allocation(burnin_time = 10,
      eff_sample_time = 1,
      var_between = var_ests$var_between,
      var_within = var_ests$var_within,
      min_m = 20,
      p = 0.05)
#> $r
#> [1] 418
#> 
#> $m
#> [1] 20
```

If only ratios are available, `optimize_allocation_from_ratios()` can be
used instead and returns the same result:

``` r
variance_ratio <- var_ests$var_between / var_ests$var_within
optimize_allocation_from_ratios(time_ratio = 10,
        variance_ratio  = variance_ratio,
        min_m      = 20,
        p          = 0.05)
#> $r
#> [1] 418
#> 
#> $m
#> [1] 20
```

See `vignette("simoptim")` for a full worked example.

## Citation

Beliveau A., Stringer, A. and Tan C. (in preparation). Optimal
allocation of sampling effort in Bayesian simulation studies with MCMC.

## License

MIT
