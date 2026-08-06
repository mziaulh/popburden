# popburden

<!-- badges: start -->
[![R-CMD-check](https://github.com/mziaulh/popburden/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mziaulh/popburden/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE.md)
<!-- badges: end -->

Reconcile disagreeing epidemiological sources into one decision-ready
patient-population or disease-burden estimate — without losing the
uncertainty on the way.

A survey, a registry, and a model-based estimate will not agree on a
prevalence. `popburden` gives you a small, tested set of tools for turning
that disagreement into a single number a non-epidemiologist can act on,
along with an honest confidence interval:

- **`reconcile_estimates()`** — pool independent source estimates by
  inverse-variance weighting, with DerSimonian-Laird random effects so
  genuine between-source heterogeneity widens the interval instead of
  being averaged away.
- **`age_standardize()`** — direct age standardization against any
  reference population, with a Poisson parametric-bootstrap interval as an
  alternative to the normal approximation when stratum counts are small.
- **`estimate_population_count()`** — convert a rate into an absolute
  patient count via Monte Carlo simulation, sampling from a Beta,
  log-normal, or normal distribution depending on what the rate is.
- **`project_forward()`** — a simple, transparent multi-year projection
  under population-growth and rate-trend assumptions, each with its own
  uncertainty.
- **`plot_reconciled()` / `plot_projection()`** — forest plot and fan chart
  for the two outputs above.

Every function returns a point estimate *and* an interval. None of them
quietly drop the uncertainty to make the final table look tidier.

## Install

```r
# install.packages("remotes")
remotes::install_github("mziaulh/popburden")
```

## Quick example

```r
library(popburden)

# Three independent prevalence estimates for the same population.
pooled <- reconcile_estimates(
  estimate = c(0.081, 0.093, 0.077),
  se       = c(0.006, 0.011, 0.004),
  source   = c("survey", "registry", "model-based")
)
pooled
#> <popburden_reconciled> (random pooling, 3 sources)
#>   pooled estimate: 0.0795  [95% CI: 0.0732, 0.0857]
#>   between-source heterogeneity: I^2 = 0.0%, tau^2 = 0.000000
#>
#>       source estimate    se  weight  ci_low ci_high
#>       survey    0.081 0.006 0.28189 0.06924 0.09276
#>     registry    0.093 0.011 0.08387 0.07144 0.11456
#>  model-based    0.077 0.004 0.63425 0.06916 0.08484

# Turn the pooled rate into a patient count for a population of 2.4M,
# propagating uncertainty via a Beta-distributed Monte Carlo simulation.
estimate_population_count(
  rate = pooled$estimate, rate_se = pooled$se,
  population = 2.4e6, distribution = "beta"
)

# Project the count 5 years forward under a demographic scenario.
project_forward(
  base_count = pooled$estimate * 2.4e6, horizon_years = 5,
  pop_growth = 0.008, pop_growth_sd = 0.002,
  rate_trend = 0.015, rate_trend_sd = 0.01
)
```

See `vignette("burden-estimation-walkthrough")` for the full pipeline —
reconciliation, age standardization, population sizing, and projection —
run end to end on a synthetic six-region dataset.

## On the example data

`simulate_burden_example()` and the walkthrough vignette use **entirely
synthetic** data: six fictional regions, a fictional chronic condition, and
prevalence figures generated from a seeded random-number stream. They are
built to be shaped like real WHO/IHME GBD/national-survey exports, not to
resemble any specific real disease, country, or published estimate. Point
the same functions at real data by matching the column names documented in
`?simulate_burden_example`.

## Why this exists

Reconciling multiple imperfect data sources into a single population or
burden figure — and being explicit about the two things you *haven't* done
(built a commercial forecast, or validated the pipeline against a
biopharma-grade denominator) — is a specific, checkable skill. This package
is that skill written down as code instead of asserted in a cover letter.

## License

MIT © [Muhammad Zia ul Haq](https://mziaulh.github.io)
