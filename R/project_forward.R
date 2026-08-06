#' Project a patient population or burden figure forward in time
#'
#' A lightweight demand-forecasting routine: starting from a current-year
#' patient count, projects it forward assuming the underlying population
#' grows (or shrinks) at `pop_growth` per year and the underlying rate
#' drifts at `rate_trend` per year, with both assumptions allowed to carry
#' their own uncertainty, propagated by Monte Carlo simulation. This is a
#' deliberately simple, transparent projection meant as a starting point for
#' scenario discussion, not a substitute for a full cohort-component
#' demographic model or a claims-based uptake curve.
#'
#' @param base_count Current-year patient count (point estimate).
#' @param horizon_years Integer, how many years forward to project.
#' @param pop_growth Expected annual population growth rate (e.g. `0.01` for
#'   1%/year). Default 0.
#' @param pop_growth_sd Standard deviation on `pop_growth`, representing
#'   uncertainty in the demographic assumption. Default 0 (treated as fixed).
#' @param rate_trend Expected annual trend in the underlying rate (e.g.
#'   `0.02` for a rate rising 2%/year, `-0.01` for one falling 1%/year).
#'   Default 0.
#' @param rate_trend_sd Standard deviation on `rate_trend`. Default 0.
#' @param conf_level Confidence level for the projection interval.
#' @param n_sim Number of Monte Carlo trajectories to simulate.
#'
#' @return A data frame with one row per projected year (`0` = base year):
#'   the year offset, the point-estimate trajectory, and the simulated
#'   mean and confidence interval.
#'
#' @examples
#' project_forward(
#'   base_count = 42000, horizon_years = 5,
#'   pop_growth = 0.008, pop_growth_sd = 0.002,
#'   rate_trend = 0.015, rate_trend_sd = 0.01
#' )
#'
#' @export
project_forward <- function(base_count, horizon_years,
                             pop_growth = 0, pop_growth_sd = 0,
                             rate_trend = 0, rate_trend_sd = 0,
                             conf_level = 0.95, n_sim = 10000) {
  if (horizon_years < 1) stop("`horizon_years` must be at least 1.", call. = FALSE)
  if (pop_growth_sd < 0 || rate_trend_sd < 0) {
    stop("`pop_growth_sd` and `rate_trend_sd` must be non-negative.", call. = FALSE)
  }

  years <- 0:horizon_years
  point <- base_count * (1 + pop_growth)^years * (1 + rate_trend)^years

  g_draws <- if (pop_growth_sd > 0) stats::rnorm(n_sim, pop_growth, pop_growth_sd) else rep(pop_growth, n_sim)
  r_draws <- if (rate_trend_sd > 0) stats::rnorm(n_sim, rate_trend, rate_trend_sd) else rep(rate_trend, n_sim)

  sims <- vapply(years, function(t) {
    base_count * (1 + g_draws)^t * (1 + r_draws)^t
  }, numeric(n_sim))
  sims <- matrix(sims, nrow = n_sim)

  ci <- apply(
    sims, 2, stats::quantile,
    probs = c((1 - conf_level) / 2, 1 - (1 - conf_level) / 2)
  )

  data.frame(
    year_offset = years,
    estimate = point,
    sim_mean = colMeans(sims),
    ci_low = ci[1, ],
    ci_high = ci[2, ]
  )
}
