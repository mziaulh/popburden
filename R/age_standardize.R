#' Age-standardize a rate, with an interval that respects small counts
#'
#' Computes a directly age-standardized rate: age-specific rates are
#' calculated as `count / population` within each stratum, then combined
#' as a weighted average using external standard-population weights. A
#' normal-approximation confidence interval is available for large counts;
#' for rare conditions where a handful of strata drive the estimate, use
#' `method = "bootstrap"` for a Poisson parametric-bootstrap interval, which
#' does not rely on a normal approximation that can behave poorly when
#' stratum counts are small.
#'
#' @param count Numeric vector of stratum-specific event counts (e.g. cases).
#' @param population Numeric vector of stratum-specific population
#'   denominators, same length as `count`.
#' @param weight Numeric vector of standard-population weights, same length
#'   as `count`. Only the relative magnitude of `weight` matters; it is
#'   renormalized internally, so weights that sum to 1, 100, or 100,000 all
#'   give the same standardized rate. Supply your own reference population
#'   (e.g. a published WHO or national standard) here; none is bundled with
#'   this package so that a stale or mistyped table is never silently used.
#' @param method `"normal"` for a delta-method normal-approximation interval
#'   (fast, appropriate when stratum counts are not tiny), or `"bootstrap"`
#'   for a Poisson parametric-bootstrap interval (slower, more defensible
#'   when some strata have very few events).
#' @param conf_level Confidence level. Default 0.95.
#' @param n_sim Number of bootstrap replicates when `method = "bootstrap"`.
#'
#' @return A list of class `popburden_standardized` with the standardized
#'   rate, its standard error, confidence interval, the method used, and a
#'   data frame of the stratum-level inputs and crude rates.
#'
#' @examples
#' strata <- data.frame(
#'   age_band   = c("0-14", "15-44", "45-64", "65+"),
#'   count      = c(2, 9, 41, 88),
#'   population = c(120000, 210000, 95000, 48000)
#' )
#' # Equal weights here for illustration only; substitute a real reference
#' # population for a real analysis.
#' age_standardize(
#'   count = strata$count, population = strata$population,
#'   weight = c(0.25, 0.25, 0.25, 0.25), method = "bootstrap"
#' )
#'
#' @export
age_standardize <- function(count, population, weight,
                             method = c("normal", "bootstrap"),
                             conf_level = 0.95, n_sim = 5000) {
  method <- match.arg(method)
  n <- length(count)
  if (length(population) != n || length(weight) != n) {
    stop("`count`, `population`, and `weight` must be the same length.", call. = FALSE)
  }
  if (any(population <= 0)) stop("`population` must be strictly positive.", call. = FALSE)
  if (any(weight < 0) || sum(weight) == 0) stop("`weight` must be non-negative and not all zero.", call. = FALSE)
  if (any(count < 0)) stop("`count` must be non-negative.", call. = FALSE)

  w <- weight / sum(weight)
  crude_rate <- count / population
  std_rate <- sum(w * crude_rate)

  z <- stats::qnorm(1 - (1 - conf_level) / 2)

  if (method == "normal") {
    # Delta method, treating each stratum count as Poisson(count).
    var_stratum <- count / population^2
    var_std <- sum(w^2 * var_stratum)
    se_std <- sqrt(var_std)
    ci_low <- max(0, std_rate - z * se_std)
    ci_high <- std_rate + z * se_std
  } else {
    sims <- replicate(n_sim, {
      sim_count <- stats::rpois(n, lambda = count)
      sum(w * sim_count / population)
    })
    se_std <- stats::sd(sims)
    ci_low <- unname(stats::quantile(sims, (1 - conf_level) / 2))
    ci_high <- unname(stats::quantile(sims, 1 - (1 - conf_level) / 2))
  }

  out <- list(
    estimate = std_rate,
    se = se_std,
    ci_low = ci_low,
    ci_high = ci_high,
    conf_level = conf_level,
    method = method,
    strata = data.frame(
      count = count, population = population,
      weight = w, crude_rate = crude_rate,
      stringsAsFactors = FALSE
    )
  )
  class(out) <- "popburden_standardized"
  out
}

#' @export
print.popburden_standardized <- function(x, digits = 6, ...) {
  cat(sprintf(
    "<popburden_standardized> (%s CI, %d strata)\n  standardized rate: %s [%d%% CI: %s, %s]\n\n",
    x$method, nrow(x$strata),
    formatC(x$estimate, digits = digits, format = "f"),
    round(x$conf_level * 100),
    formatC(x$ci_low, digits = digits, format = "f"),
    formatC(x$ci_high, digits = digits, format = "f")
  ))
  print(x$strata, row.names = FALSE, digits = 4)
  invisible(x)
}
