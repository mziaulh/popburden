#' Convert a rate into an absolute patient-count estimate, with uncertainty
#'
#' Multiplies a rate (a prevalence proportion, or an incidence/mortality
#' rate) by a population denominator to get an absolute patient count, and
#' propagates the rate's uncertainty through to the count via Monte Carlo
#' simulation. The sampling distribution for the rate can be chosen to match
#' what the rate actually is: `"beta"` for a proportion bounded on \[0, 1\]
#' (recommended for prevalence), `"lognormal"` for a strictly positive rate
#' (recommended for incidence or mortality rates), or `"normal"` as a
#' generic fallback.
#'
#' @param rate Point estimate of the rate (e.g. a prevalence proportion).
#' @param rate_se Standard error of `rate`. Provide this or `rate_ci`.
#' @param rate_ci Optional length-2 vector `c(low, high)` giving a
#'   `conf_level` confidence interval for `rate`; used to back out `rate_se`
#'   if `rate_se` is not supplied directly.
#' @param population Population denominator (a single number, or a vector to
#'   get an estimate per stratum/region).
#' @param distribution `"beta"`, `"lognormal"`, or `"normal"`; see Description.
#' @param conf_level Confidence level for both the input CI (if `rate_ci` is
#'   used) and the output CI. Default 0.95.
#' @param n_sim Number of Monte Carlo draws.
#'
#' @return A data frame with one row per element of `population`: the point
#'   estimate (`rate * population`), and the simulated count's mean and
#'   lower/upper confidence bounds.
#'
#' @examples
#' # A prevalence of 8.5% (95% CI 7.6-9.4%) applied to three regions.
#' estimate_population_count(
#'   rate = 0.085, rate_ci = c(0.076, 0.094),
#'   population = c(1.2e6, 3.4e5, 8.9e5),
#'   distribution = "beta"
#' )
#'
#' @export
estimate_population_count <- function(rate, rate_se = NULL, rate_ci = NULL,
                                       population,
                                       distribution = c("beta", "lognormal", "normal"),
                                       conf_level = 0.95, n_sim = 20000) {
  distribution <- match.arg(distribution)
  if (is.null(rate_se) && is.null(rate_ci)) {
    stop("Supply either `rate_se` or `rate_ci`.", call. = FALSE)
  }
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  if (is.null(rate_se)) {
    if (length(rate_ci) != 2) stop("`rate_ci` must be length 2: c(low, high).", call. = FALSE)
    rate_se <- (rate_ci[2] - rate_ci[1]) / (2 * z)
  }
  if (distribution == "beta" && (rate <= 0 || rate >= 1)) {
    stop("`distribution = \"beta\"` requires 0 < rate < 1.", call. = FALSE)
  }

  draws <- switch(distribution,
    normal = stats::rnorm(n_sim, mean = rate, sd = rate_se),
    lognormal = {
      # Method of moments on the natural scale, mapped to the log scale.
      sigma2 <- log(1 + (rate_se / rate)^2)
      mu <- log(rate) - sigma2 / 2
      stats::rlnorm(n_sim, meanlog = mu, sdlog = sqrt(sigma2))
    },
    beta = {
      v <- rate_se^2
      if (v >= rate * (1 - rate)) {
        stop(
          "`rate_se` is too large relative to `rate` for a valid Beta distribution ",
          "(variance must be < rate * (1 - rate)). Consider `distribution = \"normal\"`.",
          call. = FALSE
        )
      }
      common <- rate * (1 - rate) / v - 1
      a <- rate * common
      b <- (1 - rate) * common
      stats::rbeta(n_sim, a, b)
    }
  )
  draws <- pmax(draws, 0)

  do.call(rbind, lapply(population, function(pop) {
    counts <- draws * pop
    data.frame(
      population = pop,
      rate = rate,
      estimate = rate * pop,
      sim_mean = mean(counts),
      ci_low = unname(stats::quantile(counts, (1 - conf_level) / 2)),
      ci_high = unname(stats::quantile(counts, 1 - (1 - conf_level) / 2))
    )
  }))
}
