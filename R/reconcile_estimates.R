#' Reconcile independent burden or prevalence estimates into one pooled figure
#'
#' Combines several independent point estimates of the same quantity (a
#' prevalence, an incidence rate, a patient count) into a single pooled
#' estimate, using inverse-variance weighting. This is the same machinery
#' used for fixed- and random-effects meta-analysis, applied here to
#' reconciling estimates that come from different *sources* (survey,
#' registry, model-based) rather than different *studies*.
#'
#' @param estimate Numeric vector of point estimates, one per source.
#' @param se Numeric vector of standard errors, one per source, same length
#'   as `estimate`.
#' @param source Optional character vector naming each source, used to
#'   label output and plots. Defaults to `Source 1`, `Source 2`, ...
#' @param method `"fixed"` for inverse-variance-weighted fixed-effect pooling
#'   (assumes all sources estimate the same true quantity and differ only by
#'   sampling error), or `"random"` for DerSimonian-Laird random-effects
#'   pooling (allows genuine between-source heterogeneity, e.g. differing
#'   case definitions or catchment periods). Default `"random"`, since
#'   independent real-world sources rarely share a single measurement-error
#'   model.
#' @param conf_level Confidence level for the pooled interval. Default 0.95.
#'
#' @return An object of class `popburden_reconciled`: a list with the pooled
#'   estimate, its standard error and confidence interval, per-source
#'   weights, and (for `method = "random"`) the between-source variance
#'   tau-squared, Cochran's Q heterogeneity statistic, and I-squared.
#'
#' @references
#' DerSimonian R, Laird N (1986). "Meta-analysis in clinical trials."
#' Controlled Clinical Trials, 7(3), 177-188.
#'
#' Higgins JPT, Thompson SG (2002). "Quantifying heterogeneity in a
#' meta-analysis." Statistics in Medicine, 21(11), 1539-1558.
#'
#' @examples
#' # Three independent prevalence estimates for the same condition,
#' # e.g. from a household survey, a claims-based registry, and a
#' # model-based (GBD-style) estimate.
#' r <- reconcile_estimates(
#'   estimate = c(0.081, 0.093, 0.077),
#'   se       = c(0.006, 0.011, 0.004),
#'   source   = c("survey", "registry", "model-based")
#' )
#' r
#'
#' @export
reconcile_estimates <- function(estimate, se, source = NULL,
                                 method = c("random", "fixed"),
                                 conf_level = 0.95) {
  method <- match.arg(method)

  if (length(estimate) != length(se)) {
    stop("`estimate` and `se` must be the same length.", call. = FALSE)
  }
  if (length(estimate) < 1) {
    stop("Need at least one source.", call. = FALSE)
  }
  if (any(se <= 0)) {
    stop("`se` must be strictly positive for every source.", call. = FALSE)
  }
  if (is.null(source)) {
    source <- paste("Source", seq_along(estimate))
  }
  if (length(source) != length(estimate)) {
    stop("`source` must be the same length as `estimate`.", call. = FALSE)
  }

  k <- length(estimate)
  w_fixed <- 1 / se^2

  tau2 <- 0
  Q <- NA_real_
  I2 <- NA_real_

  if (k > 1) {
    pooled_fixed <- sum(w_fixed * estimate) / sum(w_fixed)
    Q <- sum(w_fixed * (estimate - pooled_fixed)^2)
    df <- k - 1
    C <- sum(w_fixed) - sum(w_fixed^2) / sum(w_fixed)
    tau2 <- if (C > 0) max(0, (Q - df) / C) else 0
    I2 <- max(0, (Q - df) / Q) * 100
  }

  if (method == "fixed" || k == 1) {
    w <- w_fixed
  } else {
    w <- 1 / (se^2 + tau2)
  }

  pooled <- sum(w * estimate) / sum(w)
  pooled_se <- sqrt(1 / sum(w))
  z <- stats::qnorm(1 - (1 - conf_level) / 2)

  out <- list(
    estimate   = pooled,
    se         = pooled_se,
    conf_level = conf_level,
    ci_low     = pooled - z * pooled_se,
    ci_high    = pooled + z * pooled_se,
    method     = if (k == 1) "single source (no pooling)" else method,
    tau2       = tau2,
    Q          = Q,
    I2         = I2,
    sources    = data.frame(
      source   = source,
      estimate = estimate,
      se       = se,
      weight   = w / sum(w),
      ci_low   = estimate - z * se,
      ci_high  = estimate + z * se,
      stringsAsFactors = FALSE
    )
  )
  class(out) <- "popburden_reconciled"
  out
}

#' @export
print.popburden_reconciled <- function(x, digits = 4, ...) {
  cat(sprintf(
    "<popburden_reconciled> (%s pooling, %d source%s)\n",
    x$method, nrow(x$sources), if (nrow(x$sources) == 1) "" else "s"
  ))
  cat(sprintf(
    "  pooled estimate: %s  [%d%% CI: %s, %s]\n",
    formatC(x$estimate, digits = digits, format = "f"),
    round(x$conf_level * 100),
    formatC(x$ci_low, digits = digits, format = "f"),
    formatC(x$ci_high, digits = digits, format = "f")
  ))
  if (!is.na(x$I2)) {
    cat(sprintf(
      "  between-source heterogeneity: I^2 = %.1f%%, tau^2 = %.6f\n",
      x$I2, x$tau2
    ))
  }
  cat("\n")
  print(x$sources, row.names = FALSE, digits = digits)
  invisible(x)
}
