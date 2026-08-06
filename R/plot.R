#' @importFrom rlang .data
NULL

#' Forest plot of a reconciled estimate
#'
#' Plots each source's estimate and confidence interval alongside the pooled
#' estimate, in the style of a meta-analysis forest plot. Requires the
#' ggplot2 package.
#'
#' @param x A `popburden_reconciled` object, from [reconcile_estimates()].
#' @param source_col,pooled_col Colours for the individual-source points and
#'   the pooled diamond.
#'
#' @return A ggplot object.
#'
#' @examples
#' r <- reconcile_estimates(
#'   estimate = c(0.081, 0.093, 0.077),
#'   se       = c(0.006, 0.011, 0.004),
#'   source   = c("survey", "registry", "model-based")
#' )
#' if (requireNamespace("ggplot2", quietly = TRUE)) plot_reconciled(r)
#'
#' @export
plot_reconciled <- function(x, source_col = "grey30", pooled_col = "#b3532c") {
  if (!inherits(x, "popburden_reconciled")) {
    stop("`x` must be a `popburden_reconciled` object from reconcile_estimates().", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" is needed for plot_reconciled(). Please install it.", call. = FALSE)
  }

  df <- x$sources
  row_levels <- c("Pooled", rev(df$source))
  df$row <- factor(df$source, levels = row_levels)

  pooled <- data.frame(
    row = factor("Pooled", levels = row_levels),
    estimate = x$estimate, ci_low = x$ci_low, ci_high = x$ci_high
  )

  ggplot2::ggplot(df, ggplot2::aes(x = .data$estimate, y = .data$row)) +
    ggplot2::geom_vline(xintercept = x$estimate, linetype = "dashed", colour = "grey70") +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data$ci_low, xmax = .data$ci_high),
      width = 0.15, orientation = "y", colour = source_col
    ) +
    ggplot2::geom_point(size = 2.5, colour = source_col) +
    ggplot2::geom_errorbar(
      data = pooled,
      ggplot2::aes(xmin = .data$ci_low, xmax = .data$ci_high, y = .data$row),
      width = 0.15, orientation = "y", colour = pooled_col
    ) +
    ggplot2::geom_point(
      data = pooled, ggplot2::aes(x = .data$estimate, y = .data$row),
      shape = 18, size = 4.5, colour = pooled_col
    ) +
    ggplot2::scale_y_discrete(limits = row_levels) +
    ggplot2::labs(
      x = "estimate", y = NULL,
      title = sprintf("Reconciled estimate (%s pooling)", x$method),
      subtitle = if (!is.na(x$I2)) sprintf("I^2 = %.0f%%", x$I2) else NULL
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

#' Fan chart of a forward projection
#'
#' Plots the point-estimate trajectory and confidence band from
#' [project_forward()].
#'
#' @param df A data frame returned by [project_forward()].
#' @param base_year Optional calendar year to label year 0; if supplied, the
#'   x-axis shows calendar years instead of year offsets.
#'
#' @return A ggplot object.
#'
#' @examples
#' p <- project_forward(base_count = 42000, horizon_years = 5,
#'                       pop_growth = 0.008, pop_growth_sd = 0.002,
#'                       rate_trend = 0.015, rate_trend_sd = 0.01)
#' if (requireNamespace("ggplot2", quietly = TRUE)) plot_projection(p, base_year = 2026)
#'
#' @export
plot_projection <- function(df, base_year = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" is needed for plot_projection(). Please install it.", call. = FALSE)
  }
  df$x <- if (is.null(base_year)) df$year_offset else base_year + df$year_offset
  xlab <- if (is.null(base_year)) "years from base" else "year"

  ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$estimate)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$ci_low, ymax = .data$ci_high),
      fill = "#b3532c", alpha = 0.15
    ) +
    ggplot2::geom_line(colour = "#b3532c", linewidth = 1) +
    ggplot2::geom_point(colour = "#b3532c", size = 1.8) +
    ggplot2::labs(x = xlab, y = "projected count", title = "Forward projection") +
    ggplot2::theme_minimal(base_size = 12)
}
