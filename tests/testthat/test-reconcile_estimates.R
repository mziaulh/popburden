test_that("single source passes through unpooled", {
  r <- reconcile_estimates(estimate = 0.08, se = 0.01)
  expect_equal(r$estimate, 0.08)
  expect_equal(r$se, 0.01)
  expect_match(r$method, "single source")
  expect_true(is.na(r$I2))
})

test_that("fixed-effect pooling matches manual inverse-variance calculation", {
  est <- c(0.08, 0.10)
  se <- c(0.01, 0.02)
  w <- 1 / se^2
  expected <- sum(w * est) / sum(w)

  r <- reconcile_estimates(est, se, method = "fixed")
  expect_equal(r$estimate, expected)
  expect_equal(r$se, sqrt(1 / sum(w)))
})

test_that("equal standard errors reduce fixed pooling to a simple mean", {
  est <- c(0.05, 0.09, 0.07)
  se <- rep(0.01, 3)
  r <- reconcile_estimates(est, se, method = "fixed")
  expect_equal(r$estimate, mean(est))
})

test_that("source weights sum to one", {
  r <- reconcile_estimates(c(0.08, 0.10, 0.09), c(0.01, 0.02, 0.015), method = "random")
  expect_equal(sum(r$sources$weight), 1, tolerance = 1e-10)
})

test_that("random-effects tau2 and I2 are non-negative and I2 is a percentage", {
  r <- reconcile_estimates(c(0.06, 0.15, 0.09), c(0.005, 0.005, 0.005), method = "random")
  expect_gte(r$tau2, 0)
  expect_gte(r$I2, 0)
  expect_lte(r$I2, 100)
})

test_that("random-effects widens (or matches) the fixed-effect interval under heterogeneity", {
  est <- c(0.05, 0.20, 0.09) # deliberately discordant sources
  se <- c(0.004, 0.004, 0.004)
  fixed <- reconcile_estimates(est, se, method = "fixed")
  random <- reconcile_estimates(est, se, method = "random")
  expect_gte(random$se, fixed$se - 1e-12)
})

test_that("bad inputs error clearly", {
  expect_error(reconcile_estimates(c(0.1, 0.2), c(0.01)), "same length")
  expect_error(reconcile_estimates(c(0.1, 0.2), c(0.01, 0)), "strictly positive")
  expect_error(reconcile_estimates(numeric(0), numeric(0)), "at least one source")
})

test_that("print method runs without error", {
  r <- reconcile_estimates(c(0.08, 0.10), c(0.01, 0.02))
  expect_output(print(r), "popburden_reconciled")
})
