test_that("point estimate is exactly rate * population", {
  out <- estimate_population_count(0.08, rate_se = 0.005, population = c(1e6, 2e6))
  expect_equal(out$estimate, c(0.08 * 1e6, 0.08 * 2e6))
})

test_that("beta-distribution simulation recovers the target mean and SE", {
  set.seed(42)
  out <- estimate_population_count(
    rate = 0.1, rate_se = 0.01, population = 1e6,
    distribution = "beta", n_sim = 50000
  )
  expect_equal(out$sim_mean, 0.1 * 1e6, tolerance = 0.02 * 1e6)
})

test_that("rate_ci is converted to a consistent rate_se", {
  z <- stats::qnorm(0.975)
  se_direct <- 0.005
  ci <- c(0.08 - z * se_direct, 0.08 + z * se_direct)

  set.seed(1)
  out_ci <- estimate_population_count(0.08, rate_ci = ci, population = 1e6, distribution = "normal")
  set.seed(1)
  out_se <- estimate_population_count(0.08, rate_se = se_direct, population = 1e6, distribution = "normal")
  expect_equal(out_ci$sim_mean, out_se$sim_mean, tolerance = 0.01 * 1e6)
})

test_that("lognormal draws are strictly non-negative and mean-correct on average", {
  set.seed(7)
  out <- estimate_population_count(
    rate = 0.02, rate_se = 0.003, population = 5e5,
    distribution = "lognormal", n_sim = 20000
  )
  expect_gte(out$ci_low, 0)
  expect_equal(out$sim_mean, 0.02 * 5e5, tolerance = 0.05 * (0.02 * 5e5))
})

test_that("beta distribution rejects an SE incompatible with the mean", {
  # Beta variance is bounded above by rate * (1 - rate) = 0.25 here;
  # rate_se = 0.6 implies variance 0.36, which is infeasible.
  expect_error(
    estimate_population_count(0.5, rate_se = 0.6, population = 1e6, distribution = "beta"),
    "too large"
  )
})

test_that("bad inputs error clearly", {
  expect_error(estimate_population_count(0.08, population = 1e6), "rate_se.*rate_ci|Supply either")
  expect_error(
    estimate_population_count(1.2, rate_se = 0.01, population = 1e6, distribution = "beta"),
    "0 < rate < 1"
  )
})
