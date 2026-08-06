test_that("a uniform crude rate standardizes to itself regardless of weights", {
  pop <- c(100000, 200000, 50000, 30000)
  rate <- 0.02
  counts <- pop * rate
  weights <- c(10, 20, 5, 65) # deliberately unequal, unnormalized

  out <- age_standardize(counts, pop, weights, method = "normal")
  expect_equal(out$estimate, rate, tolerance = 1e-8)
})

test_that("weight is renormalized internally (scale invariance)", {
  pop <- c(100000, 50000)
  counts <- c(500, 300)
  w1 <- c(1, 3)
  w2 <- c(1000, 3000)

  out1 <- age_standardize(counts, pop, w1, method = "normal")
  out2 <- age_standardize(counts, pop, w2, method = "normal")
  expect_equal(out1$estimate, out2$estimate)
})

test_that("normal-approximation CI brackets the point estimate", {
  out <- age_standardize(c(5, 40, 80), c(50000, 90000, 40000), c(0.2, 0.5, 0.3))
  expect_lte(out$ci_low, out$estimate)
  expect_gte(out$ci_high, out$estimate)
})

test_that("bootstrap CI is centered near the point estimate", {
  set.seed(1)
  out <- age_standardize(
    c(5, 40, 80), c(50000, 90000, 40000), c(0.2, 0.5, 0.3),
    method = "bootstrap", n_sim = 4000
  )
  expect_equal(out$estimate, (0.2 * 5 / 50000 + 0.5 * 40 / 90000 + 0.3 * 80 / 40000))
  expect_lt(abs(mean(c(out$ci_low, out$ci_high)) - out$estimate) / out$estimate, 0.15)
})

test_that("bad inputs error clearly", {
  expect_error(age_standardize(c(1, 2), c(100), c(0.5, 0.5)), "same length")
  expect_error(age_standardize(c(1, 2), c(100, -5), c(0.5, 0.5)), "strictly positive")
  expect_error(age_standardize(c(1, 2), c(100, 100), c(-1, 0.5)), "non-negative")
  expect_error(age_standardize(c(-1, 2), c(100, 100), c(0.5, 0.5)), "non-negative")
})

test_that("print method runs without error", {
  out <- age_standardize(c(5, 40), c(50000, 90000), c(0.5, 0.5))
  expect_output(print(out), "popburden_standardized")
})
