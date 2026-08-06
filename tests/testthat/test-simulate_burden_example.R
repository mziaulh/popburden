test_that("returns the documented shape", {
  d <- simulate_burden_example()
  expect_named(d, c("population", "sources"))
  expect_named(d$population, c("region", "age_band", "population"))
  expect_named(d$sources, c("region", "source", "estimate", "se"))
  expect_equal(nrow(d$population), 6 * 4)
  expect_equal(nrow(d$sources), 6 * 3)
})

test_that("is reproducible given the same seed", {
  d1 <- simulate_burden_example(seed = 123)
  d2 <- simulate_burden_example(seed = 123)
  expect_equal(d1, d2)
})

test_that("different seeds give different data", {
  d1 <- simulate_burden_example(seed = 1)
  d2 <- simulate_burden_example(seed = 2)
  expect_false(isTRUE(all.equal(d1$sources$estimate, d2$sources$estimate)))
})

test_that("values are in plausible physical ranges", {
  d <- simulate_burden_example()
  expect_true(all(d$population$population > 0))
  expect_true(all(d$sources$estimate > 0 & d$sources$estimate < 1))
  expect_true(all(d$sources$se > 0))
})

test_that("does not leak RNG state to the caller", {
  set.seed(999)
  before <- stats::runif(1)
  set.seed(999)
  invisible(simulate_burden_example(seed = 555))
  after <- stats::runif(1)
  expect_equal(before, after)
})
