test_that("year 0 always equals the base count", {
  df <- project_forward(base_count = 1000, horizon_years = 3, pop_growth = 0.02)
  expect_equal(df$estimate[df$year_offset == 0], 1000)
})

test_that("point trajectory matches the closed-form compound growth formula", {
  df <- project_forward(
    base_count = 1000, horizon_years = 4,
    pop_growth = 0.01, rate_trend = 0.02
  )
  expected <- 1000 * (1.01 * 1.02)^df$year_offset
  expect_equal(df$estimate, expected)
})

test_that("zero uncertainty collapses the simulated interval onto the point estimate", {
  df <- project_forward(
    base_count = 500, horizon_years = 3,
    pop_growth = 0.015, pop_growth_sd = 0,
    rate_trend = 0, rate_trend_sd = 0
  )
  expect_equal(df$ci_low, df$estimate, tolerance = 1e-9)
  expect_equal(df$ci_high, df$estimate, tolerance = 1e-9)
})

test_that("uncertainty widens the interval as the horizon lengthens", {
  set.seed(3)
  df <- project_forward(
    base_count = 10000, horizon_years = 10,
    pop_growth = 0.01, pop_growth_sd = 0.01,
    rate_trend = 0, rate_trend_sd = 0.02
  )
  width <- df$ci_high - df$ci_low
  expect_true(width[length(width)] > width[2])
})

test_that("horizon_years must be at least 1", {
  expect_error(project_forward(base_count = 100, horizon_years = 0), "at least 1")
})

test_that("negative sd arguments error clearly", {
  expect_error(
    project_forward(base_count = 100, horizon_years = 2, pop_growth_sd = -1),
    "non-negative"
  )
})
