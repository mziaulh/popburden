#' Simulate an illustrative multi-source, multi-region burden dataset
#'
#' Generates a synthetic dataset in the shape this package is designed to
#' work with: several fictional regions, each with an age-stratified
#' population, and three independent "sources" of prevalence estimates for
#' a fictional chronic condition (a household survey, an administrative
#' registry, and a model-based estimate) with source-appropriate sampling
#' error. **All figures are simulated for demonstration; they are not
#' derived from, and are not intended to resemble, any specific real
#' disease, country, or published estimate.** To use real data, build the
#' same two data frames (see Value) from WHO, IHME GBD, or national survey
#' exports.
#'
#' @param seed Random seed, for reproducibility. Default `20260805`.
#'
#' @return A list with two data frames:
#'   \describe{
#'     \item{population}{One row per region x age band: `region`,
#'       `age_band`, `population`.}
#'     \item{sources}{One row per region x source: `region`, `source`,
#'       `estimate` (a prevalence proportion), `se`.}
#'   }
#'
#' @examples
#' d <- simulate_burden_example()
#' str(d)
#'
#' @export
simulate_burden_example <- function(seed = 20260805) {
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    NULL
  }
  on.exit({
    if (!is.null(old_seed)) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })
  set.seed(seed)

  regions <- paste("Region", LETTERS[1:6])
  age_bands <- c("0-14", "15-44", "45-64", "65+")
  age_share <- c("0-14" = 0.27, "15-44" = 0.42, "45-64" = 0.21, "65+" = 0.10)
  base_pop <- c(1.4e6, 2.1e6, 3.0e5, 4.2e6, 9.5e5, 1.8e6)

  population <- do.call(rbind, lapply(seq_along(regions), function(i) {
    data.frame(
      region = regions[i],
      age_band = age_bands,
      population = round(base_pop[i] * age_share[age_bands] *
        stats::runif(length(age_bands), 0.92, 1.08)),
      stringsAsFactors = FALSE
    )
  }))
  rownames(population) <- NULL

  true_prev <- stats::runif(length(regions), 0.06, 0.12)

  sources <- do.call(rbind, lapply(seq_along(regions), function(i) {
    tp <- true_prev[i]
    data.frame(
      region = regions[i],
      source = c("household_survey", "administrative_registry", "model_based"),
      estimate = pmax(0.001, c(
        tp + stats::rnorm(1, 0, 0.006),
        tp + stats::rnorm(1, 0.012, 0.010), # registries tend to undercount
        tp + stats::rnorm(1, -0.004, 0.004)
      )),
      se = c(0.006, 0.011, 0.004),
      stringsAsFactors = FALSE
    )
  }))
  rownames(sources) <- NULL

  list(population = population, sources = sources)
}
