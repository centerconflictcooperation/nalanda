forecast_raw <- function() {
  tibble::tibble(
    condition_id = c(1, 1, 1, 1, 2, 2),
    family = c("A", "A", "A", "B", "A", "B"),
    model_id = c("a1", "a1", "a2", "b1", "a1", "b1"),
    prompt_id = c("p1", "p2", "p1", "p1", "p1", "p1"),
    completion = 1L,
    effect = c(1, 3, 5, NA, 2, 6),
    confidence = c(2, 4, 6, 8, 4, 10)
  )
}

test_that("tidy_forecast_aggregation keeps stages, multiple outcomes, and counts", {
  aggregated <- aggregate_model_forecasts(
    forecast_raw(), c("effect", "confidence"), "condition_id"
  )
  out <- tidy_forecast_aggregation(aggregated, "condition_id")

  expect_true(all(c("aggregation_level", "condition_id", "family", "model_id",
                    "prompt_id", "outcome", "estimate", "contributor_type",
                    "n_contributors") %in% names(out)))
  expect_setequal(unique(out$outcome), c("effect", "confidence"))
  expect_setequal(unique(out$aggregation_level), c("prompt", "model", "family", "consensus"))
  expect_equal(out$n_contributors[out$aggregation_level == "consensus"], rep(2L, 4))
})

test_that("tidy_forecast_aggregation supports no family hierarchy", {
  aggregated <- aggregate_model_forecasts(
    forecast_raw() |> dplyr::select(-family), "effect", "condition_id", family_col = NULL
  )
  out <- tidy_forecast_aggregation(aggregated, "condition_id", family_col = NULL)

  expect_false("family" %in% names(out))
  expect_false("family" %in% unique(out$aggregation_level))
  expect_equal(out$n_contributors[out$aggregation_level == "consensus"], c(3L, 2L))
})

test_that("tidy_forecast_aggregation preserves a one-model result", {
  raw <- forecast_raw() |>
    dplyr::filter(.data$model_id == "a1", .data$condition_id == 1)
  aggregated <- aggregate_model_forecasts(raw, "effect", "condition_id")
  out <- tidy_forecast_aggregation(aggregated, "condition_id")

  expect_equal(unique(stats::na.omit(out$model_id)), "a1")
  expect_equal(out$n_contributors[out$aggregation_level == "consensus"], 1L)
})

test_that("summarize_forecast_disagreement handles unequal counts and missing values", {
  data <- tibble::tibble(
    condition = c(1, 1, 1, 2), outcome = "effect",
    source = c("a", "b", "c", "a"), estimate = c(1, 3, NA, 10), width = 10
  )
  out <- summarize_forecast_disagreement(
    data, c("condition", "outcome"), "estimate", "source", scale_width_col = "width"
  )
  strict <- summarize_forecast_disagreement(data, c("condition", "outcome"), "estimate", "source", na_rm = FALSE)

  expect_equal(out$n_contributors, c(3L, 1L))
  expect_equal(out$n_nonmissing, c(2L, 1L))
  expect_equal(out$n_missing, c(1L, 0L))
  expect_equal(out$mean, c(2, 10))
  expect_equal(out$range, c(2, 0))
  expect_equal(out$range_per_scale, c(.2, 0))
  expect_true(is.na(strict$mean[[1]]))
  expect_true(is.na(out$sd[[2]]))
})

test_that("compare_forecast_aggregations aligns outcomes and normalizes differences", {
  raw <- forecast_raw()
  median_aggregation <- aggregate_model_forecasts(raw, c("effect", "confidence"), "condition_id", method = "median")
  mean_aggregation <- aggregate_model_forecasts(raw, c("effect", "confidence"), "condition_id", method = "mean")
  widths <- tidyr::expand_grid(condition_id = 1:2, outcome = c("effect", "confidence")) |>
    dplyr::mutate(scale_width = 10)

  out <- compare_forecast_aggregations(
    median_aggregation, mean_aggregation, unit_by = "condition_id",
    outcomes = c("effect", "confidence"), labels = c("median", "mean"), scale_width = widths
  )

  expect_equal(nrow(out), 4L)
  expect_true(all(c("median", "mean", "difference", "absolute_difference",
                    "normalized_difference") %in% names(out)))
  expect_equal(out$normalized_difference, out$difference / 10)

  median_with_width <- median_aggregation
  mean_with_width <- mean_aggregation
  median_with_width$consensus$width <- 10
  mean_with_width$consensus$width <- 10
  column_width <- compare_forecast_aggregations(
    median_with_width, mean_with_width, unit_by = "condition_id",
    outcomes = c("effect", "confidence"), scale_width = "width"
  )
  expect_equal(column_width$normalized_difference, column_width$difference / 10)
})

test_that("compare_forecast_aggregations rejects duplicate or mismatched identities", {
  aggregation <- aggregate_model_forecasts(forecast_raw(), "effect", "condition_id")
  duplicate <- aggregation
  duplicate$consensus <- dplyr::bind_rows(duplicate$consensus, duplicate$consensus[1, ])
  missing <- aggregation
  missing$consensus <- missing$consensus[-1, ]

  expect_error(
    compare_forecast_aggregations(duplicate, aggregation, unit_by = "condition_id", outcomes = "effect"),
    "duplicate"
  )
  expect_error(
    compare_forecast_aggregations(aggregation, missing, unit_by = "condition_id", outcomes = "effect"),
    "same unit"
  )
})

test_that("model agreement helpers consume the tidy model stage", {
  aggregation <- aggregate_model_forecasts(forecast_raw(), "effect", "condition_id")
  model_stage <- tidy_forecast_aggregation(aggregation, "condition_id") |>
    dplyr::filter(.data$aggregation_level == "model", .data$outcome == "effect")

  pairwise <- model_pairwise_cor(model_stage, outcome = "estimate", unit_by = "condition_id", model_col = "model_id")
  expect_equal(nrow(pairwise), 6L)
  expect_s3_class(plot_model_agreement(pairwise, type = "heatmap", method = "pearson"), "ggplot")
})
