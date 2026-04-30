# Tests for model agreement functions (R/model_agreement.R)

# ---- Shared test fixture ----------------------------------------------------

make_test_data <- function(n_sims = 5, seed = 42) {
  set.seed(seed)
  models   <- c("gpt-4o", "gemini-2.5-flash", "claude-3.5")
  books    <- c("BookA", "BookB")
  chapters <- paste0("ch", 1:3)
  groups   <- c("Democrat", "Republican")

  grid <- expand.grid(
    model = models, book_id = books, chapter_id = chapters,
    group = groups, sim = seq_len(n_sims),
    stringsAsFactors = FALSE
  )
  # Create ratings with moderate model agreement: shared book/chapter signal
  # plus model-specific noise
  grid$book_effect    <- ifelse(grid$book_id == "BookA", 5, -3)
  grid$chapter_effect <- as.numeric(factor(grid$chapter_id)) * 2
  grid$model_offset   <- dplyr::case_when(
    grid$model == "gpt-4o"           ~ 0,
    grid$model == "gemini-2.5-flash" ~ 3,
    grid$model == "claude-3.5"       ~ -2
  )
  grid$rating <- 60 + grid$book_effect + grid$chapter_effect +
    grid$model_offset + rnorm(nrow(grid), 0, 4)
  grid$book_effect <- NULL
  grid$chapter_effect <- NULL
  grid$model_offset <- NULL
  tibble::as_tibble(grid)
}


# ---- aggregate_simulations --------------------------------------------------

test_that("aggregate_simulations returns correct structure", {
  raw <- make_test_data()
  agg <- aggregate_simulations(raw, outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))

  expect_s3_class(agg, "tbl_df")
  expect_true(all(c("mean_rating", "sd_rating", "n_sims") %in% names(agg)))
  # Should have 3 models * 2 books * 3 chapters * 2 groups = 36 rows

  expect_equal(nrow(agg), 3 * 2 * 3 * 2)
  expect_true(all(agg$n_sims == 5))
})

test_that("aggregate_simulations errors on missing columns", {
  raw <- make_test_data()
  expect_error(
    aggregate_simulations(raw, outcome = "nonexistent"),
    "Missing columns"
  )
})


# ---- model_agreement --------------------------------------------------------

test_that("model_agreement computes ICC and Kendall's W", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))

  result <- model_agreement(agg, outcome = "mean_rating",
    unit_by = c("book_id", "chapter_id", "group"))

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_setequal(result$metric, c("icc", "kendall_w"))
  expect_true(all(result$n_models == 3))
  expect_true(all(result$n_units == 12))  # 2 books * 3 chapters * 2 groups
  expect_true(all(result$value >= 0 & result$value <= 1, na.rm = TRUE))
})

test_that("model_agreement with group_by splits correctly", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))

  result <- model_agreement(agg, outcome = "mean_rating",
    unit_by = c("book_id", "chapter_id"),
    group_by = "group")

  expect_true("group" %in% names(result))
  expect_equal(nrow(result), 4)  # 2 groups * 2 metrics
})

test_that("model_agreement with single metric works", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))

  result <- model_agreement(agg, outcome = "mean_rating",
    unit_by = c("book_id", "chapter_id", "group"), metrics = "icc")

  expect_equal(nrow(result), 1)
  expect_equal(result$metric, "icc")
})

test_that("model_agreement errors on missing columns", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))

  expect_error(
    model_agreement(agg, outcome = "bad_col"),
    "Missing columns"
  )
})


# ---- model_pairwise_cor -----------------------------------------------------

test_that("model_pairwise_cor returns all pairs", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))

  pw <- model_pairwise_cor(agg, outcome = "mean_rating",
    unit_by = c("book_id", "chapter_id", "group"))

  # 3 models => 3 pairs * 2 methods = 6 rows
  expect_equal(nrow(pw), 6)
  expect_true(all(c("model_a", "model_b", "method", "correlation", "n_units") %in% names(pw)))
  expect_true(all(abs(pw$correlation) <= 1, na.rm = TRUE))
})

test_that("model_pairwise_cor with group_by works", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))

  pw <- model_pairwise_cor(agg, outcome = "mean_rating",
    unit_by = c("book_id", "chapter_id"),
    group_by = "group")

  expect_true("group" %in% names(pw))
  # 2 groups * 3 pairs * 2 methods = 12
  expect_equal(nrow(pw), 12)
})


# ---- model_rank_consistency -------------------------------------------------

test_that("model_rank_consistency returns ranks and concordance", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))

  rc <- model_rank_consistency(agg, outcome = "mean_rating",
    unit_by = c("book_id", "chapter_id"),
    rank_within = "group")

  expect_type(rc, "list")
  expect_named(rc, c("ranks", "concordance"))
  expect_s3_class(rc$ranks, "tbl_df")
  expect_s3_class(rc$concordance, "tbl_df")
  expect_true("rank" %in% names(rc$ranks))
  expect_true("kendall_w" %in% names(rc$concordance))
  expect_true("group" %in% names(rc$concordance))
  expect_equal(nrow(rc$concordance), 2)  # 2 groups
})

test_that("model_rank_consistency requires one score per model and unit", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))

  expect_error(
    model_rank_consistency(agg, outcome = "mean_rating",
      unit_by = "book_id", rank_within = "group"),
    "aggregate the data first"
  )

  agg_book <- agg |>
    dplyr::group_by(model, book_id, group) |>
    dplyr::summarise(mean_rating = mean(mean_rating), .groups = "drop")

  rc <- model_rank_consistency(agg_book, outcome = "mean_rating",
    unit_by = "book_id", rank_within = "group")

  expect_s3_class(rc$ranks, "tbl_df")
  expect_equal(nrow(rc$ranks), 3 * 2 * 2)
  expect_equal(nrow(rc$concordance), 2)
})


# ---- plot_model_agreement ---------------------------------------------------

test_that("plot_model_agreement returns ggplot for both types", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))

  agreement <- model_agreement(agg, outcome = "mean_rating",
    unit_by = c("book_id", "chapter_id", "group"))
  p1 <- plot_model_agreement(agreement, type = "metrics")
  expect_s3_class(p1, "ggplot")

  pw <- model_pairwise_cor(agg, outcome = "mean_rating",
    unit_by = c("book_id", "chapter_id", "group"))
  p2 <- plot_model_agreement(pw, type = "heatmap")
  expect_s3_class(p2, "ggplot")
})


# ---- Internal: ICC and Kendall's W edge cases -------------------------------

test_that("ICC returns NA for fewer than 2 raters or targets", {
  mat1 <- matrix(1:5, ncol = 1)
  expect_true(is.na(.compute_icc(mat1)$icc))

  mat2 <- matrix(1:3, nrow = 1)
  expect_true(is.na(.compute_icc(mat2)$icc))
})

test_that("ICC of perfectly agreeing raters is near 1", {
  mat <- matrix(rep(1:10, 3), ncol = 3)
  icc <- .compute_icc(mat)$icc
  expect_true(icc > 0.99)
})

test_that("Kendall's W of identical rankings is 1", {
  mat <- matrix(rep(1:5, 4), ncol = 4)
  w <- .compute_kendall_w(mat, rank_data = FALSE)$w
  expect_equal(w, 1)
})
