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

test_that("aggregate_simulations returns NA for all-missing cells", {
  raw <- make_test_data()
  raw$rating[raw$model == "gpt-4o" & raw$book_id == "BookA" &
    raw$chapter_id == "ch1" & raw$group == "Democrat"] <- NA_real_

  agg <- aggregate_simulations(raw, outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))

  missing_cell <- agg |>
    dplyr::filter(model == "gpt-4o", book_id == "BookA",
      chapter_id == "ch1", group == "Democrat")

  expect_true(is.na(missing_cell$mean_rating))
  expect_false(is.nan(missing_cell$mean_rating))
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

test_that("model_agreement drops incomplete units for ICC and Kendall's W", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))
  agg$mean_rating[agg$model == "gpt-4o" & agg$book_id == "BookA" &
    agg$chapter_id == "ch1" & agg$group == "Democrat"] <- NA_real_

  result <- model_agreement(agg, outcome = "mean_rating",
    unit_by = c("book_id", "chapter_id", "group"))

  expect_false(any(is.nan(result$value)))
  expect_true(all(result$n_units == 11))
  expect_true(all(result$n_models == 3))
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


# ---- model_agreement_sensitivity --------------------------------------------

test_that("model_agreement_sensitivity returns slide-ready default levels", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))

  result <- model_agreement_sensitivity(agg, outcome = "mean_rating")

  expect_s3_class(result, "tbl_df")
  expect_equal(
    result[["Analysis level"]],
    c("Book + chapter + party", "Book + chapter", "Book + chapter",
      "Book + party", "Book", "Book")
  )
  expect_equal(result[["Subgroup"]], c("Overall", "Democrat", "Republican",
    "Overall", "Democrat", "Republican"))
  expect_true(all(c("ICC", "Kendall's W") %in% names(result)))
})

test_that("model_agreement_sensitivity aggregates with NA-safe means", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))
  agg$mean_rating[agg$model == "gpt-4o" & agg$book_id == "BookA" &
    agg$chapter_id == "ch1" & agg$group == "Democrat"] <- NA_real_

  result <- model_agreement_sensitivity(
    agg,
    outcome = "mean_rating",
    analyses = list("Book + party" = list(unit_by = c("book_id", "group"))),
    format = "long"
  )

  expect_equal(nrow(result), 2)
  expect_true(all(result$n_units == 4))
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


# ---- pairwise_for_level ------------------------------------------------------

test_that("pairwise_for_level aggregates before pairwise correlations", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))

  expected_data <- agg |>
    dplyr::group_by(model, book_id) |>
    dplyr::summarise(mean_rating = mean(mean_rating), .groups = "drop")

  expected <- model_pairwise_cor(
    expected_data,
    outcome = "mean_rating",
    unit_by = "book_id",
    methods = "pearson"
  )

  result <- pairwise_for_level(
    agg,
    outcome = "mean_rating",
    unit_by = "book_id",
    methods = "pearson"
  )

  expect_equal(result, expected)
})

test_that("pairwise_for_level supports grouped pairwise correlations", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))

  result <- pairwise_for_level(
    agg,
    outcome = "mean_rating",
    unit_by = "book_id",
    group_by = "group",
    methods = "spearman"
  )

  expect_true("group" %in% names(result))
  expect_equal(nrow(result), 6)
  expect_equal(unique(result$method), "spearman")
})

test_that("pairwise_for_level uses NA-safe aggregation", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))
  agg$mean_rating[agg$model == "gpt-4o" & agg$book_id == "BookA"] <- NA_real_

  result <- pairwise_for_level(
    agg,
    outcome = "mean_rating",
    unit_by = "book_id",
    methods = "pearson"
  )

  expect_false(any(is.nan(result$correlation)))
})


# ---- summarize_model_correlations -------------------------------------------

test_that("summarize_model_correlations condenses pairwise results", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))

  pw <- model_pairwise_cor(agg, outcome = "mean_rating",
    unit_by = c("book_id", "chapter_id", "group"))

  result <- summarize_model_correlations(pw, method = "pearson")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$method, "pearson")
  expect_equal(result$n_pairs, 3)
  expect_true(result$most_aligned_model %in% unique(c(pw$model_a, pw$model_b)))
  expect_match(result$label, "Mean pairwise r =")
  expect_match(result$label, "Most aligned:")
})

test_that("summarize_model_correlations preserves group columns", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))

  pw <- model_pairwise_cor(agg, outcome = "mean_rating",
    unit_by = c("book_id", "chapter_id"), group_by = "group",
    methods = "spearman")

  result <- summarize_model_correlations(pw)

  expect_true("group" %in% names(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$n_pairs, c(3, 3))
})

test_that("summarize_model_correlations rejects unavailable methods", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))

  pw <- model_pairwise_cor(agg, outcome = "mean_rating",
    unit_by = c("book_id", "chapter_id", "group"), methods = "spearman")

  expect_error(
    summarize_model_correlations(pw, method = "pearson"),
    "`method = \"pearson\"` is not available"
  )
})


# ---- summarize_top_units -----------------------------------------------------

test_that("summarize_top_units ranks items within groups across models", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))

  result <- summarize_top_units(
    agg,
    outcome = "mean_rating",
    item_by = "book_id",
    rank_within = "group",
    top_n = 1
  )

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("group", "book_id", "mean_score", "mean_rank",
    "median_rank", "top_n_models", "n_models", "top_n_label") %in% names(result)))
  expect_equal(nrow(result), 4)
  expect_equal(unique(result$n_models), 3)
  expect_true(all(result$top_n_models >= 0 & result$top_n_models <= 3))
})

test_that("summarize_top_units can return model-level ranks", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))

  result <- summarize_top_units(
    agg,
    outcome = "mean_rating",
    item_by = "book_id",
    rank_within = "group",
    include_ranks = TRUE
  )

  expect_type(result, "list")
  expect_named(result, c("summary", "ranks"))
  expect_s3_class(result$summary, "tbl_df")
  expect_s3_class(result$ranks, "tbl_df")
  expect_true("rank" %in% names(result$ranks))
})

test_that("summarize_top_units supports lower-is-better rankings", {
  x <- tibble::tibble(
    model = rep(c("m1", "m2"), each = 2),
    book_id = rep(c("BookA", "BookB"), times = 2),
    score = c(1, 5, 2, 6)
  )

  high <- summarize_top_units(
    x,
    outcome = "score",
    item_by = "book_id",
    top_n = 1,
    higher_is_better = TRUE
  )
  low <- summarize_top_units(
    x,
    outcome = "score",
    item_by = "book_id",
    top_n = 1,
    higher_is_better = FALSE
  )

  expect_equal(high$book_id[1], "BookB")
  expect_equal(low$book_id[1], "BookA")
})


# ---- top-unit plots ----------------------------------------------------------

test_that("plot_top_units returns a ggplot", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))

  top_units <- summarize_top_units(
    agg,
    outcome = "mean_rating",
    item_by = "book_id",
    rank_within = "group"
  )

  p <- plot_top_units(
    top_units,
    item_col = "book_id",
    facet_by = "group",
    top_n_items = 1
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$scales$get_scales("size")$name, "Mean evaluation\nscore")
  expect_null(p$scales$get_scales("fill"))
  expect_equal(p$labels$x, "Mean rank (lower = better)")
  expect_match(p$labels$caption, "top 3")
  built <- ggplot2::ggplot_build(p)
  expect_false(any(grepl("/", built$data[[2]]$label)))
})

test_that("plot_top_unit_heatmap returns a ggplot", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))

  top_units <- summarize_top_units(
    agg,
    outcome = "mean_rating",
    item_by = "book_id",
    rank_within = "group",
    include_ranks = TRUE
  )

  p <- plot_top_unit_heatmap(
    top_units$ranks,
    item_col = "book_id",
    facet_by = "group",
    top_n_items = 1
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_top_unit_pairs returns a ggplot for two subgroup levels", {
  agg <- aggregate_simulations(make_test_data(), outcome = "rating",
    by = c("model", "book_id", "chapter_id", "group"))

  top_units <- summarize_top_units(
    agg,
    outcome = "mean_rating",
    item_by = "book_id",
    rank_within = "group"
  )

  p <- plot_top_unit_pairs(
    top_units,
    item_col = "book_id",
    subgroup_col = "group",
    top_n_items = 2
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "Mean rank (lower = better)")
})

test_that("plot_top_unit_pairs uses package party colors", {
  expect_equal(
    nalanda:::top_unit_pair_palette(c("Democrat", "Republican")),
    c("Democrat" = "#00AEF3", "Republican" = "#E81B23")
  )
  expect_equal(
    nalanda:::top_unit_pair_palette(c("Republican", "Democrat")),
    c("Republican" = "#E81B23", "Democrat" = "#00AEF3")
  )
})

test_that("plot_top_unit_pairs requires exactly two subgroup levels", {
  x <- tibble::tibble(
    book_id = rep("BookA", 3),
    group = c("a", "b", "c"),
    mean_rank = c(1, 2, 3)
  )

  expect_error(
    plot_top_unit_pairs(x, item_col = "book_id", subgroup_col = "group"),
    "exactly two"
  )
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

  p3 <- plot_model_agreement(pw, type = "heatmap", method = "pearson")
  expect_s3_class(p3, "ggplot")
  expect_equal(p3$labels$subtitle, "Pearson correlation of continuous scores")

  pw_spearman <- model_pairwise_cor(agg, outcome = "mean_rating",
    unit_by = c("book_id", "chapter_id", "group"), methods = "spearman")
  expect_error(
    plot_model_agreement(pw_spearman, type = "heatmap", method = "pearson"),
    "`method = \"pearson\"` is not available"
  )
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
