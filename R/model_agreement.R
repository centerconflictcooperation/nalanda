# ---- Internal helpers --------------------------------------------------------

#' Compute ICC(2,1) -- two-way random, single measures, absolute agreement
#'
#' @param mat Numeric matrix: rows = targets, columns = raters.
#' @return A list with `icc`, `n_targets`, `n_raters`, and mean squares.
#' @noRd
.compute_icc <- function(mat) {
  n <- nrow(mat)
  k <- ncol(mat)

  if (k < 2 || n < 2) {
    return(list(icc = NA_real_, n_targets = n, n_raters = k))
  }

  grand_mean <- mean(mat, na.rm = TRUE)
  row_means  <- rowMeans(mat, na.rm = TRUE)
  col_means  <- colMeans(mat, na.rm = TRUE)

  ss_row   <- k * sum((row_means - grand_mean)^2)
  ss_col   <- n * sum((col_means - grand_mean)^2)
  ss_total <- sum((mat - grand_mean)^2, na.rm = TRUE)
  ss_error <- ss_total - ss_row - ss_col

  ms_row   <- ss_row   / (n - 1)
  ms_col   <- ss_col   / (k - 1)
  ms_error <- ss_error / ((n - 1) * (k - 1))

  # ICC(2,1): absolute agreement, two-way random
  icc_val <- (ms_row - ms_error) /
    (ms_row + (k - 1) * ms_error + (k / n) * (ms_col - ms_error))

  # F-test for rows (targets differ)
  f_val   <- ms_row / ms_error
  df1     <- n - 1L
  df2     <- (n - 1L) * (k - 1L)
  p_value <- stats::pf(f_val, df1, df2, lower.tail = FALSE)

  list(icc = icc_val, n_targets = n, n_raters = k,
       ms_row = ms_row, ms_col = ms_col, ms_error = ms_error,
       f = f_val, p_value = p_value)
}

#' Compute Kendall's coefficient of concordance (W)
#'
#' @param mat Numeric matrix: rows = items, columns = raters.
#' @param rank_data If TRUE, rank values within each rater first.
#' @return A list with `w`, `chi_sq`, `df`, `p_value`, `n_items`, `n_raters`.
#' @noRd
.compute_kendall_w <- function(mat, rank_data = TRUE) {
  n <- nrow(mat)
  k <- ncol(mat)

  if (k < 2 || n < 2) {
    return(list(w = NA_real_, chi_sq = NA_real_, df = NA_integer_,
                p_value = NA_real_, n_items = n, n_raters = k))
  }

  if (rank_data) {
    mat <- apply(mat, 2, rank, na.last = "keep")
  }

  rank_sums     <- rowSums(mat, na.rm = TRUE)
  mean_rank_sum <- mean(rank_sums)
  s_val         <- sum((rank_sums - mean_rank_sum)^2)

  w       <- 12 * s_val / (k^2 * (n^3 - n))
  chi_sq  <- k * (n - 1) * w
  df      <- n - 1L
  p_value <- stats::pchisq(chi_sq, df, lower.tail = FALSE)

  list(w = w, chi_sq = chi_sq, df = df, p_value = p_value,
       n_items = n, n_raters = k)
}

#' Interpret ICC values (Cicchetti 1994 guidelines)
#' @noRd
.interpret_icc <- function(icc) {
  dplyr::case_when(
    is.na(icc)  ~ "insufficient data",
    icc < 0.40  ~ "poor",
    icc < 0.60  ~ "fair",
    icc < 0.75  ~ "good",
    TRUE        ~ "excellent"
  )
}

#' Interpret Kendall's W
#' @noRd
.interpret_w <- function(w) {
  dplyr::case_when(
    is.na(w) ~ "insufficient data",
    w < 0.30 ~ "weak",
    w < 0.50 ~ "fair",
    w < 0.70 ~ "moderate",
    TRUE     ~ "strong"
  )
}

#' Reshape long data to a rater matrix (rows = units, cols = models)
#' @noRd
.to_rating_matrix <- function(data, outcome, unit_by, model_col) {
  duplicate_keys <- c(unit_by, model_col)
  duplicates <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(duplicate_keys))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::filter(.data$n > 1L)

  if (nrow(duplicates) > 0) {
    stop(
      "Each combination of `unit_by` and `model_col` must identify one row. ",
      "Found duplicate rows for: ", paste(duplicate_keys, collapse = ", "), ". ",
      "Include additional unit columns in `unit_by` or aggregate the data first ",
      "(for example, summarize chapters to one score per book before ranking books).",
      call. = FALSE
    )
  }

  data <- data |>
    tidyr::unite("..unit_id..", dplyr::all_of(unit_by), sep = "|", remove = FALSE)

  wide <- data |>
    dplyr::select(dplyr::all_of(c("..unit_id..", model_col, outcome))) |>
    tidyr::pivot_wider(names_from = dplyr::all_of(model_col),
                       values_from = dplyr::all_of(outcome))

  mat <- as.matrix(wide[, -1, drop = FALSE])
  rownames(mat) <- wide[["..unit_id.."]]
  mat
}


# ---- Exported: aggregate_simulations ----------------------------------------

#' Aggregate simulation runs
#'
#' Computes the mean and standard deviation of an outcome across simulation
#' replicates within each model-by-unit cell. This is the recommended first step
#' before computing inter-model agreement: it collapses intra-model sampling
#' noise so that downstream metrics reflect genuine model differences rather
#' than Monte Carlo variance.
#'
#' @param data A data frame with one row per simulation run, containing columns
#'   for model identity, unit identifiers, and the outcome variable.
#' @param outcome Character string naming the outcome column to aggregate
#'   (default `"outcome"`).
#' @param by Character vector of column names to group by. Must include a column
#'   identifying the model (typically `"model"`). Default
#'   `c("model", "book_id", "chapter_id", "group")`.
#'
#' @return A tibble with one row per unique combination of `by`, plus:
#' \describe{
#'   \item{mean_\{outcome\}}{Mean of `outcome` across simulation runs.}
#'   \item{sd_\{outcome\}}{Standard deviation across runs.}
#'   \item{n_sims}{Number of simulation replicates in the cell.}
#' }
#'
#' @export
#' @examples
#' sim_data <- data.frame(
#'   model = rep(c("gpt-4o", "gemini-2.5-flash"), each = 40),
#'   book_id = rep("BookA", 80),
#'   chapter_id = rep(paste0("ch", 1:4), each = 10, times = 2),
#'   group = rep(c("Democrat", "Republican"), 40),
#'   sim = rep(1:10, 8),
#'   rating = rnorm(80, 60, 10)
#' )
#' aggregate_simulations(sim_data, outcome = "rating",
#'   by = c("model", "book_id", "chapter_id", "group"))
aggregate_simulations <- function(data,
                                  outcome = "outcome",
                                  by = c("model", "book_id", "chapter_id", "group")) {
  stopifnot(is.data.frame(data))
  missing_cols <- setdiff(c(by, outcome), names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  mean_nm <- paste0("mean_", outcome)
  sd_nm   <- paste0("sd_", outcome)

  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarize(!!mean_nm := mean(.data[[outcome]], na.rm = TRUE),
                     !!sd_nm   := stats::sd(.data[[outcome]], na.rm = TRUE),
                     n_sims = dplyr::n(), .groups = "drop")
}


# ---- Exported: model_agreement ----------------------------------------------

#' Compute inter-model agreement
#'
#' Quantifies how consistently different AI models score the same units using
#' ICC(2,1) (intraclass correlation, absolute agreement) and/or Kendall's W
#' (coefficient of concordance). Models produce continuous scores on a 1--100
#' scale; this function operates on those raw scores (typically after
#' aggregating simulation runs via [aggregate_simulations()]).
#'
#' Each model is treated as a **rater** and each unique combination of
#' `unit_by` columns as a **target**. ICC captures agreement in both level and
#' rank order; Kendall's W converts the continuous scores to ranks internally
#' and assesses rank-order concordance only.
#'
#' @param data A data frame with one row per model-by-unit combination.
#' @param outcome Character string naming the score column (default
#'   `"mean_outcome"`).
#' @param unit_by Character vector of columns that jointly identify a unit
#'   (default `c("book_id", "chapter_id", "group")`).
#' @param group_by Optional character vector. If provided, agreement metrics
#'   are computed separately within each level of these columns (e.g.,
#'   `"group"` to get separate estimates for Democrats and Republicans).
#' @param model_col Character string naming the model column (default
#'   `"model"`).
#' @param metrics Character vector of metrics to compute. One or both of
#'   `"icc"` and `"kendall_w"` (default both).
#'
#' @return A tibble with columns: any `group_by` columns, plus
#' \describe{
#'   \item{metric}{`"icc"` or `"kendall_w"`.}
#'   \item{value}{The agreement statistic (0--1 scale).}
#'   \item{interpretation}{Qualitative label (e.g., "good", "moderate").}
#'   \item{n_models}{Number of models (raters).}
#'   \item{n_units}{Number of units (targets).}
#'   \item{p_value}{p-value for the statistic (F-test for ICC, chi-squared
#'     approximation for Kendall's W).}
#' }
#'
#' @details
#' ## Which metric to report?
#'
#' * **ICC(2,1)** is the primary recommendation for continuous scores. It
#'   penalises models that systematically differ in level **and** in rank
#'   ordering. Interpret with Cicchetti (1994) cut-offs: < .40 poor,
#'   .40--.59 fair, .60--.74 good, >= .75 excellent.
#' * **Kendall's W** converts the continuous 1--100 scores to ranks and asks
#'   only whether models rank the units the same way. Useful when the
#'   absolute scale is arbitrary or when the researcher cares about ordinal
#'   agreement (e.g., "which book scored highest?") rather than exact score
#'   match.
#'
#' For a quick "single consistency score," report ICC. Add Kendall's W as a
#' supplementary rank-agreement check.
#'
#' ## Aggregation guidance
#'
#' Always aggregate simulation runs first via [aggregate_simulations()].
#' Failing to do so inflates _n_ and distorts agreement estimates.
#'
#' @export
#' @examples
#' \dontrun{
#' # After aggregating simulations
#' agg <- aggregate_simulations(sim_data, outcome = "rating",
#'   by = c("model", "book_id", "chapter_id", "group"))
#'
#' # Overall agreement
#' model_agreement(agg, outcome = "mean_rating",
#'   unit_by = c("book_id", "chapter_id", "group"))
#'
#' # Agreement by political group
#' model_agreement(agg, outcome = "mean_rating",
#'   unit_by = c("book_id", "chapter_id"),
#'   group_by = "group")
#' }
model_agreement <- function(data,
                            outcome = "mean_outcome",
                            unit_by = c("book_id", "chapter_id", "group"),
                            group_by = NULL,
                            model_col = "model",
                            metrics = c("icc", "kendall_w")) {
  stopifnot(is.data.frame(data))
  metrics <- match.arg(metrics, c("icc", "kendall_w"), several.ok = TRUE)

  required <- c(model_col, outcome, unit_by, group_by)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  .compute_one <- function(df) {
    mat <- .to_rating_matrix(df, outcome, unit_by, model_col)
    rows <- list()

    if ("icc" %in% metrics) {
      res <- .compute_icc(mat)
      rows <- c(rows, list(tibble::tibble(
        metric = "icc", value = res$icc,
        interpretation = .interpret_icc(res$icc),
        n_models = res$n_raters, n_units = res$n_targets,
        p_value = res$p_value
      )))
    }

    if ("kendall_w" %in% metrics) {
      res <- .compute_kendall_w(mat, rank_data = TRUE)
      rows <- c(rows, list(tibble::tibble(
        metric = "kendall_w", value = res$w,
        interpretation = .interpret_w(res$w),
        n_models = res$n_raters, n_units = res$n_items,
        p_value = res$p_value
      )))
    }

    dplyr::bind_rows(rows)
  }

  if (is.null(group_by) || length(group_by) == 0) {
    return(.compute_one(data))
  }

  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by))) |>
    dplyr::group_modify(~ .compute_one(.x)) |>
    dplyr::ungroup()
}


# ---- Exported: model_pairwise_cor -------------------------------------------

#' Pairwise model correlations
#'
#' Computes Pearson and/or Spearman correlations between every pair of models
#' on a shared set of units. This is a diagnostic complement to the omnibus
#' metrics in [model_agreement()]: it reveals *which* models diverge.
#'
#' @inheritParams model_agreement
#' @param methods Character vector of correlation types. One or both of
#'   `"pearson"` and `"spearman"` (default both).
#'
#' @return A tibble with columns: any `group_by` columns, plus `model_a`,
#'   `model_b`, `method`, `correlation`, and `n_units`.
#'
#' @export
#' @examples
#' \dontrun{
#' pw <- model_pairwise_cor(agg, outcome = "mean_rating",
#'   unit_by = c("book_id", "chapter_id", "group"))
#' plot_model_agreement(pw, type = "heatmap")
#' }
model_pairwise_cor <- function(data,
                               outcome = "mean_outcome",
                               unit_by = c("book_id", "chapter_id", "group"),
                               group_by = NULL,
                               model_col = "model",
                               methods = c("pearson", "spearman")) {
  stopifnot(is.data.frame(data))
  methods <- match.arg(methods, c("pearson", "spearman"), several.ok = TRUE)

  required <- c(model_col, outcome, unit_by, group_by)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  .compute_one <- function(df) {
    mat    <- .to_rating_matrix(df, outcome, unit_by, model_col)
    models <- colnames(mat)

    if (length(models) < 2) {
      return(tibble::tibble(
        model_a = character(), model_b = character(),
        method = character(), correlation = double(), n_units = integer()
      ))
    }

    pairs <- utils::combn(models, 2, simplify = FALSE)

    purrr::map_dfr(pairs, function(p) {
      x <- mat[, p[1]]
      y <- mat[, p[2]]
      ok <- stats::complete.cases(x, y)
      n  <- sum(ok)

      purrr::map_dfr(methods, function(m) {
        r <- if (n >= 3) stats::cor(x[ok], y[ok], method = m) else NA_real_
        tibble::tibble(model_a = p[1], model_b = p[2],
                       method = m, correlation = r, n_units = n)
      })
    })
  }

  if (is.null(group_by) || length(group_by) == 0) {
    return(.compute_one(data))
  }

  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by))) |>
    dplyr::group_modify(~ .compute_one(.x)) |>
    dplyr::ungroup()
}


# ---- Exported: model_rank_consistency ---------------------------------------

#' Compare model-derived rankings
#'
#' Takes each model's continuous scores (1--100 scale) and derives rankings
#' from them, then evaluates cross-model concordance via Kendall's W. The
#' rankings are computed by the researcher from the raw scores — models
#' themselves only produce continuous ratings, not ordinal ranks. Useful for
#' answering "Do models rank books the same way?"
#'
#' `unit_by` must identify exactly one row per model within each ranking context.
#' If the data still contain lower-level rows (for example, chapters) and you
#' want book-level ranks, aggregate those rows to the book level before calling
#' this function.
#'
#' @inheritParams model_agreement
#' @param rank_within Optional character vector of columns that define separate
#'   ranking contexts (e.g., `"group"`). Items are ranked independently within
#'   each combination of these columns.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{ranks}{Tibble with the unit columns, `model`, `score`, and `rank`.}
#'   \item{concordance}{Tibble with Kendall's W and associated statistics,
#'     one row per `rank_within` combination (or one row total).}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' rc <- model_rank_consistency(agg, outcome = "mean_rating",
#'   unit_by = c("book_id", "chapter_id"),
#'   rank_within = "group")
#' rc$concordance
#' rc$ranks
#'
#' agg_book <- agg |>
#'   dplyr::group_by(model, book_id, group) |>
#'   dplyr::summarise(mean_rating = mean(mean_rating), .groups = "drop")
#'
#' rc_book <- model_rank_consistency(agg_book, outcome = "mean_rating",
#'   unit_by = "book_id",
#'   rank_within = "group")
#' }
model_rank_consistency <- function(data,
                                   outcome = "mean_outcome",
                                   unit_by = c("book_id", "chapter_id"),
                                   rank_within = NULL,
                                   model_col = "model") {
  stopifnot(is.data.frame(data))

  required <- c(model_col, outcome, unit_by, rank_within)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  rank_groups <- c(rank_within, model_col)

  # Rank items within each model (and within rank_within groups)
  ranked <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(rank_groups))) |>
    dplyr::mutate(rank = rank(-.data[[outcome]], ties.method = "average")) |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::all_of(c(rank_within, unit_by, model_col, outcome, "rank"))) |>
    dplyr::rename(score = dplyr::all_of(outcome))

  # Concordance via Kendall's W
  .concordance_one <- function(df) {
    mat <- .to_rating_matrix(
      dplyr::rename(df, ..outcome.. = "score"),
      outcome = "..outcome..", unit_by = unit_by, model_col = model_col
    )
    res <- .compute_kendall_w(mat, rank_data = TRUE)
    tibble::tibble(
      kendall_w = res$w, chi_sq = res$chi_sq, df = res$df,
      p_value = res$p_value, n_models = res$n_raters, n_items = res$n_items,
      interpretation = .interpret_w(res$w)
    )
  }

  if (is.null(rank_within) || length(rank_within) == 0) {
    concordance <- .concordance_one(ranked)
  } else {
    concordance <- ranked |>
      dplyr::group_by(dplyr::across(dplyr::all_of(rank_within))) |>
      dplyr::group_modify(~ .concordance_one(.x)) |>
      dplyr::ungroup()
  }

  list(ranks = ranked, concordance = concordance)
}


# ---- Exported: plot_model_agreement -----------------------------------------

#' Plot inter-model agreement
#'
#' Creates diagnostic visualizations for model agreement or pairwise
#' correlation results.
#'
#' @param data Output of [model_agreement()] (for `type = "metrics"`) or
#'   [model_pairwise_cor()] (for `type = "heatmap"`).
#' @param type Character. `"metrics"` for a dot plot of agreement statistics;
#'   `"heatmap"` for a pairwise correlation tile plot.
#'
#' @return A ggplot2 object.
#'
#' @export
#' @examples
#' \dontrun{
#' plot_model_agreement(model_agreement(agg, outcome = "mean_rating"),
#'   type = "metrics")
#' plot_model_agreement(model_pairwise_cor(agg, outcome = "mean_rating"),
#'   type = "heatmap")
#' }
plot_model_agreement <- function(data, type = c("metrics", "heatmap")) {
  type <- match.arg(type)
  if (type == "metrics") .plot_agreement_metrics(data) else .plot_agreement_heatmap(data)
}

#' @noRd
.plot_agreement_metrics <- function(data) {
  known <- c("metric", "value", "interpretation", "n_models", "n_units", "p_value")
  group_cols <- setdiff(names(data), known)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[["metric"]], y = .data[["value"]])) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_text(ggplot2::aes(label = round(.data[["value"]], 2)),
                       vjust = -1, size = 3.5) +
    ggplot2::ylim(0, 1) +
    ggplot2::labs(x = "Metric", y = "Agreement",
                  title = "Inter-model agreement") +
    ggplot2::theme_minimal()

  if (length(group_cols) > 0) {
    p <- p + ggplot2::facet_wrap(
      stats::as.formula(paste("~", paste(group_cols, collapse = " + ")))
    )
  }
  p
}

#' @noRd
.plot_agreement_heatmap <- function(data) {
  known <- c("model_a", "model_b", "method", "correlation", "n_units")

  if ("spearman" %in% data[["method"]]) {
    plot_data <- dplyr::filter(data, .data[["method"]] == "spearman")
    subtitle  <- "Spearman rank correlation"
  } else {
    plot_data <- dplyr::filter(data, .data[["method"]] == "pearson")
    subtitle  <- "Pearson correlation"
  }

  group_cols <- setdiff(names(data), known)

  p <- ggplot2::ggplot(plot_data,
    ggplot2::aes(x = .data[["model_a"]], y = .data[["model_b"]],
                 fill = .data[["correlation"]])) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = round(.data[["correlation"]], 2)),
                       size = 3.5) +
    ggplot2::scale_fill_gradient2(
      low = "#d73027", mid = "#ffffbf", high = "#1a9850",
      midpoint = 0.5, limits = c(0, 1), name = "Correlation") +
    ggplot2::labs(x = NULL, y = NULL,
                  title = "Pairwise model agreement", subtitle = subtitle) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  if (length(group_cols) > 0) {
    p <- p + ggplot2::facet_wrap(
      stats::as.formula(paste("~", paste(group_cols, collapse = " + ")))
    )
  }
  p
}
