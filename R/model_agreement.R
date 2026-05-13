# ---- Internal helpers --------------------------------------------------------

#' Compute ICC(2,1) -- two-way random, single measures, absolute agreement
#'
#' @param mat Numeric matrix: rows = targets, columns = raters.
#' @return A list with `icc`, `n_targets`, `n_raters`, and mean squares.
#' @noRd
.compute_icc <- function(mat) {
  mat <- mat[stats::complete.cases(mat), , drop = FALSE]

  n <- nrow(mat)
  k <- ncol(mat)

  if (k < 2 || n < 2) {
    return(list(icc = NA_real_, n_targets = n, n_raters = k))
  }

  grand_mean <- mean(mat)
  row_means  <- rowMeans(mat)
  col_means  <- colMeans(mat)

  ss_row   <- k * sum((row_means - grand_mean)^2)
  ss_col   <- n * sum((col_means - grand_mean)^2)
  ss_total <- sum((mat - grand_mean)^2)
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
  mat <- mat[stats::complete.cases(mat), , drop = FALSE]

  n <- nrow(mat)
  k <- ncol(mat)

  if (k < 2 || n < 2) {
    return(list(w = NA_real_, chi_sq = NA_real_, df = NA_integer_,
                p_value = NA_real_, n_items = n, n_raters = k))
  }

  if (rank_data) {
    mat <- apply(mat, 2, rank)
  }

  rank_sums     <- rowSums(mat)
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

#' Summarise missing values as NA instead of NaN
#' @noRd
.mean_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

standardize_top_unit_scores <- function(data, model_col, standardize = "none") {
  if (identical(standardize, "none")) {
    return(data)
  }

  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(model_col))) |>
    dplyr::mutate(
      score = standardize_top_unit_vector(.data$score, standardize)
    ) |>
    dplyr::ungroup()
}

standardize_top_unit_vector <- function(x, standardize) {
  if (all(is.na(x))) {
    return(x)
  }

  if (identical(standardize, "z")) {
    sigma <- stats::sd(x, na.rm = TRUE)
    if (is.na(sigma) || identical(sigma, 0)) {
      return(ifelse(is.na(x), NA_real_, 0))
    }
    return((x - mean(x, na.rm = TRUE)) / sigma)
  }

  if (identical(standardize, "minmax")) {
    lo <- min(x, na.rm = TRUE)
    hi <- max(x, na.rm = TRUE)
    if (is.na(lo) || is.na(hi) || identical(hi, lo)) {
      return(ifelse(is.na(x), NA_real_, 0))
    }
    return((x - lo) / (hi - lo))
  }

  denom <- max(abs(x), na.rm = TRUE)
  if (is.na(denom) || identical(denom, 0)) {
    return(ifelse(is.na(x), NA_real_, 0))
  }
  x / denom
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
    dplyr::summarize(!!mean_nm := .mean_or_na(.data[[outcome]]),
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
#' Units with missing scores for one or more models are excluded from ICC and
#' Kendall's W because agreement metrics require the same units to be scored by
#' all raters. The reported `n_units` is the number of complete units used in
#' the calculation.
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


# ---- Exported: model_agreement_sensitivity ----------------------------------

#' Summarize model agreement across analysis levels
#'
#' Builds a slide-ready sensitivity table by running [model_agreement()] across
#' several substantively useful unit definitions (for example book-level,
#' chapter-level, and party-specific agreement). Lower-level rows are
#' aggregated with an NA-safe mean before each agreement calculation, so skipped
#' chapters do not turn an entire book mean into `NA`.
#'
#' @inheritParams model_agreement
#' @param analyses Optional named list defining analyses to run. Each element
#'   should be a list with `unit_by` and, optionally, `group_by`. If `NULL`, a
#'   default set is inferred from available book, chapter, and party/group
#'   columns.
#' @param format Character. `"wide"` returns one row per analysis/subgroup with
#'   ICC and Kendall's W side by side; `"long"` returns the stacked
#'   [model_agreement()] results with analysis labels.
#' @param digits Integer. Number of decimal places used in the formatted wide
#'   table.
#' @param drop_missing Logical. Whether to drop rows with missing model, unit,
#'   or grouping identifiers before computing each analysis (default `TRUE`).
#'
#' @return A tibble. With `format = "wide"`, columns include `Analysis level`,
#'   `Subgroup`, `N models`, `N units`, `ICC`, and `Kendall's W`.
#'
#' @export
#' @examples
#' \dontrun{
#' model_agreement_sensitivity(
#'   agg,
#'   outcome = "mean_delta_gap",
#'   model_col = "model"
#' )
#' }
model_agreement_sensitivity <- function(data,
                                        outcome = "mean_outcome",
                                        model_col = "model",
                                        analyses = NULL,
                                        metrics = c("icc", "kendall_w"),
                                        format = c("wide", "long"),
                                        digits = 2,
                                        drop_missing = TRUE) {
  stopifnot(is.data.frame(data))
  format <- match.arg(format)
  metrics <- match.arg(metrics, c("icc", "kendall_w"), several.ok = TRUE)

  required <- c(model_col, outcome)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  if (is.null(analyses)) {
    analyses <- default_agreement_analyses(data)
  }

  if (length(analyses) == 0) {
    stop(
      "No analyses could be inferred. Provide `analyses` with `unit_by` columns.",
      call. = FALSE
    )
  }

  long <- purrr::imap_dfr(analyses, function(spec, label) {
    if (is.null(spec$unit_by)) {
      stop("Each `analyses` element must define `unit_by`.", call. = FALSE)
    }

    unit_by <- spec$unit_by
    group_by <- if (is.null(spec$group_by)) NULL else spec$group_by
    id_cols <- c(model_col, unit_by, group_by)

    missing_cols <- setdiff(c(id_cols, outcome), names(data))
    if (length(missing_cols) > 0) {
      stop(
        "Analysis `", label, "` refers to missing column(s): ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }

    analysis_data <- data
    if (isTRUE(drop_missing)) {
      analysis_data <- analysis_data |>
        dplyr::filter(dplyr::if_all(dplyr::all_of(id_cols), ~ !is.na(.x)))
    }

    analysis_data <- analysis_data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
      dplyr::summarise(
        !!outcome := .mean_or_na(.data[[outcome]]),
        .groups = "drop"
      )

    result <- model_agreement(
      analysis_data,
      outcome = outcome,
      unit_by = unit_by,
      group_by = group_by,
      model_col = model_col,
      metrics = metrics
    )

    result <- result |>
      dplyr::mutate(analysis = label, .before = 1)

    if (is.null(group_by) || length(group_by) == 0) {
      result <- result |>
        dplyr::mutate(subgroup = "Overall", .after = "analysis")
    } else {
      result <- add_agreement_subgroup(result, group_by)
    }

    result |>
      dplyr::select(
        dplyr::all_of(c("analysis", "subgroup")),
        dplyr::everything(),
        -dplyr::any_of(group_by)
      )
  })

  if (format == "long") {
    return(long)
  }

  long |>
    dplyr::mutate(
      metric = dplyr::recode(
        .data$metric,
        icc = "ICC",
        kendall_w = "Kendall's W"
      ),
      value_label = ifelse(
        is.na(.data$value),
        paste0("NA (", .data$interpretation, ")"),
        paste0(round(.data$value, digits), " (", .data$interpretation, ")")
      )
    ) |>
    dplyr::select(
      "analysis", "subgroup", "n_models", "n_units", "metric", "value_label"
    ) |>
    tidyr::pivot_wider(names_from = "metric", values_from = "value_label") |>
    dplyr::rename(
      `Analysis level` = "analysis",
      Subgroup = "subgroup",
      `N models` = "n_models",
      `N units` = "n_units"
    )
}

default_agreement_analyses <- function(data) {
  book_col <- first_present(names(data), c("book", "book_id"))
  chapter_col <- first_present(names(data), c("chapter", "chapter_id"))
  party_col <- first_present(names(data), c("party", "group"))

  analyses <- list()

  if (!is.null(book_col) && !is.null(chapter_col) && !is.null(party_col)) {
    analyses[["Book + chapter + party"]] <- list(
      unit_by = c(book_col, chapter_col, party_col)
    )
    analyses[["Book + chapter"]] <- list(
      unit_by = c(book_col, chapter_col),
      group_by = party_col
    )
  }

  if (!is.null(book_col) && !is.null(party_col)) {
    analyses[["Book + party"]] <- list(unit_by = c(book_col, party_col))
    analyses[["Book"]] <- list(unit_by = book_col, group_by = party_col)
  } else if (!is.null(book_col)) {
    analyses[["Book"]] <- list(unit_by = book_col)
  }

  analyses
}

first_present <- function(x, candidates) {
  present <- candidates[candidates %in% x]
  if (length(present) == 0) NULL else present[[1]]
}

add_agreement_subgroup <- function(result, group_by) {
  if (length(group_by) == 1) {
    return(result |>
      dplyr::mutate(subgroup = as.character(.data[[group_by]]), .after = "analysis"))
  }

  subgroup <- apply(
    as.data.frame(result[, group_by, drop = FALSE]),
    1,
    function(row) {
      paste(paste(group_by, row, sep = "="), collapse = "; ")
    }
  )

  result |>
    dplyr::mutate(subgroup = subgroup, .after = "analysis")
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


# ---- Exported: pairwise_for_level -------------------------------------------

#' Pairwise model correlations at a chosen analysis level
#'
#' Aggregates lower-level rows to the requested unit level, then calls
#' [model_pairwise_cor()]. This is a convenience wrapper for cases where data
#' still contain chapter, party, or simulation-detail rows but the researcher
#' wants correlations at a broader level, such as book-level correlations.
#'
#' @inheritParams model_pairwise_cor
#' @param drop_missing Logical. Whether to drop rows with missing model, unit,
#'   or grouping identifiers before aggregating (default `TRUE`).
#'
#' @return Output of [model_pairwise_cor()] for the requested level.
#'
#' @export
#' @examples
#' \dontrun{
#' # Book-level pairwise correlations from chapter-party-level aggregated data
#' pw_book <- pairwise_for_level(
#'   agg,
#'   outcome = "mean_delta_gap",
#'   unit_by = "book",
#'   model_col = "model",
#'   methods = "pearson"
#' )
#'
#' summarize_model_correlations(pw_book, method = "pearson")
#' }
pairwise_for_level <- function(data,
                               outcome = "mean_outcome",
                               unit_by = c("book_id", "chapter_id", "group"),
                               group_by = NULL,
                               model_col = "model",
                               methods = c("pearson", "spearman"),
                               drop_missing = TRUE) {
  stopifnot(is.data.frame(data))
  methods <- match.arg(methods, c("pearson", "spearman"), several.ok = TRUE)

  required <- c(model_col, outcome, unit_by, group_by)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  id_cols <- unique(c(model_col, unit_by, group_by))
  data_level <- data
  if (isTRUE(drop_missing)) {
    data_level <- data_level |>
      dplyr::filter(dplyr::if_all(dplyr::all_of(id_cols), ~ !is.na(.x)))
  }

  data_level <- data_level |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
    dplyr::summarise(
      !!outcome := .mean_or_na(.data[[outcome]]),
      .groups = "drop"
    )

  model_pairwise_cor(
    data_level,
    outcome = outcome,
    unit_by = unit_by,
    group_by = group_by,
    model_col = model_col,
    methods = methods
  )
}


# ---- Exported: summarize_model_correlations ---------------------------------

#' Summarize pairwise model correlations
#'
#' Condenses the output of [model_pairwise_cor()] into one row per correlation
#' method and subgroup. The summary includes the average pairwise correlation
#' and the "most aligned" model, defined as the model with the highest average
#' correlation with all other models. This is useful for adding a small
#' headline annotation to correlation-matrix slides.
#'
#' @param data Output of [model_pairwise_cor()].
#' @param method Optional character. If supplied, keep only one correlation
#'   method, e.g. `"pearson"` or `"spearman"`.
#' @param digits Integer. Number of decimal places used in the display `label`.
#'
#' @return A tibble with any subgroup columns from `data`, plus `method`,
#'   `mean_correlation`, `median_correlation`, `min_correlation`,
#'   `max_correlation`, `n_pairs`, `most_aligned_model`,
#'   `most_aligned_correlation`, and `label`.
#'
#' @export
#' @examples
#' \dontrun{
#' pw <- model_pairwise_cor(agg, outcome = "mean_rating",
#'   unit_by = c("book_id", "chapter_id", "group"))
#' summarize_model_correlations(pw, method = "pearson")
#' }
summarize_model_correlations <- function(data, method = NULL, digits = 2) {
  stopifnot(is.data.frame(data))

  required <- c("model_a", "model_b", "method", "correlation", "n_units")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "`data` must be output from `model_pairwise_cor()`. Missing columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(method)) {
    method <- match.arg(method, c("pearson", "spearman"))
    if (!(method %in% data$method)) {
      stop(
        "`method = \"", method, "\"` is not available in `data`. ",
        "Compute it first with `model_pairwise_cor(..., methods = \"",
        method, "\")`.",
        call. = FALSE
      )
    }
    method_filter <- method
    data <- dplyr::filter(data, .data$method == method_filter)
  }

  group_cols <- setdiff(names(data), required)
  grouping_cols <- c(group_cols, "method")

  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) |>
    dplyr::group_modify(~ summarize_model_correlation_group(.x, digits = digits)) |>
    dplyr::ungroup()
}

summarize_model_correlation_group <- function(data, digits = 2) {
  pair_values <- as.numeric(data$correlation)
  model_scores <- dplyr::bind_rows(
    tibble::tibble(model = data$model_a, correlation = pair_values),
    tibble::tibble(model = data$model_b, correlation = pair_values)
  ) |>
    dplyr::group_by(.data$model) |>
    dplyr::summarise(
      mean_correlation = .mean_or_na(.data$correlation),
      n_pairs = sum(!is.na(.data$correlation)),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$mean_correlation), .data$model)

  best <- model_scores[1, , drop = FALSE]

  mean_r <- .mean_or_na(pair_values)
  median_r <- if (all(is.na(pair_values))) {
    NA_real_
  } else {
    stats::median(pair_values, na.rm = TRUE)
  }
  min_r <- if (all(is.na(pair_values))) NA_real_ else min(pair_values, na.rm = TRUE)
  max_r <- if (all(is.na(pair_values))) NA_real_ else max(pair_values, na.rm = TRUE)

  tibble::tibble(
    mean_correlation = mean_r,
    median_correlation = median_r,
    min_correlation = min_r,
    max_correlation = max_r,
    n_pairs = sum(!is.na(pair_values)),
    most_aligned_model = best$model,
    most_aligned_correlation = best$mean_correlation,
    label = paste0(
      "Mean pairwise r = ", format_round_or_na(mean_r, digits),
      "\nMost aligned: ", best$model,
      " (mean r = ", format_round_or_na(best$mean_correlation, digits), ")"
    )
  )
}

format_round_or_na <- function(x, digits = 2) {
  if (length(x) == 0 || is.na(x)) {
    return("NA")
  }

  format(round(x, digits), nsmall = digits, trim = TRUE)
}


# ---- Exported: summarize_top_units ------------------------------------------

#' Summarize units that rank consistently high across models
#'
#' Aggregates lower-level rows to a chosen unit level, ranks units within each
#' model, and summarizes which units most consistently appear near the top
#' across models. This is useful for questions such as "Which books
#' consistently have the strongest effects across models?"
#'
#' @inheritParams model_agreement
#' @param item_by Character vector identifying the items to rank, e.g. `"book"`
#'   or `"book_id"`.
#' @param rank_within Optional character vector defining separate ranking
#'   contexts, e.g. `"party"` to rank books separately within party.
#' @param top_n Integer. Number of top-ranked items to count for each model.
#' @param higher_is_better Logical. If `TRUE` (default), larger outcome values
#'   receive better ranks. If `FALSE`, smaller values receive better ranks.
#' @param standardize Character. How to standardize item scores within each
#'   model before computing cross-model mean scores. `"z"` (default) centers
#'   and scales scores within model; `"none"` keeps raw scores; `"minmax"`
#'   rescales scores within model to 0--1; `"max"` divides scores within model
#'   by that model's maximum absolute score. Ranks are unchanged by monotonic
#'   standardization, but `mean_score` and point sizes in [plot_top_units()] use
#'   the standardized scores.
#' @param include_ranks Logical. If `TRUE`, return a list with both the summary
#'   table and the model-level ranks. If `FALSE` (default), return only the
#'   summary table.
#' @param drop_missing Logical. Whether to drop rows with missing model, item,
#'   or ranking-context identifiers before aggregating (default `TRUE`).
#'
#' @return A tibble, or a list with `summary` and `ranks` when
#'   `include_ranks = TRUE`.
#'
#'   The summary table contains:
#'   \describe{
#'     \item{`rank_within` columns}{Optional grouping columns used to define
#'       separate ranking contexts, such as party.}
#'     \item{`item_by` columns}{The ranked item identifiers, such as book.}
#'     \item{`mean_score`}{Mean outcome score for the item across models.}
#'     \item{`score_scale`}{The score standardization method used for
#'       `mean_score`.}
#'     \item{`mean_rank`}{Average rank of the item across models. Lower values
#'       indicate more consistently high-ranked items when
#'       `higher_is_better = TRUE`.}
#'     \item{`overall_mean_rank`}{When `rank_within` is supplied, the item's
#'       average rank computed without those ranking contexts. This preserves a
#'       common item order for subgroup displays.}
#'     \item{`median_rank`}{Median rank of the item across models.}
#'     \item{`top_n_models`}{Number of models that ranked the item within the
#'       top `top_n` items in its ranking context. For example, if
#'       `top_n = 3` and `top_n_models = 4`, then 4 models placed that item in
#'       their top 3.}
#'     \item{`n_models`}{Number of models with non-missing ranks for the item.}
#'     \item{`top_n`}{The top-N threshold used to compute `top_n_models`.}
#'     \item{`top_n_label`}{Compact display label combining `top_n_models` and
#'       `n_models`, such as `"4/5"`.}
#'   }
#'
#'   When `include_ranks = TRUE`, the `ranks` table contains one row per
#'   model-by-item combination, including `score`, `rank`, and `top_n`.
#'
#' @export
#' @examples
#' \dontrun{
#' summarize_top_units(
#'   agg,
#'   outcome = "mean_delta_gap",
#'   item_by = "book",
#'   rank_within = "party",
#'   model_col = "model",
#'   top_n = 3
#' )
#' }
summarize_top_units <- function(data,
                                outcome = "mean_outcome",
                                item_by = "book_id",
                                rank_within = NULL,
                                model_col = "model",
                                top_n = 3,
                                higher_is_better = TRUE,
                                standardize = c("z", "none", "minmax", "max"),
                                include_ranks = FALSE,
                                drop_missing = TRUE) {
  stopifnot(is.data.frame(data))
  standardize <- match.arg(standardize)
  if (!is.numeric(top_n) || length(top_n) != 1 || is.na(top_n) || top_n < 1) {
    stop("`top_n` must be a positive number.", call. = FALSE)
  }
  top_n <- as.integer(top_n)

  required <- c(model_col, outcome, item_by, rank_within)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  id_cols <- unique(c(model_col, item_by, rank_within))
  data_level <- data
  if (isTRUE(drop_missing)) {
    data_level <- data_level |>
      dplyr::filter(dplyr::if_all(dplyr::all_of(id_cols), ~ !is.na(.x)))
  }

  data_level <- data_level |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
    dplyr::summarise(
      score = .mean_or_na(.data[[outcome]]),
      .groups = "drop"
    )
  data_level <- standardize_top_unit_scores(data_level, model_col, standardize)

  overall_summary <- NULL
  if (!is.null(rank_within) && length(rank_within) > 0) {
    overall_level <- data
    if (isTRUE(drop_missing)) {
      overall_level <- overall_level |>
        dplyr::filter(dplyr::if_all(dplyr::all_of(c(model_col, item_by)), ~ !is.na(.x)))
    }

    overall_ranked <- overall_level |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(model_col, item_by)))) |>
      dplyr::summarise(
        score = .mean_or_na(.data[[outcome]]),
        .groups = "drop"
      )
    overall_ranked <- standardize_top_unit_scores(overall_ranked, model_col, standardize) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(model_col))) |>
      dplyr::mutate(
        rank = if (isTRUE(higher_is_better)) {
          rank(-.data$score, ties.method = "average", na.last = "keep")
        } else {
          rank(.data$score, ties.method = "average", na.last = "keep")
        },
        top_n = .data$rank <= top_n
      ) |>
      dplyr::ungroup()

    overall_summary <- overall_ranked |>
      dplyr::group_by(dplyr::across(dplyr::all_of(item_by))) |>
      dplyr::summarise(
        overall_mean_score = .mean_or_na(.data$score),
        overall_mean_rank = .mean_or_na(.data$rank),
        overall_top_n_models = sum(.data$top_n, na.rm = TRUE),
        .groups = "drop"
      )
  }

  rank_groups <- c(rank_within, model_col)
  ranked <- data_level |>
    dplyr::group_by(dplyr::across(dplyr::all_of(rank_groups))) |>
    dplyr::mutate(
      rank = if (isTRUE(higher_is_better)) {
        rank(-.data$score, ties.method = "average", na.last = "keep")
      } else {
        rank(.data$score, ties.method = "average", na.last = "keep")
      },
      top_n = .data$rank <= top_n
    ) |>
    dplyr::ungroup()

  summary_groups <- c(rank_within, item_by)
  summary <- ranked |>
    dplyr::group_by(dplyr::across(dplyr::all_of(summary_groups))) |>
    dplyr::summarise(
      mean_score = .mean_or_na(.data$score),
      mean_rank = .mean_or_na(.data$rank),
      median_rank = if (all(is.na(.data$rank))) {
        NA_real_
      } else {
        stats::median(.data$rank, na.rm = TRUE)
      },
      top_n_models = sum(.data$top_n, na.rm = TRUE),
      n_models = sum(!is.na(.data$rank)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      score_scale = standardize,
      top_n = top_n,
      top_n_label = paste0(.data$top_n_models, "/", .data$n_models)
    )

  if (!is.null(overall_summary)) {
    summary <- summary |>
      dplyr::left_join(overall_summary, by = item_by) |>
      dplyr::arrange(
        .data$overall_mean_rank,
        dplyr::desc(.data$overall_mean_score),
        dplyr::across(dplyr::all_of(rank_within)),
        dplyr::desc(.data$mean_score)
      )
  } else {
    summary <- summary |>
      dplyr::arrange(
        dplyr::across(dplyr::all_of(rank_within)),
        .data$mean_rank,
        dplyr::desc(.data$mean_score)
      )
  }

  if (isTRUE(include_ranks)) {
    return(list(summary = summary, ranks = ranked))
  }

  summary
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
#' Units with missing scores for one or more models are excluded from the
#' concordance calculation. The reported `n_items` is the number of complete
#' items used.
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
#' @param method Character. Correlation method to plot when `type = "heatmap"`:
#'   `"spearman"` for rank correlations or `"pearson"` for linear correlations
#'   on the continuous scores. If `NULL` (default), Spearman is used when
#'   available, otherwise Pearson is used.
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
#' plot_model_agreement(model_pairwise_cor(agg, outcome = "mean_rating"),
#'   type = "heatmap", method = "pearson")
#' }
plot_model_agreement <- function(data, type = c("metrics", "heatmap"), method = NULL) {
  type <- match.arg(type)
  if (!is.null(method)) {
    method <- match.arg(method, c("spearman", "pearson"))
  }

  if (type == "metrics") {
    if (!is.null(method)) {
      warning("`method` is ignored when `type = \"metrics\"`.", call. = FALSE)
    }
    .plot_agreement_metrics(data)
  } else {
    .plot_agreement_heatmap(data, method = method)
  }
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
.plot_agreement_heatmap <- function(data, method = NULL) {
  known <- c("model_a", "model_b", "method", "correlation", "n_units")

  available_methods <- unique(data[["method"]])
  if (is.null(method)) {
    method <- if ("spearman" %in% available_methods) "spearman" else "pearson"
  }

  if (!(method %in% available_methods)) {
    stop(
      "`method = \"", method, "\"` is not available in `data`. ",
      "Compute it first with `model_pairwise_cor(..., methods = \"", method, "\")`.",
      call. = FALSE
    )
  }

  if (identical(method, "spearman")) {
    plot_data <- dplyr::filter(data, .data[["method"]] == "spearman")
    subtitle <- "Spearman rank correlation"
  } else {
    plot_data <- dplyr::filter(data, .data[["method"]] == "pearson")
    subtitle <- "Pearson correlation of continuous scores"
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
