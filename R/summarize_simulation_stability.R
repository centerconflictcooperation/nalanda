#' Summarize simulation stability across chapters
#'
#' This helper provides a compact bird's-eye view of where repeated simulation
#' runs vary across chapters, books, parties, or models. It reuses the
#' simulation-level SD columns from [summarize_chapter_scores()] and reports how
#' often each metric showed non-zero variation within the requested grouping.
#'
#' If `x` is already output from [summarize_chapter_scores()], the function uses
#' it directly. Otherwise, it first computes chapter-level summaries with
#' `by_party = TRUE`, because party-specific stability is the most common
#' diagnostic use case.
#'
#' Groups with only one simulation row have `NA` SD values from
#' [stats::sd()]. Those groups are treated as not testable for variability and
#' are excluded from the variation counts.
#'
#' @param x A data frame or list-like object. This can be raw simulation metrics
#'   (for example from [compute_run_ai_metrics()]) or chapter summaries from
#'   [summarize_chapter_scores()].
#' @param by Character vector of columns used for the compact summary. Defaults
#'   to `"party"`.
#' @param metrics Optional character vector of metric names to inspect without
#'   the `sd_` prefix. Defaults to the four core ratings:
#'   `pre_ingroup`, `pre_outgroup`, `post_ingroup`, and `post_outgroup`.
#' @param tol Numeric tolerance for treating an SD as zero. Defaults to `0`.
#'
#' @return A tibble with one row per grouping combination, including the number
#'   of assessed units (`n_units`), the proportion of units showing any
#'   pre-period variation, the proportion showing any post-period variation,
#'   and an overall `all_stable` flag.
#'
#' @examples
#' stability <- summarize_simulation_stability(toy_sim_results)
#' stability
#'
#' summarize_simulation_stability(
#'   toy_sim_results,
#'   by = c("model", "party")
#' )
#' @export
summarize_simulation_stability <- function(
  x,
  by = "party",
  metrics = NULL,
  tol = 0
) {
  model <- attr(x, "model")
  models <- attr(x, "models")
  temperature <- attr(x, "temperature")
  n_simulations <- attr(x, "n_simulations")

  if (is.null(model) && is.list(x) && !inherits(x, "data.frame") &&
    length(x) > 0) {
    model <- model %||% attr(x[[1]], "model")
    temperature <- temperature %||% attr(x[[1]], "temperature")
    n_simulations <- n_simulations %||% attr(x[[1]], "n_simulations")
  }
  model <- normalize_model_name(model)
  models <- normalize_model_metadata(models)

  df <- if (is.list(x) && !inherits(x, "data.frame")) {
    flatten_sim_results(x)
  } else {
    x
  }

  df <- tibble::as_tibble(df)

  available_sd <- grep("^sd_", names(df), value = TRUE)
  if (length(available_sd) == 0) {
    df <- summarize_chapter_scores(
      df,
      aggregate_level = "chapter",
      by_party = TRUE
    )
    available_sd <- grep("^sd_", names(df), value = TRUE)
  }

  if (length(available_sd) == 0) {
    stop(
      "`x` must contain chapter-level SD columns or be compatible with ",
      "`summarize_chapter_scores()`."
    )
  }

  available_metrics <- sub("^sd_", "", available_sd)
  if (is.null(metrics)) {
    metrics <- c(
      "pre_ingroup",
      "pre_outgroup",
      "post_ingroup",
      "post_outgroup"
    )
    metrics <- intersect(metrics, available_metrics)
  } else {
    metrics <- intersect(metrics, available_metrics)
  }

  if (length(metrics) == 0) {
    stop("No requested metrics were found in `x`.")
  }

  sd_cols <- paste0("sd_", metrics)
  by_present <- intersect(by, names(df))

  if (!("sim" %in% names(df))) {
    stop("`x` must include a `sim` column.")
  }

  variation_df <- df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(sd_cols),
        ~ !is.na(.) & abs(.) > tol,
        .names = "var_{.col}"
      ),
      any_pre_variation = dplyr::if_any(
        dplyr::all_of(intersect(
          c("var_sd_pre_ingroup", "var_sd_pre_outgroup"),
          paste0("var_", sd_cols)
        )),
        identity
      ),
      any_post_variation = dplyr::if_any(
        dplyr::all_of(intersect(
          c("var_sd_post_ingroup", "var_sd_post_outgroup"),
          paste0("var_", sd_cols)
        )),
        identity
      )
    )

  variation_cols <- paste0("var_", sd_cols)

  out <- variation_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by_present))) |>
    dplyr::summarise(
      n_units = dplyr::n(),
      n_testable_units = sum(.data$sim > 1, na.rm = TRUE),
      min_sims = min(.data$sim, na.rm = TRUE),
      max_sims = max(.data$sim, na.rm = TRUE),
      n_units_any_variation = sum(
        dplyr::if_any(
          dplyr::all_of(variation_cols),
          identity
        )
      ),
      n_units_any_pre_variation = sum(.data$any_pre_variation, na.rm = TRUE),
      n_units_any_post_variation = sum(.data$any_post_variation, na.rm = TRUE),
      .groups = "drop"
    )

  out$prop_units_any_pre_variation <- ifelse(
    out$n_testable_units > 0,
    out$n_units_any_pre_variation / out$n_testable_units,
    NA_real_
  )
  out$prop_units_any_post_variation <- ifelse(
    out$n_testable_units > 0,
    out$n_units_any_post_variation / out$n_testable_units,
    NA_real_
  )
  out$all_stable <- out$n_units_any_variation == 0

  out <- out |>
    dplyr::select(
      dplyr::all_of(by_present),
      .data$n_units,
      .data$prop_units_any_pre_variation,
      .data$prop_units_any_post_variation,
      .data$all_stable
    )

  attr(out, "model") <- model
  if (length(models) > 1) {
    attr(out, "models") <- models
  }
  attr(out, "temperature") <- temperature
  attr(out, "n_simulations") <- n_simulations
  out <- apply_model_order(out, models)
  out
}
