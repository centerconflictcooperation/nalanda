#' Summarize whether the model adopts the requested identity
#'
#' This helper turns raw simulation output into a tally table showing which
#' reported `party` was returned for each requested `identity`. It is especially
#' useful for first-turn checks from [run_ai_on_chapters_one_turn()], where the
#' main question is whether the model actually accepts the assigned identity.
#'
#' Because raw chapter outputs can contain repeated rows per simulation
#' (for example one row per target group, or one row per turn), this function
#' first reduces the input to one identity-assignment row per simulated unit.
#'
#' @param x A data frame or list-like object containing raw rows from
#'   [run_ai_on_chapters()], [run_ai_on_chapters_one_turn()], or another
#'   workflow with `identity`, `party`, and `sim` columns.
#' @param by Character vector of columns to group by before tallying.
#'   Defaults to `c("model", "book", "chapter", "identity")`. Missing columns
#'   are silently ignored. When `compact = TRUE`, the default drops `identity`
#'   so the result is one row per model/chapter grouping unless you explicitly
#'   include `identity`.
#' @param compact Logical. If `TRUE`, return a wide compact summary with one row
#'   per grouping combination and one `rate_*` column per observed identity
#'   label.
#' @param expected_col Character scalar naming the requested identity column.
#'   Defaults to `"identity"`.
#' @param observed_col Character scalar naming the model-reported identity
#'   column. Defaults to `"party"`.
#'
#' @return A tibble with one row per grouping combination and reported identity,
#'   including counts (`n`), totals within group (`total_n`), proportions
#'   (`prop`), and a logical `matches_requested`.
#'
#' @examples
#' x <- tibble::tibble(
#'   chapter = c("chapter_1", "chapter_1", "chapter_1", "chapter_1"),
#'   sim = c(1, 1, 2, 2),
#'   identity = c("Democrat", "Democrat", "Democrat", "Democrat"),
#'   party = c("Democrat", "Democrat", "Republican", "Republican"),
#'   target_group = c("Democrat", "Republican", "Democrat", "Republican"),
#'   rating = c(70, 40, 68, 35)
#' )
#'
#' summarize_identity_adherence(x)
#' @export
summarize_identity_adherence <- function(
  x,
  by = c("model", "book", "chapter", "identity"),
  compact = FALSE,
  expected_col = "identity",
  observed_col = "party"
) {
  model <- attr(x, "model")
  models <- attr(x, "models")
  temperature <- attr(x, "temperature")
  n_simulations <- attr(x, "n_simulations")

  if (is.null(model) && is.list(x) && !inherits(x, "data.frame") &&
    length(x) > 0) {
    model <- rlang::`%||%`(model, attr(x[[1]], "model"))
    temperature <- rlang::`%||%`(temperature, attr(x[[1]], "temperature"))
    n_simulations <- rlang::`%||%`(n_simulations, attr(x[[1]], "n_simulations"))
  }
  model <- normalize_model_name(model)
  models <- normalize_model_metadata(models)

  df <- if (is.list(x) && !inherits(x, "data.frame")) {
    flatten_sim_results(x)
  } else {
    x
  }

  df <- tibble::as_tibble(df)

  if (isTRUE(compact) && identical(by, c("model", "book", "chapter", "identity"))) {
    by <- intersect(c("model", "book", "chapter"), names(df))
    if (length(by) == 0) {
      by <- intersect("model", names(df))
    }
  }

  required_cols <- c(expected_col, observed_col, "sim")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "`x` is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      "."
    )
  }

  by_present <- intersect(by, names(df))
  base_unit_cols <- intersect(
    c("model", "book", "chapter", "treatment", "treatment_index", "chapter_index", "sim"),
    names(df)
  )
  unit_cols <- unique(c(base_unit_cols, by_present, expected_col, observed_col))

  deduped <- dplyr::distinct(df, dplyr::across(dplyr::all_of(unit_cols)))

  by_cols <- unique(c(intersect(by, names(deduped)), observed_col))

  out <- deduped |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by_cols))) |>
    dplyr::summarise(
      n = dplyr::n(),
      .groups = "drop"
    )

  total_cols <- setdiff(by_cols, observed_col)

  if (length(total_cols) > 0) {
    out <- out |>
      dplyr::group_by(dplyr::across(dplyr::all_of(total_cols))) |>
      dplyr::mutate(
        total_n = sum(.data$n),
        prop = .data$n / .data$total_n
      ) |>
      dplyr::ungroup()
  } else {
    total_n <- sum(out$n)
    out <- out |>
      dplyr::mutate(
        total_n = total_n,
        prop = .data$n / .data$total_n
      )
  }

  if (expected_col %in% names(out)) {
    out <- out |>
      dplyr::mutate(
        matches_requested = as.character(.data[[observed_col]]) ==
          as.character(.data[[expected_col]])
      )
  } else {
    out$matches_requested <- NA
  }

  arrange_cols <- intersect(
    c("book", "chapter", "treatment", expected_col, observed_col),
    names(out)
  )
  out <- dplyr::arrange(out, dplyr::across(dplyr::all_of(arrange_cols)))

  if (isTRUE(compact)) {
    suffix <- function(x) {
      x <- tolower(as.character(x))
      x <- gsub("[^a-z0-9]+", "_", x)
      gsub("^_|_$", "", x)
    }

    id_cols <- intersect(setdiff(by, observed_col), names(out))
    out <- out |>
      dplyr::mutate(.observed_key = suffix(.data[[observed_col]])) |>
      dplyr::select(dplyr::all_of(id_cols), "total_n", ".observed_key", "prop") |>
      dplyr::distinct() |>
      tidyr::pivot_wider(
        id_cols = dplyr::all_of(c(id_cols, "total_n")),
        names_from = ".observed_key",
        values_from = "prop",
        names_prefix = "rate_"
      ) |>
      dplyr::rename(n = "total_n")
  }

  attr(out, "model") <- model
  if (length(models) > 1) {
    attr(out, "models") <- models
  }
  attr(out, "temperature") <- temperature
  attr(out, "n_simulations") <- n_simulations
  out <- apply_model_order(out, models)
  out
}

#' Summarize identity match rates by model
#'
#' This helper converts raw identity-adoption output into one row per grouping
#' combination, reporting how often the model's reported identity matched the
#' requested one.
#'
#' @param x A data frame or list-like object from a simulation workflow
#'   containing `identity` and `party`.
#' @param by Character vector of columns to group by. Defaults to
#'   `c("model", "identity")`.
#' @param compact Logical. If `TRUE`, return a wide one-row-per-group summary
#'   with one rate column per requested identity plus a shared `n` column.
#' @param expected_col Character scalar naming the requested identity column.
#' @param observed_col Character scalar naming the model-reported identity
#'   column.
#'
#' @return A tibble with counts and match rates, including `n_requested`,
#'   `n_match`, `adoption_rate`, `n_mismatch`, and `mismatch_rate`.
#' @export
summarize_identity_match_rates <- function(
  x,
  by = c("model", "identity"),
  compact = FALSE,
  expected_col = "identity",
  observed_col = "party"
) {
  breakdown <- summarize_identity_adherence(
    x = x,
    by = unique(c(by, expected_col)),
    expected_col = expected_col,
    observed_col = observed_col
  )

  group_cols <- intersect(unique(c(by, expected_col)), names(breakdown))

  out <- breakdown |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      n_requested = dplyr::first(.data$total_n),
      n_match = sum(ifelse(.data$matches_requested, .data$n, 0L)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      adoption_rate = .data$n_match / .data$n_requested,
      n_mismatch = .data$n_requested - .data$n_match,
      mismatch_rate = .data$n_mismatch / .data$n_requested
    )

  arrange_cols <- intersect(c("model", expected_col), names(out))
  out <- dplyr::arrange(out, dplyr::across(dplyr::all_of(arrange_cols)))

  if (isTRUE(compact) && expected_col %in% names(out)) {
    suffix <- function(x) {
      x <- tolower(as.character(x))
      x <- gsub("[^a-z0-9]+", "_", x)
      gsub("^_|_$", "", x)
    }

    out <- out |>
      dplyr::mutate(
        .identity_key = suffix(.data[[expected_col]])
      ) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(group_cols, expected_col)))) |>
      dplyr::mutate(n = dplyr::first(.data$n_requested)) |>
      dplyr::ungroup() |>
      tidyr::pivot_wider(
        id_cols = dplyr::all_of(unique(c(setdiff(group_cols, expected_col), "n"))),
        names_from = ".identity_key",
        values_from = "adoption_rate",
        names_prefix = "rate_"
      )
  }

  attr(out, "model") <- attr(x, "model")
  if (!is.null(attr(x, "models"))) {
    attr(out, "models") <- attr(x, "models")
  }
  attr(out, "temperature") <- attr(x, "temperature")
  attr(out, "n_simulations") <- attr(x, "n_simulations")
  out <- apply_model_order(out, attr(x, "models"))
  out
}

apply_model_order <- function(df, model_order = NULL) {
  if (!("model" %in% names(df))) {
    return(df)
  }

  model_order <- normalize_model_metadata(model_order)
  if (is.null(model_order) || length(model_order) < 1) {
    return(df)
  }

  df$model <- factor(df$model, levels = model_order, ordered = TRUE)
  df <- dplyr::arrange(df, .data$model)
  df$model <- as.character(df$model)
  df
}
