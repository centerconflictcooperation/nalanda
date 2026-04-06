#' Summarize generic treatment results
#'
#' Aggregate raw outputs from [simulate_treatment()] by treatment,
#' computing counts plus means and SDs for numeric response fields. This is
#' useful for structured outputs such as readability, sentiment, or any other
#' custom numeric scores returned by the model.
#'
#' @param x A data frame or list-like object containing raw rows as produced by
#'   [simulate_treatment()]. If a list is supplied, nested data frames are
#'   flattened before summarising.
#' @param by_identity Logical. If `TRUE`, summaries are computed separately by
#'   identity when an `identity` column is present.
#' @param by_turn Logical. If `TRUE` (default), summaries are computed
#'   separately by turn using `turn_type` when available, otherwise `turn_index`.
#' @param fields Optional character vector of numeric columns to summarize.
#'   Defaults to all numeric columns except common bookkeeping fields such as
#'   `sim`, `turn_index`, and `treatment_index`.
#'
#' @return A tibble summarizing the requested aggregation level. Numeric fields
#'   are returned as `mean_*` and `sd_*` columns, alongside a `sim` count.
#'   Model metadata attributes are copied to the result.
#'
#' @examples
#' readability <- tibble::tibble(
#'   treatment = c("treatment_1", "treatment_1", "treatment_2"),
#'   sim = c(1, 2, 1),
#'   turn_type = "turn_1",
#'   readability_score = c(6, 8, 7),
#'   readability_confidence = c(4, 5, 4)
#' )
#'
#' summarize_treatment_results(readability)
#' @export
summarize_treatment_results <- function(
  x,
  by_identity = FALSE,
  by_turn = TRUE,
  fields = NULL
) {
  model <- attr(x, "model")
  temperature <- attr(x, "temperature")
  n_simulations <- attr(x, "n_simulations")

  if (is.null(model) && is.list(x) && !inherits(x, "data.frame") &&
    length(x) > 0) {
    model <- model %||% attr(x[[1]], "model")
    temperature <- temperature %||% attr(x[[1]], "temperature")
    n_simulations <- n_simulations %||% attr(x[[1]], "n_simulations")
  }
  model <- normalize_model_name(model)

  df <- if (is.list(x) && !inherits(x, "data.frame")) {
    flatten_sim_results(x)
  } else {
    x
  }

  df <- tibble::as_tibble(df)

  if (!"treatment" %in% names(df)) {
    candidates <- grep(
      "treat|interven|file|name",
      names(df),
      value = TRUE,
      ignore.case = TRUE
    )
    candidate <- if (length(candidates) > 0) candidates[1] else NA_character_
    if (!is.na(candidate) && nzchar(candidate)) {
      df <- dplyr::rename(df, treatment = !!rlang::sym(candidate))
    } else {
      stop(
        "Input data must contain a 'treatment' column (or a close alternative)"
      )
    }
  }

  has_identity <- "identity" %in% names(df)
  has_turn_type <- "turn_type" %in% names(df)
  has_turn_index <- "turn_index" %in% names(df)

  if (is.null(fields)) {
    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    fields <- setdiff(
      numeric_cols,
      c("sim", "turn_index", "treatment_index")
    )
  }

  if (length(fields) == 0) {
    stop("No numeric response fields available to summarize.")
  }

  missing_fields <- setdiff(fields, names(df))
  if (length(missing_fields) > 0) {
    stop(
      "Requested fields not found in input: ",
      paste(missing_fields, collapse = ", ")
    )
  }

  non_numeric_fields <- fields[!vapply(df[fields], is.numeric, logical(1))]
  if (length(non_numeric_fields) > 0) {
    stop(
      "All `fields` must be numeric. Non-numeric fields: ",
      paste(non_numeric_fields, collapse = ", ")
    )
  }

  group_cols <- character()
  group_cols <- c(group_cols, "treatment")
  if (by_identity && has_identity) group_cols <- c(group_cols, "identity")
  if (by_turn) {
    if (has_turn_type) {
      group_cols <- c(group_cols, "turn_type")
    } else if (has_turn_index) {
      group_cols <- c(group_cols, "turn_index")
    }
  }

  summary_exprs <- unlist(
    lapply(fields, function(field) {
      list(
        rlang::expr(mean(!!rlang::sym(field), na.rm = TRUE)),
        rlang::expr(stats::sd(!!rlang::sym(field), na.rm = TRUE))
      )
    }),
    recursive = FALSE
  )
  names(summary_exprs) <- unlist(
    lapply(fields, function(field) c(paste0("mean_", field), paste0("sd_", field)))
  )

  df <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      sim = dplyr::n(),
      !!!summary_exprs,
      .groups = "drop"
    )

  extract_treatment_num <- function(x) {
    suppressWarnings(as.integer(stringr::str_extract(x, "\\d+")))
  }

  df <- df |>
    dplyr::mutate(treatment_num = extract_treatment_num(.data$treatment))

  arrange_cols <- intersect(
    c("turn_type", "turn_index", "identity", "treatment_num", "treatment"),
    names(df)
  )

  df <- dplyr::arrange(df, dplyr::across(dplyr::all_of(arrange_cols)))

  index_groups <- intersect(
    c("turn_type", "turn_index", "identity"),
    names(df)
  )
  if (length(index_groups) > 0) {
    df <- df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(index_groups))) |>
      dplyr::mutate(treatment_index = dplyr::row_number()) |>
      dplyr::ungroup()
  } else {
    df <- df |>
      dplyr::mutate(treatment_index = dplyr::row_number())
  }

  df <- dplyr::select(df, -dplyr::all_of("treatment_num"))

  attr(df, "model") <- model
  attr(df, "temperature") <- temperature
  attr(df, "n_simulations") <- n_simulations
  df
}
