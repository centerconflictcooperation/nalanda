#' Tidy hierarchical forecast aggregation results
#'
#' Converts the stage tables returned by [aggregate_model_forecasts()] into a
#' single long table.  The result is useful for diagnostics and plotting, and
#' retains the identifiers that apply at each aggregation level.
#'
#' @param aggregation A named list returned by [aggregate_model_forecasts()].
#' @param unit_by Character vector of columns identifying a forecast target.
#' @param outcomes Optional character vector of numeric outcome columns. When
#'   omitted, numeric columns other than count and identifier columns are used.
#' @param completion_col,prompt_col,model_col,family_col Names of the identifier
#'   columns used when the aggregation was made. `family_col` may be `NULL`.
#'
#' @return A tibble with `aggregation_level`, the available identifiers,
#'   `outcome`, `estimate`, `contributor_type`, and `n_contributors`.
#' @export
tidy_forecast_aggregation <- function(
  aggregation,
  unit_by,
  outcomes = NULL,
  completion_col = "completion",
  prompt_col = "prompt_id",
  model_col = "model_id",
  family_col = "family"
) {
  forecast_check_columns(unit_by, "`unit_by`")
  if (!is.list(aggregation) || is.null(names(aggregation))) {
    stop("`aggregation` must be a named list returned by `aggregate_model_forecasts()`.", call. = FALSE)
  }
  identifier_cols <- unique(c(unit_by, completion_col, prompt_col, model_col, family_col))
  if (!is.null(outcomes)) forecast_check_columns(outcomes, "`outcomes`")

  count_by_stage <- c(prompt = "n_completions", model = "n_prompts",
                      family = "n_models", consensus = if (is.null(family_col)) "n_models" else "n_families")
  stages <- intersect(names(count_by_stage), names(aggregation))
  if (length(stages) == 0L) {
    stop("`aggregation` has no recognized forecast aggregation stages.", call. = FALSE)
  }

  purrr::map_dfr(stages, function(stage) {
    data <- aggregation[[stage]]
    if (is.null(data)) return(tibble::tibble())
    if (!inherits(data, "data.frame")) {
      stop("Aggregation stage `", stage, "` must be a data frame.", call. = FALSE)
    }
    count_col <- count_by_stage[[stage]]
    needed <- c(unit_by, count_col)
    missing <- setdiff(needed, names(data))
    if (length(missing) > 0L) {
      stop("Aggregation stage `", stage, "` is missing column(s): ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
    stage_outcomes <- outcomes
    if (is.null(stage_outcomes)) {
      stage_outcomes <- names(data)[vapply(data, is.numeric, logical(1))]
      stage_outcomes <- setdiff(stage_outcomes, c(identifier_cols, grep("^n_", names(data), value = TRUE)))
    }
    missing_outcomes <- setdiff(stage_outcomes, names(data))
    if (length(missing_outcomes) > 0L) {
      stop("Aggregation stage `", stage, "` is missing outcome(s): ",
           paste(missing_outcomes, collapse = ", "), call. = FALSE)
    }
    non_numeric <- stage_outcomes[!vapply(data[stage_outcomes], is.numeric, logical(1))]
    if (length(non_numeric) > 0L) {
      stop("Outcome columns must be numeric: ", paste(non_numeric, collapse = ", "), call. = FALSE)
    }
    keep_ids <- intersect(identifier_cols, names(data))
    tidyr::pivot_longer(
      tibble::as_tibble(data)[c(keep_ids, stage_outcomes, count_col)],
      cols = dplyr::all_of(stage_outcomes), names_to = "outcome", values_to = "estimate"
    ) |>
      dplyr::rename(n_contributors = dplyr::all_of(count_col)) |>
      dplyr::mutate(
        aggregation_level = stage,
        contributor_type = sub("^n_", "", count_col),
        .before = 1
      )
  })
}

#' Summarize forecast disagreement within units
#'
#' Computes distribution summaries across any contributor type, such as models
#' or prompt variants. Missing estimates are counted explicitly in
#' `n_nonmissing` and `n_missing`; with `na_rm = FALSE`, any missing estimate
#' makes the numeric summaries for that unit missing.
#'
#' @param data A data frame containing contributor-level estimates.
#' @param unit_by Character vector identifying the units to summarize within.
#' @param estimate_col Numeric estimate column.
#' @param contributor_col Contributor or source identifier column.
#' @param na_rm Whether to omit missing estimates from numeric summaries.
#' @param scale_width_col Optional positive numeric column giving the width of
#'   the outcome scale within each unit. If supplied, normalized SD, MAD, and
#'   range columns are returned.
#'
#' @return A tibble with contributor counts, missingness counts, and mean,
#'   median, SD, MAD, minimum, maximum, and range.
#' @export
summarize_forecast_disagreement <- function(
  data,
  unit_by,
  estimate_col = "estimate",
  contributor_col,
  na_rm = TRUE,
  scale_width_col = NULL
) {
  if (!inherits(data, "data.frame")) stop("`data` must be a data frame.", call. = FALSE)
  forecast_check_columns(c(unit_by, estimate_col, contributor_col), "column names")
  if (!is.logical(na_rm) || length(na_rm) != 1L || is.na(na_rm)) stop("`na_rm` must be TRUE or FALSE.", call. = FALSE)
  if (!is.null(scale_width_col)) forecast_check_columns(scale_width_col, "`scale_width_col`")
  required <- c(unit_by, estimate_col, contributor_col, scale_width_col)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) stop("Missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  if (!is.numeric(data[[estimate_col]])) stop("`estimate_col` must name a numeric column.", call. = FALSE)
  if (!is.null(scale_width_col) && !is.numeric(data[[scale_width_col]])) stop("`scale_width_col` must name a numeric column.", call. = FALSE)

  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(unit_by))) |>
    dplyr::group_modify(function(.x, .y) {
      values <- .x[[estimate_col]]
      width <- if (is.null(scale_width_col)) NULL else forecast_group_width(.x[[scale_width_col]], scale_width_col)
      summary <- forecast_disagreement_row(values, .x[[contributor_col]], na_rm)
      if (!is.null(width)) {
        summary$scale_width <- width
        summary$sd_per_scale <- summary$sd / width
        summary$mad_per_scale <- summary$mad / width
        summary$range_per_scale <- summary$range / width
      }
      tibble::as_tibble(summary)
    }) |>
    dplyr::ungroup()
}

#' Compare estimates from two forecast aggregations
#'
#' Aligns a selected stage from two compatible outputs of
#' [aggregate_model_forecasts()] and reports signed and absolute differences.
#' Unlike a permissive join, this helper errors when either result has missing
#' or duplicate unit--outcome identities.
#'
#' @param first,second Named aggregation lists to compare.
#' @param stage Aggregation stage: one of `"prompt"`, `"model"`, `"family"`,
#'   or `"consensus"`.
#' @param unit_by Character vector identifying a forecast target.
#' @param outcomes Character vector of outcome columns.
#' @param labels Two names for the estimate columns in the result.
#' @param scale_width Optional scale-width specification: a character column
#'   present in both selected stage tables, or a data frame with `unit_by`,
#'   optional `outcome`, and a numeric `scale_width` column.
#'
#' @return A tibble with both estimates, `difference` (`second - first`),
#'   `absolute_difference`, and optional scale-normalized differences.
#' @export
compare_forecast_aggregations <- function(
  first, second, stage = "consensus", unit_by, outcomes,
  labels = c("first", "second"), scale_width = NULL
) {
  forecast_check_columns(unit_by, "`unit_by`")
  forecast_check_columns(outcomes, "`outcomes`")
  if (!is.character(stage) || length(stage) != 1L || !stage %in% c("prompt", "model", "family", "consensus")) {
    stop("`stage` must be one of: prompt, model, family, consensus.", call. = FALSE)
  }
  if (!is.character(labels) || length(labels) != 2L || anyNA(labels) || any(!nzchar(labels)) || labels[[1]] == labels[[2]]) {
    stop("`labels` must contain two distinct non-empty names.", call. = FALSE)
  }
  width_col <- if (is.character(scale_width) && length(scale_width) == 1L && !is.na(scale_width)) scale_width else NULL
  left <- forecast_comparison_stage(first, stage, unit_by, outcomes, "first", width_col)
  right <- forecast_comparison_stage(second, stage, unit_by, outcomes, "second", width_col)
  keys <- c(unit_by, "outcome")
  forecast_require_same_keys(left, right, keys)
  joined <- dplyr::inner_join(left, right, by = keys, suffix = c("_first", "_second"))
  if (!is.null(width_col)) {
    first_width <- joined[[paste0(width_col, "_first")]]
    second_width <- joined[[paste0(width_col, "_second")]]
    if (!isTRUE(all.equal(first_width, second_width, check.attributes = FALSE))) {
      stop("The selected stage has different scale widths in `first` and `second`.", call. = FALSE)
    }
  }
  out <- joined |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(keys)),
      !!labels[[1]] := .data$estimate_first,
      !!labels[[2]] := .data$estimate_second,
      difference = .data$estimate_second - .data$estimate_first,
      absolute_difference = abs(.data$estimate_second - .data$estimate_first),
      scale_width = if (is.null(width_col)) NULL else .data[[paste0(width_col, "_first")]]
    )
  if (!is.null(width_col)) return(forecast_normalize_differences(out))
  forecast_add_scale_width(out, scale_width, unit_by)
}

forecast_check_columns <- function(x, argument) {
  if (!is.character(x) || length(x) < 1L || anyNA(x) || any(!nzchar(x)) || anyDuplicated(x)) {
    stop(argument, " must contain unique non-empty column names.", call. = FALSE)
  }
}

forecast_disagreement_row <- function(values, contributors, na_rm) {
  missing <- is.na(values)
  usable <- values[!missing]
  blocked <- !na_rm && any(missing)
  statistic <- function(fun, ...) if (blocked || length(usable) == 0L) NA_real_ else fun(usable, ...)
  min_value <- statistic(min)
  max_value <- statistic(max)
  list(
    n_contributors = dplyr::n_distinct(contributors, na.rm = TRUE),
    n_nonmissing = sum(!missing), n_missing = sum(missing),
    mean = statistic(mean), median = statistic(stats::median),
    sd = statistic(stats::sd), mad = statistic(stats::mad),
    min = min_value, max = max_value,
    range = if (is.na(min_value) || is.na(max_value)) NA_real_ else max_value - min_value
  )
}

forecast_group_width <- function(width, name) {
  values <- unique(width[!is.na(width)])
  if (length(values) != 1L || !is.finite(values) || values <= 0) {
    stop("`", name, "` must contain one positive, non-missing value within each unit.", call. = FALSE)
  }
  values[[1]]
}

forecast_comparison_stage <- function(aggregation, stage, unit_by, outcomes, side, extra_col = NULL) {
  if (!is.list(aggregation) || is.null(aggregation[[stage]]) || !inherits(aggregation[[stage]], "data.frame")) {
    stop("`", side, "` must contain a data-frame `", stage, "` stage.", call. = FALSE)
  }
  data <- aggregation[[stage]]
  required <- c(unit_by, outcomes, extra_col)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) stop("`", side, "` stage is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  if (anyDuplicated(data[unit_by])) stop("`", side, "` stage has duplicate unit identities.", call. = FALSE)
  if (anyNA(data[unit_by])) stop("`", side, "` stage has missing unit identities.", call. = FALSE)
  non_numeric <- outcomes[!vapply(data[outcomes], is.numeric, logical(1))]
  if (length(non_numeric) > 0L) stop("Outcome columns must be numeric: ", paste(non_numeric, collapse = ", "), call. = FALSE)
  if (!is.null(extra_col) && !is.numeric(data[[extra_col]])) {
    stop("Scale-width column `", extra_col, "` must be numeric.", call. = FALSE)
  }
  tidyr::pivot_longer(tibble::as_tibble(data)[c(unit_by, extra_col, outcomes)], dplyr::all_of(outcomes), names_to = "outcome", values_to = "estimate")
}

forecast_require_same_keys <- function(left, right, keys) {
  absent_left <- dplyr::anti_join(left, right, by = keys)
  absent_right <- dplyr::anti_join(right, left, by = keys)
  if (nrow(absent_left) > 0L || nrow(absent_right) > 0L) {
    stop("Aggregation stages do not contain the same unit--outcome identities.", call. = FALSE)
  }
}

forecast_add_scale_width <- function(data, scale_width, unit_by) {
  if (is.null(scale_width)) return(data)
  keys <- c(unit_by, "outcome")
  if (is.character(scale_width) && length(scale_width) == 1L && !is.na(scale_width)) return(data)
  if (!inherits(scale_width, "data.frame") || !"scale_width" %in% names(scale_width)) {
    stop("`scale_width` must be NULL or a data frame with a `scale_width` column.", call. = FALSE)
  }
  lookup_keys <- intersect(keys, names(scale_width))
  if (!all(unit_by %in% lookup_keys)) stop("Scale-width lookup must include every `unit_by` column.", call. = FALSE)
  if (anyDuplicated(scale_width[lookup_keys])) stop("Scale-width lookup has duplicate identities.", call. = FALSE)
  if (!is.numeric(scale_width$scale_width)) stop("`scale_width$scale_width` must be numeric.", call. = FALSE)
  out <- dplyr::left_join(data, tibble::as_tibble(scale_width)[c(lookup_keys, "scale_width")], by = lookup_keys)
  if (anyNA(out$scale_width) || any(!is.finite(out$scale_width)) || any(out$scale_width <= 0)) {
    stop("Scale-width lookup must provide one positive width for every compared identity.", call. = FALSE)
  }
  forecast_normalize_differences(out)
}

forecast_normalize_differences <- function(data) {
  if (anyNA(data$scale_width) || any(!is.finite(data$scale_width)) || any(data$scale_width <= 0)) {
    stop("Scale widths must be positive and non-missing.", call. = FALSE)
  }
  dplyr::mutate(data,
    normalized_difference = .data$difference / .data$scale_width,
    normalized_absolute_difference = .data$absolute_difference / .data$scale_width
  )
}
