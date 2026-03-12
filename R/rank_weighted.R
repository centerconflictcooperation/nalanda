#' Rank rows using a weighted rubric
#'
#' Compute a weighted score across selected numeric columns and return the input
#' data with a final `weighted_score`, sorted by score.
#'
#' @param data A data frame.
#' @param weights Named numeric vector of weights. Names must match columns in
#'   `data`, and weights must sum to 1.
#' @param normalize Logical. If `TRUE` (default), selected variables are scaled
#'   to `[0, 1]` using min-max normalization before weighting.
#' @param decreasing Logical. If `TRUE` (default), rows are sorted from highest
#'   to lowest `weighted_score`.
#'
#' @return A tibble containing all original columns plus `weighted_score`,
#'   sorted by score.
#' @export
rank_weighted <- function(
  data,
  weights,
  normalize = TRUE,
  decreasing = TRUE
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.numeric(weights) || anyNA(weights) || length(weights) == 0L) {
    stop("`weights` must be a non-empty numeric vector with no missing values.", call. = FALSE)
  }

  vars <- names(weights)
  if (is.null(vars) || anyNA(vars) || any(vars == "")) {
    stop("`weights` must be a named numeric vector.", call. = FALSE)
  }
  if (anyDuplicated(vars)) {
    stop("`weights` names must be unique.", call. = FALSE)
  }
  if (!is.logical(normalize) || length(normalize) != 1L || is.na(normalize)) {
    stop("`normalize` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(decreasing) || length(decreasing) != 1L || is.na(decreasing)) {
    stop("`decreasing` must be TRUE or FALSE.", call. = FALSE)
  }

  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0L) {
    stop(
      sprintf(
        "The following weight names are missing from `data`: %s",
        paste(missing_vars, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  non_numeric <- vars[!vapply(data[vars], is.numeric, logical(1))]
  if (length(non_numeric) > 0L) {
    stop(
      sprintf(
        "All weighted columns must be numeric. Non-numeric columns: %s",
        paste(non_numeric, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  w <- as.numeric(weights)
  w_sum <- sum(w)
  if (abs(w_sum - 1) >= 1e-8) {
    stop(
      sprintf("`weights` must sum to 1. Current sum is %.2f.", w_sum),
      call. = FALSE
    )
  }

  min_max_scale <- function(x) {
    rng <- range(x, na.rm = TRUE)
    span <- rng[2] - rng[1]
    if (!is.finite(span) || span == 0) {
      return(ifelse(is.na(x), NA_real_, 0))
    }
    (x - rng[1]) / span
  }

  df <- tibble::as_tibble(data)
  score_data <- dplyr::select(df, dplyr::all_of(vars))

  if (normalize) {
    score_data <- score_data |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), min_max_scale)
      )
  }

  df <- df |>
    dplyr::mutate(
      weighted_score = as.vector(as.matrix(score_data) %*% w)
    ) |>
    dplyr::arrange(if (decreasing) dplyr::desc(.data$weighted_score) else .data$weighted_score)

  df
}

