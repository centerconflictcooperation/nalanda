#' Compute derived pre/post effect metrics from raw turn-level output
#'
#' Separates post-processing from model execution so users can re-compute
#' metrics without re-running API calls.
#'
#' @param x A data frame or list-like object from [run_ai_on_chapters()] with
#'   turn-level rows including `chapter`, `sim`, `identity`, `turn_type`, and
#'   `rating`. If a list is supplied, the function will attempt to combine its
#'   data-frame elements with [dplyr::bind_rows()] before computing metrics. If
#'   cumulative output from [run_ai_cumulative_chapters()] is detected, this
#'   function delegates to [compute_run_ai_metrics_cumulative()].
#' @param per_group Optional logical. Whether the run used per-group mode
#'   (`{group}` in question template). If `NULL` (default), mode is inferred
#'   from `target_group`:
#'   \itemize{
#'     \item per-group if any non-missing `target_group` values exist;
#'     \item single-question if `target_group` is entirely missing.
#'   }
#' @return A simulation-level tibble with derived metrics (for example
#'   `pre_outgroup`, `post_outgroup`, `delta_outgroup`, and in per-group mode
#'   also `pre_ingroup`, `post_ingroup`, `pre_gap`, `post_gap`,
#'   `delta_ingroup`, `delta_gap`).
#'
#' @examples
#' metrics <- compute_run_ai_metrics(toy_run_ai_turns)
#' head(metrics)
#'
#' # The processed output can be passed on to summary and plotting helpers.
#' summary_by_chapter <- summarize_chapter_scores(metrics)
#' head(summary_by_chapter)
#' @export
compute_run_ai_metrics <- function(x, per_group = NULL) {
  input <- x

  if (is.list(input) && !inherits(input, "data.frame")) {
    x <- flatten_sim_results(input)
  }

  x <- tibble::as_tibble(x)

  model <- attr(input, "model")
  models <- attr(input, "models")
  temperature <- attr(input, "temperature")
  n_simulations <- attr(input, "n_simulations")
  chapter_excerpts <- chapter_excerpt_index(input)

  if (is.null(model) && is.list(input) && !inherits(input, "data.frame") &&
    length(input) > 0) {
    model <- rlang::`%||%`(model, attr(input[[1]], "model"))
    temperature <- rlang::`%||%`(temperature, attr(input[[1]], "temperature"))
    n_simulations <- rlang::`%||%`(n_simulations, attr(input[[1]], "n_simulations"))
  }
  model <- normalize_model_name(model)
  models <- normalize_model_metadata(models)
  x <- fill_missing_model_column(x, model = model, models = models)

  required_cols <- c("chapter", "sim", "identity", "turn_type", "rating")
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0) {
    stop(
      "`x` is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      "."
    )
  }
  if (!("target_group" %in% names(x))) {
    x$target_group <- NA_character_
  }

  if (is_cumulative_turn_output(x)) {
    return(compute_run_ai_metrics_cumulative(input, per_group = per_group))
  }

  if (is.null(per_group)) {
    per_group <- any(!is.na(x$target_group) & nzchar(as.character(x$target_group)))
  }

  id_cols <- c("chapter", "sim", "identity")
  optional_id <- c(
    "model",
    "book",
    "party",
    "baseline_prompt",
    "post_prompt"
  )
  id_cols <- c(id_cols, intersect(optional_id, names(x)))

  # Aggregate tokens/cost at simulation level if present.
  include_token_col <- "input_tokens" %in% names(x)
  include_cost_col <- "cost" %in% names(x)

  # Build one row per simulation unit.
  unit_key <- interaction_key(x, id_cols)
  unit_indices <- split(seq_len(nrow(x)), unit_key)
  rows <- vector("list", length(unit_indices))
  row_i <- 0L
  for (idx in unit_indices) {
    row_i <- row_i + 1L
    xi <- x[idx, , drop = FALSE]
    row <- as.list(xi[1, id_cols, drop = FALSE])

    pre <- xi[xi$turn_type == "baseline", , drop = FALSE]
    post <- xi[xi$turn_type == "post", , drop = FALSE]

    if (nrow(pre) == 0 || nrow(post) == 0) {
      stop("Each simulation unit must include both baseline and post turns.")
    }

    if (include_token_col) {
      row$input_tokens <- sum(as.numeric(xi$input_tokens), na.rm = TRUE)
    }
    if (include_cost_col) {
      row$cost <- sum(as.numeric(xi$cost), na.rm = TRUE)
    }

    if (isTRUE(per_group)) {
      identity_label <- as.character(row$identity)

      row$pre_ingroup <- mean_or_na(
        pre$rating[as.character(pre$target_group) == identity_label]
      )
      row$post_ingroup <- mean_or_na(
        post$rating[as.character(post$target_group) == identity_label]
      )
      row$pre_outgroup <- mean_or_na(
        pre$rating[as.character(pre$target_group) != identity_label]
      )
      row$post_outgroup <- mean_or_na(
        post$rating[as.character(post$target_group) != identity_label]
      )

      row$pre_gap <- row$pre_ingroup - row$pre_outgroup
      row$post_gap <- row$post_ingroup - row$post_outgroup
      row$delta_ingroup <- row$post_ingroup - row$pre_ingroup
      row$delta_outgroup <- row$post_outgroup - row$pre_outgroup
      row$delta_gap <- row$delta_outgroup - row$delta_ingroup
    } else {
      row$pre_rating <- mean_or_na(pre$rating)
      row$post_rating <- mean_or_na(post$rating)
      row$pre_ingroup <- NA_real_
      row$post_ingroup <- NA_real_
      row$pre_outgroup <- row$pre_rating
      row$post_outgroup <- row$post_rating
      row$delta_outgroup <- row$post_outgroup - row$pre_outgroup
    }

    rows[[row_i]] <- row
  }

  out <- tibble::as_tibble(do.call(
    rbind.data.frame,
    lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE))
  ))

  long_cols <- c("baseline_prompt", "post_prompt")
  present_long <- intersect(long_cols, names(out))
  if (length(present_long) > 0) {
    out <- out[, c(setdiff(names(out), present_long), present_long)]
  }
  out <- arrange_metric_output(out)

  attr(out, "model") <- model
  if (length(models) > 1) {
    attr(out, "models") <- models
  }
  attr(out, "temperature") <- temperature
  attr(out, "n_simulations") <- n_simulations
  attr(out, "chapter_excerpts") <- chapter_excerpts
  out
}

mean_or_na <- function(x) {
  x <- as.numeric(x)
  if (!any(!is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}

fill_missing_model_column <- function(x, model = NULL, models = NULL) {
  if (!("model" %in% names(x))) {
    return(x)
  }

  model_values <- as.character(x$model)
  fill_value <- valid_model_label(model)

  if (is.null(fill_value) && length(models) == 1) {
    fill_value <- valid_model_label(models)
  }

  if (is.null(fill_value)) {
    observed <- unique(model_values[!is.na(model_values) & nzchar(model_values)])
    if (length(observed) == 1) {
      fill_value <- valid_model_label(observed)
    }
  }

  if (is.null(fill_value)) {
    return(x)
  }

  missing_model <- is.na(model_values) | !nzchar(model_values)
  model_values[missing_model] <- fill_value
  x$model <- model_values
  x
}

valid_model_label <- function(model) {
  if (is.null(model)) {
    return(NULL)
  }

  model <- as.character(model)[1]
  if (is.na(model) || !nzchar(model)) {
    return(NULL)
  }

  model
}

is_cumulative_turn_output <- function(x) {
  required <- c("book", "chapter_index", "turn_type")
  if (!all(required %in% names(x))) {
    return(FALSE)
  }

  has_baseline_unit <- any(
    x$turn_type == "baseline" &
      (as.character(x$chapter) == "baseline" | as.integer(x$chapter_index) == 0L),
    na.rm = TRUE
  )
  has_post_chapter <- any(
    x$turn_type == "post" &
      !is.na(x$chapter_index) &
      as.integer(x$chapter_index) > 0L,
    na.rm = TRUE
  )

  has_baseline_unit && has_post_chapter
}

arrange_metric_output <- function(out) {
  out <- tibble::as_tibble(out)
  if (nrow(out) == 0) {
    return(out)
  }

  if ("chapter_index" %in% names(out)) {
    out$.chapter_order <- suppressWarnings(as.integer(out$chapter_index))
  } else if ("chapter" %in% names(out)) {
    out$.chapter_order <- parse_chapter_numbers(out$chapter)
  } else {
    out$.chapter_order <- NA_integer_
  }

  out$.chapter_label_order <- if ("chapter" %in% names(out)) {
    as.character(out$chapter)
  } else {
    NA_character_
  }
  out$.model_order <- if ("model" %in% names(out)) {
    as.character(out$model)
  } else {
    NA_character_
  }
  out$.book_order <- if ("book" %in% names(out)) {
    as.character(out$book)
  } else {
    NA_character_
  }
  out$.identity_order <- if ("identity" %in% names(out)) {
    as.character(out$identity)
  } else {
    NA_character_
  }
  out$.party_order <- if ("party" %in% names(out)) {
    as.character(out$party)
  } else {
    NA_character_
  }

  out |>
    dplyr::arrange(
      .data$.model_order,
      .data$.book_order,
      .data$.chapter_order,
      .data$.chapter_label_order,
      dplyr::across(dplyr::any_of("sim")),
      .data$.identity_order,
      .data$.party_order
    ) |>
    dplyr::select(-dplyr::all_of(c(
      ".chapter_order", ".chapter_label_order", ".model_order", ".book_order",
      ".identity_order", ".party_order"
    )))
}
