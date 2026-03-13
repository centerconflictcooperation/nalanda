#' Compute derived pre/post effect metrics from raw turn-level output
#'
#' Separates post-processing from model execution so users can re-compute
#' metrics without re-running API calls.
#'
#' @param x Tibble from [run_ai_on_chapters()] with turn-level rows including
#'   `chapter`, `sim`, `identity`, `turn_type`, and `rating`.
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
#' @export
compute_run_ai_metrics <- function(x, per_group = NULL) {
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

  if (is.null(per_group)) {
    per_group <- any(!is.na(x$target_group) & nzchar(as.character(x$target_group)))
  }

  id_cols <- c("chapter", "sim", "identity")
  optional_id <- c(
    "book",
    "party",
    "baseline_prompt",
    "post_prompt",
    "chapter_excerpt"
  )
  id_cols <- c(id_cols, intersect(optional_id, names(x)))

  # Aggregate tokens/cost at simulation level if present.
  include_token_col <- "input_tokens" %in% names(x)
  include_cost_col <- "cost" %in% names(x)

  # Build one row per simulation unit.
  unit_key <- interaction(x[id_cols], drop = TRUE, lex.order = TRUE)
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

      row$pre_ingroup <- mean(
        as.numeric(pre$rating[as.character(pre$target_group) == identity_label]),
        na.rm = TRUE
      )
      row$post_ingroup <- mean(
        as.numeric(post$rating[as.character(post$target_group) == identity_label]),
        na.rm = TRUE
      )
      row$pre_outgroup <- mean(
        as.numeric(pre$rating[as.character(pre$target_group) != identity_label]),
        na.rm = TRUE
      )
      row$post_outgroup <- mean(
        as.numeric(post$rating[as.character(post$target_group) != identity_label]),
        na.rm = TRUE
      )

      row$pre_gap <- row$pre_ingroup - row$pre_outgroup
      row$post_gap <- row$post_ingroup - row$post_outgroup
      row$delta_ingroup <- row$post_ingroup - row$pre_ingroup
      row$delta_outgroup <- row$post_outgroup - row$pre_outgroup
      row$delta_gap <- row$delta_outgroup - row$delta_ingroup
    } else {
      row$pre_rating <- mean(as.numeric(pre$rating), na.rm = TRUE)
      row$post_rating <- mean(as.numeric(post$rating), na.rm = TRUE)
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

  long_cols <- c("baseline_prompt", "post_prompt", "chapter_excerpt")
  present_long <- intersect(long_cols, names(out))
  if (length(present_long) > 0) {
    out <- out[, c(setdiff(names(out), present_long), present_long)]
  }

  out
}
