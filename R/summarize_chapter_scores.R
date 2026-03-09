#' Summarize simulated chapter scores
#'
#' Aggregate simulation results by chapter (and book, if present) computing
#' number of simulations, means and SDs for core model outputs. In the current
#' schema, this includes ingroup/outgroup pre-post ratings plus delta and gap
#' metrics (for example delta_outgroup, delta_ingroup, and delta_gap).
#' Retains a chapter excerpt at chapter level.
#'
#' @param x A data frame or list-like object containing simulation rows as
#'   produced by run_ai_on_chapters(). Expected columns include chapter,
#'   pre/post ingroup-outgroup fields, and the derived difference columns used
#'   in summaries (gap_pre, gap_post, delta_outgroup, delta_ingroup, delta_gap).
#'   If book and party are present, the summary will include those groupings.
#' @param aggregate_level Character. One of "chapter" (default) or "book".
#'   When "book", results are aggregated to the book level.
#' @param by_party Logical. If TRUE, summaries are computed separately by party
#'   (if present).
#' @return A tibble summarizing each chapter (and book if present). The returned
#'   object will have the original model attribute copied to it.
#' @export
summarize_chapter_scores <- function(
  x,
  aggregate_level = c("chapter", "book"),
  by_party = FALSE
) {
  aggregate_level <- match.arg(aggregate_level)
  model <- attr(x, "model")
  temperature <- attr(x, "temperature")

  # Fallback: check first list element (covers lapply(files, readRDS) workflow)
  if (is.null(model) && is.list(x) && !inherits(x, "data.frame") &&
    length(x) > 0) {
    model <- model %||% attr(x[[1]], "model")
    temperature <- temperature %||% attr(x[[1]], "temperature")
  }

  df <- if (is.list(x) && !inherits(x, "data.frame")) {
    flatten_sim_results(x)
  } else {
    x
  }

  # Ensure we operate on a plain tibble/data.frame to avoid S3 surprises
  df <- tibble::as_tibble(df)

  # ensure chapter column exists (try to infer common alternatives)
  if (!"chapter" %in% names(df)) {
    candidates <- grep(
      "chap|file|name",
      names(df),
      value = TRUE,
      ignore.case = TRUE
    )
    candidate <- if (length(candidates) > 0) candidates[1] else NA_character_
    if (!is.na(candidate) && nzchar(candidate)) {
      df <- dplyr::rename(df, chapter = !!rlang::sym(candidate))
    } else {
      stop(
        "Input data must contain a 'chapter' column (or a close alternative)"
      )
    }
  }

  has_party <- "party" %in% names(df)
  has_book <- "book" %in% names(df)
  has_identity <- "identity" %in% names(df)

  extract_chapter_num <- function(ch) {
    suppressWarnings(as.integer(stringr::str_extract(ch, "\\d+")))
  }

  # --- Determine grouping columns ---
  group_cols <- c()
  if (has_book) group_cols <- c(group_cols, "book")
  if (aggregate_level == "chapter") group_cols <- c(group_cols, "chapter")
  if (by_party && has_party) group_cols <- c(group_cols, "party")
  if (has_identity) group_cols <- c(group_cols, "identity")

  # --- Summarise ---
  df <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      sim = dplyr::n(),
      mean_pre_ingroup = mean(pre_ingroup, na.rm = TRUE),
      sd_pre_ingroup = sd(pre_ingroup, na.rm = TRUE),
      mean_post_ingroup = mean(post_ingroup, na.rm = TRUE),
      sd_post_ingroup = sd(post_ingroup, na.rm = TRUE),
      mean_pre_outgroup = mean(pre_outgroup, na.rm = TRUE),
      sd_pre_outgroup = sd(pre_outgroup, na.rm = TRUE),
      mean_post_outgroup = mean(post_outgroup, na.rm = TRUE),
      sd_post_outgroup = sd(post_outgroup, na.rm = TRUE),
      mean_gap_pre = mean(gap_pre, na.rm = TRUE),
      sd_gap_pre = sd(gap_pre, na.rm = TRUE),
      mean_gap_post = mean(gap_post, na.rm = TRUE),
      sd_gap_post = sd(gap_post, na.rm = TRUE),
      mean_delta_outgroup = mean(delta_outgroup, na.rm = TRUE),
      sd_delta_outgroup = sd(delta_outgroup, na.rm = TRUE),
      mean_delta_ingroup = mean(delta_ingroup, na.rm = TRUE),
      sd_delta_ingroup = sd(delta_ingroup, na.rm = TRUE),
      mean_delta_gap = mean(delta_gap, na.rm = TRUE),
      sd_delta_gap = sd(delta_gap, na.rm = TRUE),
      chapter_excerpt = if (aggregate_level == "chapter") {
        dplyr::first(chapter_excerpt)
      } else {
        NA_character_
      },
      .groups = "drop"
    )

  # --- Add chapter ordering for chapter-level results ---
  if (aggregate_level == "chapter") {
    df <- df |>
      dplyr::mutate(chapter_num = extract_chapter_num(chapter))

    if (has_book) {
      df <- df |>
        dplyr::arrange(book, chapter_num, chapter) |>
        dplyr::group_by(book) |>
        dplyr::mutate(chapter_index = dplyr::row_number()) |>
        dplyr::ungroup()
    } else {
      df <- df |>
        dplyr::arrange(chapter_num, chapter) |>
        dplyr::mutate(chapter_index = dplyr::row_number())
    }

    df <- dplyr::select(df, -chapter_num)
  }

  attr(df, "model") <- model
  attr(df, "temperature") <- temperature
  df
}

# helper to robustly flatten various nested formats produced by run_ai_on_chapters
flatten_sim_results <- function(z) {
  # If already a data.frame, return as-is
  if (inherits(z, "data.frame")) {
    return(z)
  }
  if (!is.list(z)) {
    stop("Unsupported input: expected data.frame or list-like object")
  }

  # Recursively collect all data.frames inside the (possibly nested) list.
  collect_dfs <- function(obj, parent_name = NULL) {
    out <- list()
    if (inherits(obj, "data.frame")) {
      df <- obj
      # if parent name exists and there's no book column, attach it
      if (!is.null(parent_name) && !"book" %in% names(df)) {
        df$book <- parent_name
      }
      return(list(df))
    }
    if (is.list(obj)) {
      nm <- names(obj)
      for (i in seq_along(obj)) {
        child_name <- if (!is.null(nm) && nzchar(nm[i])) {
          nm[i]
        } else {
          parent_name
        }
        out <- c(out, collect_dfs(obj[[i]], parent_name = child_name))
      }
    }
    out
  }

  dfs <- collect_dfs(z, parent_name = NULL)
  if (length(dfs) == 0) {
    stop("Unsupported nested structure for simulation results")
  }
  # bind rows; if we added book columns above they will be preserved
  return(dplyr::bind_rows(dfs))
}
