#' Summarize simulated chapter scores
#'
#' Aggregate simulation results by chapter (and book, if present) computing
#' number of simulations, mean and sd of scores, percent of Republican responses,
#' and retain a chapter excerpt.
#'
#' @param x A data frame or list-like object containing simulation rows with
#'   at least `score` and `chapter` columns. If `book` and `party` are present,
#'   the summary will include those groupings.
#' @return A tibble summarizing each chapter (and book if present). The returned
#'   object will have the original `model` attribute copied to it.
#' @export
summarize_chapter_scores <- function(x) {
  model <- attr(x, "model")

  # helper to robustly flatten various nested formats produced by run_ai_on_chapters
  flatten_sim_results <- function(z) {
    # If already a data.frame, return as-is
    if (inherits(z, "data.frame")) return(z)
    if (!is.list(z)) stop("Unsupported input: expected data.frame or list-like object")

    # Recursively collect all data.frames inside the (possibly nested) list.
    collect_dfs <- function(obj, parent_name = NULL) {
      out <- list()
      if (inherits(obj, "data.frame")) {
        df <- obj
        # if parent name exists and there's no book column, attach it
        if (!is.null(parent_name) && !"book" %in% names(df)) df$book <- parent_name
        return(list(df))
      }
      if (is.list(obj)) {
        nm <- names(obj)
        for (i in seq_along(obj)) {
          child_name <- if (!is.null(nm) && nzchar(nm[i])) nm[i] else parent_name
          out <- c(out, collect_dfs(obj[[i]], parent_name = child_name))
        }
      }
      out
    }

    dfs <- collect_dfs(z, parent_name = NULL)
    if (length(dfs) == 0) stop("Unsupported nested structure for simulation results")
    # bind rows; if we added book columns above they will be preserved
    return(dplyr::bind_rows(dfs))
  }

  df <- if (is.list(x) && !inherits(x, "data.frame")) flatten_sim_results(x) else x

  # Ensure we operate on a plain tibble/data.frame to avoid S3 surprises
  df <- tibble::as_tibble(df)

  # ensure chapter column exists (try to infer common alternatives)
  if (!"chapter" %in% names(df)) {
    candidates <- grep("chap|file|name", names(df), value = TRUE, ignore.case = TRUE)
    candidate <- if (length(candidates) > 0) candidates[1] else NA_character_
    if (!is.na(candidate) && nzchar(candidate)) {
      # only rename if candidate is a simple column name
      df <- dplyr::rename(df, chapter = !!rlang::sym(candidate))
    } else {
      stop("Input data must contain a 'chapter' column (or a close alternative)")
    }
  }

  has_party <- "party" %in% names(df)
  has_book <- "book" %in% names(df)
  has_baseline <- "baseline_score" %in% names(df)

  if (has_party) df <- dplyr::mutate(df, party = tolower(party))

  extract_chapter_num <- function(ch) {
    suppressWarnings(as.integer(stringr::str_extract(ch, "\\d+")))
  }

  if (has_book) {
    df <- df |>
      dplyr::group_by(book, chapter) |>
      dplyr::summarise(
        sim = sum(!is.na(score)),
        mean_baseline = if (has_baseline) mean(baseline_score, na.rm = TRUE) else NA_real_,
        sd_baseline = if (has_baseline) sd(baseline_score, na.rm = TRUE) else NA_real_,
        mean_score = if ("score" %in% names(df)) mean(score, na.rm = TRUE) else NA_real_,
        sd_score = if ("score" %in% names(df)) sd(score, na.rm = TRUE) else NA_real_,
        mean_diff = if (has_baseline && "score" %in% names(df)) mean(score - baseline_score, na.rm = TRUE) else NA_real_,
        sd_diff = if (has_baseline && "score" %in% names(df)) sd(score - baseline_score, na.rm = TRUE) else NA_real_,
        percent_republican = if (has_party) mean(party == "republican", na.rm = TRUE) * 100 else NA_real_,
        chapter_excerpt = dplyr::first(chapter_excerpt),
        .groups = "drop_last"
      ) |>
      dplyr::mutate(chapter_num = extract_chapter_num(chapter)) |>
      dplyr::arrange(book, chapter_num, chapter) |>
      dplyr::group_by(book) |>
      dplyr::mutate(chapter_index = dplyr::row_number()) |>
      dplyr::ungroup() |>
      dplyr::select(-chapter_num)
  } else {
    df <- df |>
      dplyr::group_by(chapter) |>
      dplyr::summarise(
        sim = sum(!is.na(score)),
        mean_baseline = if (has_baseline) mean(baseline_score, na.rm = TRUE) else NA_real_,
        sd_baseline = if (has_baseline) sd(baseline_score, na.rm = TRUE) else NA_real_,
        mean_score = if ("score" %in% names(df)) mean(score, na.rm = TRUE) else NA_real_,
        sd_score = if ("score" %in% names(df)) sd(score, na.rm = TRUE) else NA_real_,
        mean_diff = if (has_baseline && "score" %in% names(df)) mean(score - baseline_score, na.rm = TRUE) else NA_real_,
        sd_diff = if (has_baseline && "score" %in% names(df)) sd(score - baseline_score, na.rm = TRUE) else NA_real_,
        percent_republican = if (has_party) mean(party == "republican", na.rm = TRUE) * 100 else NA_real_,
        chapter_excerpt = dplyr::first(chapter_excerpt),
        .groups = "drop"
      ) |>
      dplyr::mutate(chapter_num = extract_chapter_num(chapter)) |>
      dplyr::arrange(chapter_num, chapter) |>
      dplyr::mutate(chapter_index = dplyr::row_number()) |>
      dplyr::select(-chapter_num)
  }

  attr(df, "model") <- model
  df
}
