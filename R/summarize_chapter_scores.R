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
  # model <- attr(x[[1]][[1]], "model")
  model <- attr(x, "model")
  df <- if (is.list(x) && !inherits(x, "data.frame")) {
    dplyr::bind_rows(unlist(x, recursive = FALSE))
  } else {
    x
  }
  # df <- if (is.list(x) && !inherits(x, "data.frame")) dplyr::bind_rows(x) else x
  has_party <- "party" %in% names(df)
  has_book <- "book" %in% names(df)
  if (has_party) {
    df <- df |> dplyr::mutate(party = tolower(party))
  }
  extract_chapter_num <- function(ch) {
    suppressWarnings(as.integer(stringr::str_extract(ch, "\\d+")))
  }
  if (has_book) {
    df <- df |>
      dplyr::group_by(book, chapter) |>
      dplyr::summarise(
        sim = sum(!is.na(score)),
        mean_score = mean(score, na.rm = TRUE),
        sd_score = sd(score, na.rm = TRUE),
        percent_republican = if (has_party) {
          mean(party == "republican", na.rm = TRUE) * 100
        } else {
          NA_real_
        },
        chapter_excerpt = dplyr::first(chapter_excerpt),
        .groups = "drop_last"
      ) |>
      dplyr::mutate(chapter_num = extract_chapter_num(chapter)) |>
      dplyr::arrange(book, chapter_num, chapter) |>
      dplyr::group_by(book) |>
      dplyr::mutate(chapter_index = row_number()) |>
      dplyr::ungroup() |>
      dplyr::select(-chapter_num)
  } else {
    df <- df |>
      dplyr::group_by(chapter) |>
      dplyr::summarise(
        sim = sum(!is.na(score)),
        mean_score = mean(score, na.rm = TRUE),
        sd_score = sd(score, na.rm = TRUE),
        percent_republican = if (has_party) {
          mean(party == "republican", na.rm = TRUE) * 100
        } else {
          NA_real_
        },
        chapter_excerpt = dplyr::first(chapter_excerpt),
        .groups = "drop"
      ) |>
      dplyr::mutate(chapter_num = extract_chapter_num(chapter)) |>
      dplyr::arrange(chapter_num, chapter) |>
      dplyr::mutate(chapter_index = row_number()) |>
      dplyr::select(-chapter_num)
  }
  attr(df, "model") <- model
  df
}
