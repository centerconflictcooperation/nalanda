#' Split a book section into chapter files using known headings
#'
#' Splits one oversized section text file into chapter-level `.txt` files by
#' finding the ordered chapter headings supplied from a table of contents. This
#' is useful when uppercase headings alone are ambiguous because non-chapter
#' subsections use the same visual style.
#'
#' @param input_file Character scalar. Path to the section `.txt` file.
#' @param chapter_titles Character vector of chapter titles, in the order they
#'   appear in `input_file`.
#' @param output_dir Character scalar. Folder where chapter files should be
#'   written. Defaults to a `chapters/` subfolder beside `input_file`.
#' @param chapter_ids Optional character vector of file stems to use for output
#'   files. Defaults to numbered slugs based on `chapter_titles`.
#' @param extension Character scalar output extension, without a leading dot.
#'   Defaults to `"txt"`.
#' @param overwrite Logical scalar. If `TRUE`, replace existing output files.
#'   Defaults to `FALSE`.
#' @param include_heading Logical scalar. If `TRUE`, keep each chapter heading
#'   as the first line of its output file. Defaults to `TRUE`.
#' @param allow_missing Logical scalar. If `TRUE`, missing headings are skipped
#'   with a warning. Defaults to `FALSE`.
#'
#' @return A tibble with one row per written chapter and columns for the chapter
#'   title, output file, source line boundaries, and word count.
#' @export
split_book_section_by_headings <- function(input_file,
                                           chapter_titles,
                                           output_dir = file.path(dirname(input_file), "chapters"),
                                           chapter_ids = NULL,
                                           extension = "txt",
                                           overwrite = FALSE,
                                           include_heading = TRUE,
                                           allow_missing = FALSE) {
  if (!is.character(input_file) || length(input_file) != 1 || !nzchar(input_file)) {
    stop("`input_file` must be a single non-empty string.")
  }
  if (!file.exists(input_file)) {
    stop("`input_file` does not exist: ", input_file)
  }
  if (!is.character(chapter_titles) || length(chapter_titles) == 0 || any(!nzchar(chapter_titles))) {
    stop("`chapter_titles` must be a non-empty character vector.")
  }
  if (!is.character(output_dir) || length(output_dir) != 1 || !nzchar(output_dir)) {
    stop("`output_dir` must be a single non-empty string.")
  }
  if (!is.null(chapter_ids) && (!is.character(chapter_ids) || length(chapter_ids) != length(chapter_titles))) {
    stop("`chapter_ids` must be NULL or a character vector the same length as `chapter_titles`.")
  }
  if (!is.character(extension) || length(extension) != 1 || !nzchar(extension)) {
    stop("`extension` must be a single non-empty string.")
  }
  if (!is.logical(overwrite) || length(overwrite) != 1 || is.na(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE.")
  }
  if (!is.logical(include_heading) || length(include_heading) != 1 || is.na(include_heading)) {
    stop("`include_heading` must be TRUE or FALSE.")
  }
  if (!is.logical(allow_missing) || length(allow_missing) != 1 || is.na(allow_missing)) {
    stop("`allow_missing` must be TRUE or FALSE.")
  }

  extension <- sub("^\\.", "", extension)
  lines <- readLines(input_file, warn = FALSE, encoding = "UTF-8")
  line_keys <- normalize_heading_lines(lines)
  title_keys <- normalize_heading_lines(chapter_titles)

  heading_lines <- integer(length(chapter_titles))
  search_start <- 1L
  for (i in seq_along(title_keys)) {
    matches <- which(line_keys == title_keys[[i]] & seq_along(line_keys) >= search_start)
    if (length(matches) == 0) {
      heading_lines[[i]] <- NA_integer_
    } else {
      heading_lines[[i]] <- matches[[1]]
      search_start <- matches[[1]] + 1L
    }
  }

  missing <- is.na(heading_lines)
  if (any(missing)) {
    msg <- paste0(
      "Could not find heading(s): ",
      paste(chapter_titles[missing], collapse = ", ")
    )
    if (!allow_missing) {
      stop(msg, call. = FALSE)
    }
    warning(msg, call. = FALSE)
  }

  found <- tibble::tibble(
    chapter_index = seq_along(chapter_titles),
    chapter_title = chapter_titles,
    heading_line = heading_lines
  ) |>
    dplyr::filter(!is.na(.data$heading_line)) |>
    dplyr::arrange(.data$heading_line) |>
    dplyr::mutate(
      start_line = if (include_heading) .data$heading_line else .data$heading_line + 1L,
      end_line = dplyr::lead(.data$heading_line - 1L, default = length(lines))
    )

  if (nrow(found) == 0) {
    stop("No requested headings were found in `input_file`.", call. = FALSE)
  }

  if (is.null(chapter_ids)) {
    chapter_ids <- paste0(
      sprintf("%02d", seq_along(chapter_titles)),
      "_",
      slugify_chapter_titles(chapter_titles)
    )
  }

  found <- found |>
    dplyr::mutate(
      output_file = paste0(chapter_ids[.data$chapter_index], ".", extension),
      output_path = file.path(output_dir, .data$output_file)
    )

  existing_outputs <- found$output_path[file.exists(found$output_path)]
  if (length(existing_outputs) > 0 && !overwrite) {
    stop(
      paste0(
        "Output file(s) already exist. Set `overwrite = TRUE` to replace them: ",
        paste(basename(existing_outputs), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  chapter_texts <- purrr::map2_chr(found$start_line, found$end_line, \(start, end) {
    text <- paste(lines[start:end], collapse = "\n")
    paste0(trimws(text, which = "right"), "\n")
  })

  purrr::walk2(chapter_texts, found$output_path, readr::write_file)

  found |>
    dplyr::mutate(
      word_count = purrr::map_int(chapter_texts, count_words_in_text)
    ) |>
    dplyr::select(
      "chapter_index",
      "chapter_title",
      "output_file",
      "output_path",
      "heading_line",
      "start_line",
      "end_line",
      "word_count"
    )
}

normalize_heading_lines <- function(x) {
  x |>
    stringr::str_replace_all("[\u2018\u2019]", "'") |>
    stringr::str_squish() |>
    stringr::str_to_lower()
}

slugify_chapter_titles <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_|_$", "") |>
    stringr::str_replace_all("_+", "_")
}

count_words_in_text <- function(x) {
  words <- stringr::str_extract_all(x, "\\b[[:alnum:]']+\\b")[[1]]
  length(words)
}
