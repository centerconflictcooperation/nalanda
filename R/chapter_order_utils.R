#' Parse chapter numbers from chapter labels
#'
#' @param chapter Character vector of chapter labels.
#' @return Integer vector of parsed chapter numbers.
#' @keywords internal
parse_chapter_numbers <- function(chapter) {
  suppressWarnings(as.integer(stringr::str_extract(chapter, "\\d+")))
}

validate_book_chapters_present <- function(chapter, book = NULL) {
  if (length(chapter) > 0) {
    return(invisible(chapter))
  }

  location <- if (!is.null(book) && nzchar(book)) {
    paste0(" for book '", book, "'")
  } else {
    ""
  }

  stop(
    "No chapters found in `book_texts`",
    location,
    ". Ensure the book folder contains at least one chapter file.",
    call. = FALSE
  )
}

#' Validate that chapter labels map to unique parsed chapter numbers
#'
#' @param chapter Character vector of chapter labels.
#' @param book Optional character scalar used to identify the source book in
#'   error messages.
#' @param arg_name Character scalar naming the input being validated.
#' @return Integer vector of parsed chapter numbers.
#' @keywords internal
validate_chapter_order <- function(
  chapter,
  book = NULL,
  arg_name = "chapter"
) {
  validate_book_chapters_present(chapter = chapter, book = book)

  chapter_num <- parse_chapter_numbers(chapter)

  if (anyNA(chapter_num)) {
    bad <- unique(chapter[is.na(chapter_num)])
    location <- if (!is.null(book) && nzchar(book)) {
      paste0(" for book '", book, "'")
    } else {
      ""
    }
    stop(
      "Could not extract a chapter number from ",
      arg_name,
      location,
      ": ",
      paste(bad, collapse = ", "),
      ". Rename chapters so each file contains a parseable numeric chapter index.",
      call. = FALSE
    )
  }

  parsed <- tibble::tibble(chapter = chapter, chapter_num = chapter_num) |>
    dplyr::distinct() |>
    dplyr::group_by(.data$chapter_num) |>
    dplyr::summarise(
      chapters = paste(sort(unique(.data$chapter)), collapse = " | "),
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$n > 1L) |>
    dplyr::arrange(.data$chapter_num)

  if (nrow(parsed) > 0) {
    location <- if (!is.null(book) && nzchar(book)) {
      paste0(" for book '", book, "'")
    } else {
      ""
    }
    detail <- paste(
      paste0(parsed$chapter_num, " -> ", parsed$chapters),
      collapse = "; "
    )
    stop(
      "Duplicate chapters identified while parsing ",
      arg_name,
      location,
      ". Multiple chapter labels map to the same parsed chapter number: ",
      detail,
      ". Rename chapters so each file maps to a unique chapter number.",
      call. = FALSE
    )
  }

  chapter_num
}
