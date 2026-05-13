#' Read book chapters into a nested list
#'
#' Convert a list of chapter file paths (as produced by `list_book_chapters()`) into
#' a nested list of chapter texts: list(book -> list(chapter_name -> text)).
#'
#' @param chapter_list A named list of character vectors with file paths.
#' @return A nested list of character scalars (texts) with chapter basenames as
#'   names. Each book element also stores its book name in a `book` attribute so
#'   selecting a single book with `$` preserves enough metadata for simulation
#'   helpers.
#' @export
read_book_texts <- function(chapter_list) {
  book_names <- names(chapter_list)
  out <- lapply(seq_along(chapter_list), function(i) {
    chapter_files <- chapter_list[[i]]
    texts <- lapply(chapter_files, readr::read_file)
    names(texts) <- basename(chapter_files)
    if (!is.null(book_names) && nzchar(book_names[[i]])) {
      attr(texts, "book") <- book_names[[i]]
    }
    texts
  })
  names(out) <- book_names
  out
}
