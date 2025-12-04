#' Read book chapters into a nested list
#'
#' Convert a list of chapter file paths (as produced by `list_book_chapters()`) into
#' a nested list of chapter texts: list(book -> list(chapter_name -> text)).
#'
#' @param chapter_list A named list of character vectors with file paths.
#' @return A nested list of character scalars (texts) with chapter basenames as names.
#' @export
read_book_texts <- function(chapter_list) {
  lapply(chapter_list, function(chapter_files) {
    texts <- lapply(chapter_files, readr::read_file)
    names(texts) <- basename(chapter_files)
    return(texts)
  })
}
