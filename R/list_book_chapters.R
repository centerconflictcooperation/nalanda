#' List book chapter files inside a books directory
#'
#' Given a path to a directory of book folders, return a named list where each
#' element is a character vector of chapter file paths (ordered by number or name).
#'
#' @param books_path Character scalar. Path containing subdirectories for each book (default "books").
#' @return A named list of character vectors of file paths.
#' @export
list_book_chapters <- function(books_path = "books") {
  book_dirs <- list.dirs(books_path, full.names = TRUE, recursive = FALSE)
  result <- lapply(book_dirs, function(book) {
    files <- list.files(book, pattern = "\\.txt$", full.names = TRUE)
    if (length(files) == 0) {
      return(files)
    }
    fn <- basename(files)
    core <- tools::file_path_sans_ext(fn)
    num <- suppressWarnings(as.integer(gsub(".*?(\\d+).*", "\\1", core)))
    if (all(is.na(num))) {
      files[order(fn)]
    } else {
      files[order(num, fn, na.last = TRUE)]
    }
  })
  names(result) <- basename(book_dirs)
  result
}
