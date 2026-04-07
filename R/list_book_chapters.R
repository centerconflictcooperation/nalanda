#' List book chapter files inside a books directory
#'
#' Given either a path to a directory of book folders or a single folder that
#' directly contains chapter files, return a named list where each element is a
#' character vector of chapter file paths (ordered by number or name).
#'
#' @param books_path Character scalar. Path containing subdirectories for each book (default "books").
#' @param extension Character scalar file extension to match, without a leading
#'   dot by default. Defaults to `"txt"`.
#' @return A named list of character vectors of file paths.
#' @export
list_book_chapters <- function(books_path = "books", extension = "txt") {
  if (!is.character(books_path) || length(books_path) != 1 || !nzchar(books_path)) {
    stop("`books_path` must be a single non-empty string.")
  }
  if (!dir.exists(books_path)) {
    stop("`books_path` does not exist: ", books_path)
  }
  if (!is.character(extension) || length(extension) != 1 || !nzchar(extension)) {
    stop("`extension` must be a single non-empty string.")
  }

  extension <- sub("^\\.", "", extension)
  pattern <- paste0("\\.", extension, "$")

  book_dirs <- list.dirs(books_path, full.names = TRUE, recursive = FALSE)
  top_level_files <- list.files(books_path, pattern = pattern, full.names = TRUE)

  if (length(book_dirs) == 0 && length(top_level_files) > 0) {
    result <- list(sort_chapter_files(top_level_files))
    names(result) <- basename(normalizePath(books_path, winslash = "/", mustWork = TRUE))
    return(result)
  }

  result <- lapply(book_dirs, function(book) {
    files <- list.files(book, pattern = pattern, full.names = TRUE)
    if (length(files) == 0) {
      return(files)
    }
    sort_chapter_files(files)
  })
  names(result) <- basename(book_dirs)
  result
}

sort_chapter_files <- function(files) {
  fn <- basename(files)
  core <- tools::file_path_sans_ext(fn)
  num <- suppressWarnings(as.integer(gsub(".*?(\\d+).*", "\\1", core)))
  if (all(is.na(num))) {
    files[order(fn)]
  } else {
    files[order(num, fn, na.last = TRUE)]
  }
}
