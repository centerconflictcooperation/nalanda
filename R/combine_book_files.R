#' Combine chapter text files into one numbered file per book
#'
#' Takes chapter files named like `1_howcanyou.txt` and `2_howcanyou.txt`,
#' groups them by the shared title stem after the underscore, orders chapters
#' by their numeric prefix, and writes one combined `.txt` file per group
#' using filenames like `1_howcanyou.txt`.
#'
#' @param input_dir Character scalar. Folder containing chapter `.txt` files.
#' @param output_dir Character scalar. Folder where combined `.txt` files should
#'   be written. Defaults to a `combined/` subfolder inside `input_dir`.
#' @param separator Character scalar. Text inserted between chapters when
#'   combining them. Defaults to two line breaks.
#' @param overwrite Logical scalar. If `TRUE`, replace existing output files.
#'   Defaults to `FALSE`.
#' @return A tibble with one row per combined book and columns describing the
#'   numeric output file, original title stem, and source chapter numbers.
#' @export
combine_book_files <- function(input_dir,
                               output_dir = file.path(input_dir, "combined"),
                               separator = "\n\n",
                               overwrite = FALSE) {
  files <- list.files(input_dir, pattern = "\\.txt$", full.names = TRUE)

  if (length(files) == 0) {
    stop("No `.txt` files found in `input_dir`.", call. = FALSE)
  }

  parsed <- tibble::tibble(
    input_path = files,
    input_name = basename(files),
    stem = tools::file_path_sans_ext(.data$input_name)
  ) |>
    tidyr::extract(
      col = "stem",
      into = c("chapter_number", "book_slug"),
      regex = "^(\\d+)_(.+)$",
      remove = FALSE
    ) |>
    dplyr::mutate(chapter_number = as.integer(.data$chapter_number))

  bad_files <- parsed |>
    dplyr::filter(is.na(.data$chapter_number) | is.na(.data$book_slug))

  if (nrow(bad_files) > 0) {
    stop(
      paste0(
        "These files do not match the expected `number_title.txt` format: ",
        paste(bad_files$input_name, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  books <- parsed |>
    dplyr::arrange(.data$chapter_number, .data$book_slug, .data$input_name) |>
    dplyr::group_by(.data$book_slug) |>
    dplyr::summarise(
      first_chapter = min(.data$chapter_number),
      chapter_numbers = list(.data$chapter_number),
      source_files = list(.data$input_path),
      text = paste(
        purrr::map_chr(.data$input_path, \(path) {
          trimws(readr::read_file(path), which = "right")
        }),
        collapse = separator
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$first_chapter, .data$book_slug) |>
    dplyr::mutate(
      book_id = dplyr::row_number(),
      output_file = paste0(.data$book_id, "_", .data$book_slug, ".txt"),
      output_path = file.path(output_dir, .data$output_file)
    )

  existing_outputs <- books$output_path[file.exists(books$output_path)]
  if (length(existing_outputs) > 0 && !overwrite) {
    stop(
      paste0(
        "Output file(s) already exist. Set `overwrite = TRUE` to replace them: ",
        paste(basename(existing_outputs), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  purrr::walk2(
    books$text,
    books$output_path,
    \(text, path) readr::write_file(paste0(text, "\n"), path)
  )

  mapping <- books |>
    dplyr::transmute(
      book_id = .data$book_id,
      book_slug = .data$book_slug,
      first_chapter = .data$first_chapter,
      n_chapters = purrr::map_int(.data$chapter_numbers, length),
      chapter_numbers = purrr::map_chr(.data$chapter_numbers, ~ paste(.x, collapse = ", ")),
      output_file = .data$output_file
    )

  mapping
}
