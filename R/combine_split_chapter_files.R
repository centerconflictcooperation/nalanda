#' Combine split chapter chunk files
#'
#' Combines text files named with page/range suffixes such as
#' `3_part1-001-050.txt`, `3_part1-051-100.txt`, and
#' `3_part1-101-138.txt` into a single `3_part1.txt` file. Files without a
#' trailing `-start-end` range are copied to `output_dir` when needed.
#'
#' @param input_dir Character scalar. Folder containing chapter `.txt` files.
#' @param output_dir Character scalar. Folder where consolidated files should
#'   be written. Defaults to `input_dir`.
#' @param extension Character scalar file extension to match, without a leading
#'   dot by default. Defaults to `"txt"`.
#' @param separator Character scalar. Text inserted between chunks when
#'   combining them. Defaults to two line breaks.
#' @param overwrite Logical scalar. If `TRUE`, replace existing output files.
#'   Defaults to `FALSE`.
#' @param remove_sources Logical scalar. If `TRUE`, remove split source chunk
#'   files after all outputs are written successfully. Defaults to `FALSE`.
#'
#' @return A tibble with one row per output file and columns describing the
#'   output path, source files, and action taken.
#' @export
combine_split_chapter_files <- function(input_dir,
                                        output_dir = input_dir,
                                        extension = "txt",
                                        separator = "\n\n",
                                        overwrite = FALSE,
                                        remove_sources = FALSE) {
  if (!is.character(input_dir) || length(input_dir) != 1 || !nzchar(input_dir)) {
    stop("`input_dir` must be a single non-empty string.")
  }
  if (!dir.exists(input_dir)) {
    stop("`input_dir` does not exist: ", input_dir)
  }
  if (!is.character(output_dir) || length(output_dir) != 1 || !nzchar(output_dir)) {
    stop("`output_dir` must be a single non-empty string.")
  }
  if (!is.character(extension) || length(extension) != 1 || !nzchar(extension)) {
    stop("`extension` must be a single non-empty string.")
  }
  if (!is.character(separator) || length(separator) != 1 || is.na(separator)) {
    stop("`separator` must be a single string.")
  }
  if (!is.logical(overwrite) || length(overwrite) != 1 || is.na(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE.")
  }
  if (!is.logical(remove_sources) || length(remove_sources) != 1 || is.na(remove_sources)) {
    stop("`remove_sources` must be TRUE or FALSE.")
  }

  extension <- sub("^\\.", "", extension)
  files <- list.files(
    input_dir,
    pattern = paste0("\\.", extension, "$"),
    full.names = TRUE
  )

  if (length(files) == 0) {
    stop("No matching files found in `input_dir` for extension `.", extension, "`.", call. = FALSE)
  }

  parsed <- tibble::tibble(
    input_path = files,
    input_name = basename(files),
    stem = tools::file_path_sans_ext(.data$input_name)
  ) |>
    dplyr::mutate(
      match = stringr::str_match(.data$stem, "^(.*)-(\\d+)-(\\d+)$"),
      output_stem = dplyr::if_else(is.na(.data$match[, 2]), .data$stem, .data$match[, 2]),
      chunk_start = suppressWarnings(as.integer(.data$match[, 3])),
      chunk_end = suppressWarnings(as.integer(.data$match[, 4])),
      is_split_chunk = !is.na(.data$chunk_start) & !is.na(.data$chunk_end),
      output_file = paste0(.data$output_stem, ".", extension),
      output_path = file.path(output_dir, .data$output_file)
    )

  split_stems <- unique(parsed$output_stem[parsed$is_split_chunk])
  plan_inputs <- parsed |>
    dplyr::filter(!(!.data$is_split_chunk & .data$output_stem %in% split_stems))

  plan <- plan_inputs |>
    dplyr::arrange(.data$output_stem, .data$chunk_start, .data$chunk_end, .data$input_name) |>
    dplyr::group_by(.data$output_stem, .data$output_file, .data$output_path) |>
    dplyr::summarise(
      source_files = list(.data$input_path),
      source_names = list(.data$input_name),
      n_files = dplyr::n(),
      is_combined = any(.data$is_split_chunk),
      is_unchanged = dplyr::n() == 1 &&
        !any(.data$is_split_chunk) &&
        normalizePath(.data$input_path[[1]], winslash = "/", mustWork = FALSE) ==
          normalizePath(.data$output_path[[1]], winslash = "/", mustWork = FALSE),
      text = paste(
        purrr::map_chr(.data$input_path, \(path) {
          trimws(readr::read_file(path), which = "right")
        }),
        collapse = separator
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      action = dplyr::case_when(
        .data$is_unchanged ~ "unchanged",
        .data$is_combined ~ "combined",
        TRUE ~ "copied"
      )
    )

  existing_outputs <- plan$output_path[
    file.exists(plan$output_path) & !plan$is_unchanged
  ]
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

  purrr::walk2(
    plan$text[!plan$is_unchanged],
    plan$output_path[!plan$is_unchanged],
    \(text, path) readr::write_file(paste0(text, "\n"), path)
  )

  if (isTRUE(remove_sources)) {
    split_sources <- parsed$input_path[parsed$is_split_chunk]
    removed <- unlink(split_sources, force = TRUE)
    if (!identical(removed, 0L)) {
      warning("Some split source files could not be removed.", call. = FALSE)
    }
  }

  plan |>
    dplyr::transmute(
      output_file = .data$output_file,
      output_path = .data$output_path,
      action = .data$action,
      n_files = .data$n_files,
      source_files = purrr::map_chr(.data$source_names, ~ paste(.x, collapse = ", "))
    ) |>
    dplyr::arrange(.data$output_file)
}
