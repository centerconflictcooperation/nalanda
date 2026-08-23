#' Renumber chapter files across ordered folders
#'
#' Renumbers chapter files across multiple folders while preserving each file's
#' title slug. This is useful after splitting a book by part, where each part
#' folder starts at `01_...` but the full book needs one chronological sequence.
#'
#' @param folders Character vector of folders, in chronological order.
#' @param extension Character scalar file extension to match, without a leading
#'   dot by default. Defaults to `"txt"`.
#' @param start Integer scalar. First chapter number to use. Defaults to `1`.
#' @param width Integer scalar. Minimum zero-padding width. Defaults to `2`.
#' @param dry_run Logical scalar. If `TRUE`, return the renaming plan without
#'   changing files. Defaults to `FALSE`.
#'
#' @return A tibble with one row per chapter file and columns describing the old
#'   and new file paths.
#' @export
renumber_chapters_across_folders <- function(folders,
                                             extension = "txt",
                                             start = 1L,
                                             width = 2L,
                                             dry_run = FALSE) {
  if (!is.character(folders) || length(folders) == 0 || any(!nzchar(folders))) {
    stop("`folders` must be a non-empty character vector.")
  }
  missing_folders <- folders[!dir.exists(folders)]
  if (length(missing_folders) > 0) {
    stop(
      "`folders` contains path(s) that do not exist: ",
      paste(missing_folders, collapse = ", ")
    )
  }
  if (!is.character(extension) || length(extension) != 1 || !nzchar(extension)) {
    stop("`extension` must be a single non-empty string.")
  }
  if (!is.numeric(start) || length(start) != 1 || is.na(start) || start < 1) {
    stop("`start` must be a positive integer.")
  }
  if (!is.numeric(width) || length(width) != 1 || is.na(width) || width < 1) {
    stop("`width` must be a positive integer.")
  }
  if (!is.logical(dry_run) || length(dry_run) != 1 || is.na(dry_run)) {
    stop("`dry_run` must be TRUE or FALSE.")
  }

  start <- as.integer(start)
  width <- as.integer(width)
  extension <- sub("^\\.", "", extension)
  pattern <- paste0("\\.", extension, "$")

  plan <- purrr::map2_dfr(folders, seq_along(folders), \(folder, folder_index) {
    files <- list.files(folder, pattern = pattern, full.names = TRUE)
    if (length(files) == 0) {
      return(tibble::tibble())
    }

    files <- sort_chapter_files(files)
    tibble::tibble(
      folder_index = folder_index,
      folder = folder,
      old_path = files,
      old_name = basename(files),
      stem = tools::file_path_sans_ext(.data$old_name)
    )
  })

  if (nrow(plan) == 0) {
    stop("No matching files found in `folders` for extension `.", extension, "`.", call. = FALSE)
  }

  total_max <- start + nrow(plan) - 1L
  width <- max(width, nchar(as.character(total_max)))

  plan <- plan |>
    dplyr::mutate(
      chapter_number = start + dplyr::row_number() - 1L,
      title_slug = stringr::str_replace(.data$stem, "^\\d+[_ -]*", ""),
      title_slug = dplyr::if_else(nzchar(.data$title_slug), .data$title_slug, .data$stem),
      new_name = paste0(
        stringr::str_pad(.data$chapter_number, width = width, pad = "0"),
        "_",
        .data$title_slug,
        ".",
        extension
      ),
      new_path = file.path(.data$folder, .data$new_name),
      temp_name = paste0(
        ".nalanda_tmp_",
        seq_len(dplyr::n()),
        "_",
        .data$old_name
      ),
      temp_path = file.path(.data$folder, .data$temp_name)
    )

  duplicate_targets <- plan$new_path[duplicated(normalizePath(plan$new_path, winslash = "/", mustWork = FALSE))]
  if (length(duplicate_targets) > 0) {
    stop(
      "Renaming plan contains duplicate target path(s): ",
      paste(unique(duplicate_targets), collapse = ", "),
      call. = FALSE
    )
  }

  if (!dry_run) {
    first_pass <- purrr::map2_lgl(plan$old_path, plan$temp_path, file.rename)
    if (!all(first_pass)) {
      stop("Failed while creating temporary filenames during rename.", call. = FALSE)
    }

    second_pass <- purrr::map2_lgl(plan$temp_path, plan$new_path, file.rename)
    if (!all(second_pass)) {
      stop("Failed while applying final filenames during rename.", call. = FALSE)
    }
  }

  plan |>
    dplyr::select(
      "folder_index",
      "folder",
      "chapter_number",
      "old_name",
      "new_name",
      "old_path",
      "new_path"
    )
}
