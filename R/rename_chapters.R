#' Rename chapter text files in a folder to a sequential order
#'
#' Scans a folder for chapter files and renames them to `chapter1.ext`,
#' `chapter2.ext`, ...
#' using heuristics for ordering (intro, part 1/2, numeric chapter numbers, appendix, etc.).
#'
#' @param folder Character scalar. Path to the folder containing chapter files.
#' @param extension Character scalar file extension to match, without a leading
#'   dot by default. Defaults to `"txt"`.
#' @return A tibble with columns `old_path`, `base`, `order_score`, `new_name`, and `new_path`.
#' @export
rename_chapters <- function(folder, extension = "txt") {
  if (!is.character(folder) || length(folder) != 1 || !nzchar(folder)) {
    stop("`folder` must be a single non-empty string.")
  }
  if (!dir.exists(folder)) {
    stop("`folder` does not exist: ", folder)
  }
  if (!is.character(extension) || length(extension) != 1 || !nzchar(extension)) {
    stop("`extension` must be a single non-empty string.")
  }

  extension <- sub("^\\.", "", extension)
  pattern <- paste0("\\.", extension, "$")
  files <- list.files(folder, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    stop("No matching files found in `folder` for extension `.", extension, "`.", call. = FALSE)
  }

  basenames <- basename(files)
  # Extract chapter numbers if present
  chapter_nums <- stringr::str_extract(basenames, "\\d+")
  chapter_nums <- suppressWarnings(as.integer(chapter_nums))

  # Create an ordering score (lower = earlier)
  order_score <- dplyr::case_when(
    stringr::str_detect(basenames, stringr::regex("intro|introduction", ignore_case = TRUE)) ~ -2,
    stringr::str_detect(basenames, stringr::regex("part\\s*1", ignore_case = TRUE)) ~ -1,
    stringr::str_detect(basenames, stringr::regex("part\\s*2", ignore_case = TRUE)) ~ 0,
    !is.na(chapter_nums) ~ chapter_nums,
    stringr::str_detect(basenames, stringr::regex("appendix", ignore_case = TRUE)) ~ 1001,
    stringr::str_detect(basenames, stringr::regex("notes", ignore_case = TRUE)) ~ 1002,
    stringr::str_detect(basenames, stringr::regex("index", ignore_case = TRUE)) ~ 1003,
    stringr::str_detect(basenames, stringr::regex("bibliography", ignore_case = TRUE)) ~ 1004,
    TRUE ~ 9999
  )

  df <- tibble::tibble(
    old_path = files,
    base = basenames,
    order_score = order_score
  ) |>
    dplyr::arrange(.data$order_score)

  # Create new filenames
  df <- df |>
    dplyr::mutate(
      new_name = paste0("chapter", dplyr::row_number(), ".", extension),
      new_path = file.path(folder, .data$new_name),
      temp_name = paste0(
        ".nalanda_tmp_",
        seq_len(dplyr::n()),
        "_",
        basename(.data$old_path)
      ),
      temp_path = file.path(folder, .data$temp_name)
    )

  # Rename in two phases to avoid collisions like `chapter11.pdf` -> `chapter1.pdf`
  first_pass <- purrr::map2_lgl(df$old_path, df$temp_path, file.rename)
  if (!all(first_pass)) {
    stop("Failed while creating temporary filenames during rename.", call. = FALSE)
  }

  second_pass <- purrr::map2_lgl(df$temp_path, df$new_path, file.rename)
  if (!all(second_pass)) {
    stop("Failed while applying final filenames during rename.", call. = FALSE)
  }

  df <- df |>
    dplyr::select(-"temp_name", -"temp_path")

  return(df)
}




