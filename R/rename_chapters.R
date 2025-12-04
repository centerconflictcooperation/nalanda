#' Rename chapter text files in a folder to a sequential order
#'
#' Scans a folder for .txt files and renames them to chapter1.txt, chapter2.txt, ...
#' using heuristics for ordering (intro, part 1/2, numeric chapter numbers, appendix, etc.).
#'
#' @param folder Character scalar. Path to the folder containing .txt files.
#' @return A tibble with columns `old_path`, `base`, `order_score`, `new_name`, and `new_path`.
#' @export
rename_chapters <- function(folder) {
  files <- list.files(folder, pattern = "\\.txt$", full.names = TRUE)
  basenames <- basename(files)
  # Extract chapter numbers if present
  chapter_nums <- str_extract(basenames, "\\d+")
  chapter_nums <- suppressWarnings(as.integer(chapter_nums))

  # Create an ordering score (lower = earlier)
  order_score <- case_when(
    str_detect(basenames, regex("intro|introduction", ignore_case = TRUE)) ~ -2,
    str_detect(basenames, regex("part\\s*1", ignore_case = TRUE)) ~ -1,
    str_detect(basenames, regex("part\\s*2", ignore_case = TRUE)) ~ 0,
    !is.na(chapter_nums) ~ chapter_nums,
    str_detect(basenames, regex("appendix", ignore_case = TRUE)) ~ 1001,
    str_detect(basenames, regex("notes", ignore_case = TRUE)) ~ 1002,
    str_detect(basenames, regex("index", ignore_case = TRUE)) ~ 1003,
    str_detect(basenames, regex("bibliography", ignore_case = TRUE)) ~ 1004,
    TRUE ~ 9999
  )

  df <- tibble::tibble(
    old_path = files,
    base = basenames,
    order_score = order_score
  ) |>
    dplyr::arrange(order_score)

  # Create new filenames
  df <- df |>
    dplyr::mutate(
      new_name = paste0("chapter", row_number(), ".txt"),
      new_path = file.path(folder, new_name)
    )

  # Actually rename
  purrr::walk2(df$old_path, df$new_path, file.rename)

  return(df)
}
