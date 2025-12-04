#' @importFrom stats sd
#' @importFrom dplyr case_when row_number
#' @importFrom stringr str_extract
#' @importFrom rlang .data
NULL

# Declare global variables used in NSE contexts to avoid R CMD check warnings
utils::globalVariables(c(
    # Column names used in dplyr pipelines
    "baseline_score",
    "book",
    "case",
    "chapter",
    "chapter_excerpt",
    "chapter_index",
    "chapter_num",
    "difference_score",
    "mean_score",
    "new_name",
    "party",
    "score",
    "sd_score",
    "sim",
    "sim_unique_id",
    "time_var"
))
