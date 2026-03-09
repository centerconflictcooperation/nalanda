#' @importFrom stats sd
#' @importFrom dplyr case_when row_number
#' @importFrom stringr str_extract
#' @importFrom rlang .data
NULL

# Declare global variables used in NSE contexts to avoid R CMD check warnings
utils::globalVariables(c(
    # Column names used in dplyr pipelines
    "book",
    "case",
    "chapter",
    "chapter_excerpt",
    "baseline_prompt",
    "post_prompt",
    "chapter_index",
    "chapter_num",
    "delta_gap",
    "delta_ingroup",
    "delta_outgroup",
    "gap_pre",
    "gap_post",
    "identity",
    "new_name",
    "party",
    "pre_ingroup",
    "post_ingroup",
    "pre_outgroup",
    "post_outgroup",
    "pre_rating",
    "post_rating",
    "score",
    "sim",
    "sim_unique_id",
    "time_var"
))
