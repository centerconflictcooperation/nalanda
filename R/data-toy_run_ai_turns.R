#' Toy raw turn-level AI simulation output
#'
#' A synthetic turn-level dataset that mimics the raw output returned by
#' [run_ai_on_chapters()] in per-group mode. It is intended for examples,
#' documentation, and testing the full workflow from raw turns to derived
#' metrics with [compute_run_ai_metrics()].
#'
#' @format A tibble with 128 rows and 11 variables:
#' \describe{
#'   \item{book}{Book title.}
#'   \item{chapter}{Chapter identifier.}
#'   \item{sim}{Simulation index.}
#'   \item{identity}{Simulated respondent identity.}
#'   \item{party}{Political party grouping.}
#'   \item{turn_index}{Conversation turn index.}
#'   \item{turn_type}{Whether the row comes from the `baseline` or `post` turn.}
#'   \item{target_group}{Group being rated on that row.}
#'   \item{rating}{Synthetic rating on a 0-100 scale.}
#'   \item{baseline_prompt}{Stored baseline prompt preview.}
#'   \item{post_prompt}{Stored post prompt preview.}
#' }
#'
#' @source Synthetic example created for package documentation.
"toy_run_ai_turns"
