#' Toy simulated chapter results
#'
#' A small synthetic dataset that mimics the row-level structure returned by
#' [run_ai_on_chapters()]. It is intended for examples, documentation, and
#' testing plotting and summarization workflows without running live AI
#' simulations. The object also includes `model` and `temperature` attributes
#' so plotting helpers can display realistic metadata in subtitles.
#'
#' @format A tibble with 32 rows and 15 variables:
#' \describe{
#'   \item{book}{Book title.}
#'   \item{chapter}{Chapter identifier.}
#'   \item{sim}{Simulation index.}
#'   \item{identity}{Simulated respondent identity.}
#'   \item{party}{Political party grouping.}
#'   \item{pre_ingroup}{Pre-reading ingroup rating.}
#'   \item{post_ingroup}{Post-reading ingroup rating.}
#'   \item{pre_outgroup}{Pre-reading outgroup rating.}
#'   \item{post_outgroup}{Post-reading outgroup rating.}
#'   \item{pre_gap}{Pre-reading ingroup minus outgroup gap.}
#'   \item{post_gap}{Post-reading ingroup minus outgroup gap.}
#'   \item{delta_ingroup}{Change in ingroup rating.}
#'   \item{delta_outgroup}{Change in outgroup rating.}
#'   \item{delta_gap}{Change in affective polarization gap.}
#'   \item{chapter_excerpt}{Short synthetic chapter excerpt.}
#' }
#'
#' @source Synthetic example created for package documentation.
"toy_sim_results"
