#' @importFrom rlang .data
NULL

interaction_key <- function(df, cols) {
  if (length(cols) == 0) {
    stop("`cols` must contain at least one column name.")
  }

  key_df <- df[, cols, drop = FALSE]
  key_args <- lapply(as.list(key_df), function(col) {
    out <- as.character(col)
    out[is.na(out)] <- "<NA>"
    out
  })
  key_args <- unname(key_args)
  names(key_args) <- NULL

  do.call(
    interaction,
    c(key_args, list(drop = TRUE, lex.order = TRUE))
  )
}
