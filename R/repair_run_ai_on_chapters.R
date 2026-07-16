#' Repair failed chapter simulation units
#'
#' Reads a saved result (or accepts one in memory), reruns retryable failed
#' simulation units, and returns a copy with those units replaced. Permanent
#' Azure content-policy failures are retained without being retried. The source
#' file is never modified; inspect the result and call `saveRDS()` yourself.
#'
#' @param x A result from [run_ai_on_chapters()] or a path to its `.Rds` file.
#' @inheritParams run_ai_on_chapters
#' @param on_error Error policy for the repair attempts.
#' @return A repaired result with the same outer shape as `x`. Attributes
#'   `repaired_units` and `non_retryable_units` identify the units selected for
#'   retry and those retained because of content-policy failures, respectively.
#' @export
repair_run_ai_on_chapters <- function(
  x, book_texts, groups, context_text, question_text,
  output_mode = c("structured", "text"), temperature = NULL, seed = 42,
  model = NULL, integration = getOption("nalanda.integration"),
  virtual_key = getOption("nalanda.virtual_key"),
  base_url = getOption("nalanda.base_url"), excerpt_chars = 200,
  on_error = c("stop", "skip")
) {
  source_path <- NULL
  if (is.character(x) && length(x) == 1L) {
    source_path <- x
    if (!file.exists(x)) stop("Result file does not exist: ", x, call. = FALSE)
    x <- readRDS(x)
  }
  tables <- if (is.data.frame(x)) list(x) else x
  if (!is.list(tables) || !length(tables) ||
      !all(vapply(tables, is.data.frame, logical(1)))) {
    stop("`x` must be a run_ai_on_chapters result or a path to one.", call. = FALSE)
  }
  combined <- dplyr::bind_rows(tables)
  unit_cols <- c("model", "book", "chapter", "sim", "identity")
  make_unit_key <- function(z) do.call(paste, c(z[unit_cols], sep = "\r"))
  required <- c(unit_cols, "rating")
  missing_cols <- setdiff(required, names(combined))
  if (length(missing_cols)) {
    stop("`x` is missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  unit_id <- interaction(combined[unit_cols], drop = TRUE)
  all_na <- stats::ave(is.na(combined$rating), unit_id, FUN = all)
  is_failed <- if ("error" %in% names(combined)) {
    !is.na(combined$error) & combined$error
  } else {
    # Compatibility with older saved results that predate explicit error flags.
    all_na
  }
  failed <- unique(combined[is_failed, unit_cols, drop = FALSE])
  if (!nrow(failed)) {
    message("No failed simulation units found; returning `x` unchanged.")
    return(x)
  }
  if (is.null(model)) model <- unique(failed$model)
  requested_model_labels <- normalize_model_metadata(model)
  failed_model_labels <- vapply(
    failed$model,
    normalize_model_name,
    character(1)
  )
  failed <- failed[
    failed_model_labels %in% requested_model_labels,
    ,
    drop = FALSE
  ]
  if (!nrow(failed)) stop("No failed units match `model`.", call. = FALSE)

  non_retryable_units <- failed[FALSE, , drop = FALSE]
  if ("error_message" %in% names(combined)) {
    selected_failed_rows <- combined[
      is_failed & make_unit_key(combined) %in% make_unit_key(failed),
      ,
      drop = FALSE
    ]
    content_policy_error <- vapply(
      selected_failed_rows$error_message,
      is_content_policy_error_message,
      logical(1)
    )
    non_retryable_units <- unique(
      selected_failed_rows[content_policy_error, unit_cols, drop = FALSE]
    )
  }
  if (nrow(non_retryable_units)) {
    message(
      "Skipping ",
      nrow(non_retryable_units),
      " non-retryable Azure content-policy failed simulation unit(s)."
    )
    failed <- failed[
      !make_unit_key(failed) %in% make_unit_key(non_retryable_units),
      ,
      drop = FALSE
    ]
  }
  if (!nrow(failed)) {
    message("No retryable failed simulation units found; returning `x` unchanged.")
    attr(x, "repair_source") <- source_path
    attr(x, "repaired_units") <- failed
    attr(x, "non_retryable_units") <- non_retryable_units
    return(x)
  }
  if (is.null(temperature)) {
    temperature <- attr(x, "temperature")
    if (is.null(temperature)) temperature <- attr(tables[[1]], "temperature")
    if (is.null(temperature)) temperature <- 0
  }

  jobs <- build_chapter_jobs(book_texts)
  available <- paste(jobs$book, jobs$chapter, sep = "\r")
  needed <- paste(failed$book, failed$chapter, sep = "\r")
  if (any(!needed %in% available)) {
    stop("`book_texts` does not contain every failed book/chapter unit.", call. = FALSE)
  }
  if (is.list(book_texts) && !is_flat_text_list(book_texts)) {
    book_texts <- book_texts[names(book_texts) %in% unique(failed$book)]
  }

  repaired <- run_ai_on_chapters(
    book_texts = book_texts, groups = groups, context_text = context_text,
    question_text = question_text, output_mode = output_mode,
    n_simulations = max(failed$sim), temperature = temperature, seed = seed,
    model = model, integration = integration, virtual_key = virtual_key,
    base_url = base_url, excerpt_chars = excerpt_chars, on_error = on_error,
    .repair_units = failed
  )
  repair_tables <- if (is.data.frame(repaired)) list(repaired) else unclass(repaired)
  repair_rows <- dplyr::bind_rows(repair_tables)
  merged <- combined[
    !make_unit_key(combined) %in% make_unit_key(repair_rows),
    ,
    drop = FALSE
  ]
  merged <- dplyr::bind_rows(merged, repair_rows)
  order_cols <- intersect(c("book", "chapter", "sim", "identity", "turn_index", "target_group"), names(merged))
  merged <- merged[do.call(order, merged[order_cols]), , drop = FALSE]

  restore <- function(tbl, original) {
    tbl <- tibble::as_tibble(tbl)
    class(tbl) <- unique(c("nalanda", class(tbl)))
    for (nm in c("model", "models", "temperature", "n_simulations", "chapter_excerpts")) {
      attr(tbl, nm) <- attr(original, nm)
    }
    tbl
  }
  if (is.data.frame(x)) {
    out <- restore(merged, x)
  } else {
    out <- lapply(names(tables), function(nm) restore(merged[merged$book == nm, , drop = FALSE], tables[[nm]]))
    names(out) <- names(tables)
    class(out) <- class(x)
    for (nm in c("model", "models", "temperature", "n_simulations")) attr(out, nm) <- attr(x, nm)
  }
  attr(out, "repair_source") <- source_path
  attr(out, "repaired_units") <- failed
  attr(out, "non_retryable_units") <- non_retryable_units
  out
}

is_content_policy_error_message <- function(message) {
  if (length(message) != 1L || is.na(message) || !nzchar(message)) {
    return(FALSE)
  }

  message <- tolower(message)
  patterns <- c(
    "content management policy",
    "responsibleaipolicyviolation",
    "content_filter"
  )
  any(vapply(
    patterns,
    grepl,
    logical(1),
    x = message,
    fixed = TRUE
  ))
}
