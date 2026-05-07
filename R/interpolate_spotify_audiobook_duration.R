#' Interpolate Spotify audiobook duration from text size
#'
#' Estimate Spotify audiobook durations for new chapters or books from a
#' reference data set where Spotify duration is already known. The predictor can
#' be either text file size in bytes or word count. File size is often the
#' simplest option when all chapters are plain text files created by the same
#' workflow.
#'
#' @param reference Data frame with known Spotify durations and, unless
#'   `books_path` is supplied, a text-size predictor.
#' @param target Data frame with chapters or books to estimate. When
#'   `books_path` and `target_book` are supplied, this can be left as `NULL`.
#' @param duration_col Character scalar. Column in `reference` containing known
#'   Spotify duration.
#' @param books_path Character scalar or `NULL`. Optional folder containing one
#'   subfolder per book, with chapter text files inside each book folder.
#' @param target_book Character vector or `NULL`. Book folder name(s) to
#'   estimate when `books_path` is supplied.
#' @param book_col Character scalar. Book identifier column in `reference`.
#' @param extension Character scalar. File extension to read from `books_path`.
#' @param reference_book_col Character scalar or `NULL`. Optional book identifier
#'   in `reference`. When supplied, the predictor is summed within each book and
#'   `duration_col` must contain one unique duration per book. Use this when
#'   reference rows are chapter-level but Spotify durations are book-level.
#' @param size_col Character scalar or `NULL`. Column containing file sizes in
#'   bytes. Use this when `measure = "file_size"`.
#' @param words_col Character scalar or `NULL`. Column containing word counts.
#'   Use this when `measure = "word_count"`.
#' @param file_col Character scalar or `NULL`. Column containing paths to text
#'   files. If supplied with `measure = "file_size"`, file sizes are computed
#'   with [file.info()]. If supplied with `measure = "word_count"`, words are
#'   counted from the files.
#' @param text_col Character scalar or `NULL`. Column containing text strings to
#'   measure directly.
#' @param measure Character scalar. Either `"file_size"` or `"word_count"`.
#' @param duration_unit Unit of `duration_col`: `"seconds"`, `"minutes"`,
#'   `"hours"`, or `"hms"` for strings like `"6:11:00"`.
#' @param output_unit Unit for the returned estimate column. Use `"hms"` for
#'   spreadsheet-friendly strings like `"5:56:00"`.
#' @param method Estimation method. `"ratio"` fits a single seconds-per-unit
#'   rate through the origin. `"lm"` fits a linear model with an intercept.
#'
#' @return A tibble containing `target` plus `.duration_seconds`, an
#'   `estimated_duration_*` column in `output_unit`, `.duration_measure`, and
#'   `.duration_method`. The total estimated duration is also stored in the
#'   `estimated_total_seconds` and `estimated_total_*` attributes.
#' @export
#'
#' @examples
#' reference <- tibble::tibble(
#'   book = c("A", "B"),
#'   file_size_bytes = c(100000, 150000),
#'   spotify_duration_minutes = c(120, 180)
#' )
#'
#' chapters <- tibble::tibble(
#'   chapter = c("chapter_1", "chapter_2"),
#'   file_size_bytes = c(25000, 50000)
#' )
#'
#' interpolate_spotify_audiobook_duration(
#'   reference,
#'   chapters,
#'   duration_col = "spotify_duration_minutes",
#'   size_col = "file_size_bytes",
#'   duration_unit = "minutes"
#' )
interpolate_spotify_audiobook_duration <- function(
  reference,
  target = NULL,
  duration_col,
  books_path = NULL,
  target_book = NULL,
  book_col = "book",
  extension = "txt",
  reference_book_col = NULL,
  size_col = NULL,
  words_col = NULL,
  file_col = NULL,
  text_col = NULL,
  measure = c("file_size", "word_count"),
  duration_unit = c("seconds", "minutes", "hours", "hms"),
  output_unit = c("minutes", "seconds", "hours", "hms"),
  method = c("ratio", "lm")
) {
  measure <- match.arg(measure)
  duration_unit <- match.arg(duration_unit)
  output_unit <- match.arg(output_unit)
  method <- match.arg(method)

  reference <- tibble::as_tibble(reference)

  if (!is.null(books_path)) {
    prepared <- prepare_audiobook_chapter_inputs(
      reference = reference,
      books_path = books_path,
      target_book = target_book,
      book_col = book_col,
      duration_col = duration_col,
      extension = extension
    )
    reference <- prepared$reference
    target <- prepared$target
    reference_book_col <- book_col
    file_col <- ".chapter_file"
  } else {
    if (is.null(target)) {
      stop("`target` is required unless `books_path` and `target_book` are supplied.")
    }
    target <- tibble::as_tibble(target)
  }

  validate_duration_inputs(
    reference = reference,
    target = target,
    duration_col = duration_col
  )

  ref_measure <- audiobook_duration_measure(
    reference,
    measure = measure,
    size_col = size_col,
    words_col = words_col,
    file_col = file_col,
    text_col = text_col,
    arg_name = "reference"
  )
  target_measure <- audiobook_duration_measure(
    target,
    measure = measure,
    size_col = size_col,
    words_col = words_col,
    file_col = file_col,
    text_col = text_col,
    arg_name = "target"
  )

  ref_duration_seconds <- convert_duration_to_seconds(
    reference[[duration_col]],
    unit = duration_unit
  )

  if (!is.null(reference_book_col)) {
    ref_grouped <- aggregate_reference_books(
      reference = reference,
      measure = ref_measure,
      duration_seconds = ref_duration_seconds,
      reference_book_col = reference_book_col
    )
    ref_measure <- ref_grouped$measure
    ref_duration_seconds <- ref_grouped$duration_seconds
  }

  validate_numeric_vector(ref_measure, "`reference` text-size predictor")
  validate_numeric_vector(target_measure, "`target` text-size predictor")
  validate_numeric_vector(ref_duration_seconds, "`reference` duration")

  if (method == "lm" && length(ref_measure) < 2) {
    stop("`method = \"lm\"` requires at least two reference rows.")
  }

  estimated_seconds <- if (method == "ratio") {
    rate <- sum(ref_duration_seconds) / sum(ref_measure)
    target_measure * rate
  } else {
    fit <- stats::lm(ref_duration_seconds ~ ref_measure)
    as.numeric(stats::predict(
      fit,
      newdata = data.frame(ref_measure = target_measure)
    ))
  }

  estimate_col <- paste0("estimated_duration_", output_unit)
  out <- target
  out$.duration_seconds <- estimated_seconds
  out[[estimate_col]] <- convert_seconds_to_duration(
    estimated_seconds,
    unit = output_unit
  )
  out$.duration_measure <- measure
  out$.duration_method <- method

  attr(out, "estimated_total_seconds") <- sum(estimated_seconds)
  total_value <- if (output_unit == "hms") {
    format_seconds_as_hms(sum(estimated_seconds))
  } else {
    sum(out[[estimate_col]])
  }
  attr(out, paste0("estimated_total_", output_unit)) <- total_value
  out
}

prepare_audiobook_chapter_inputs <- function(
  reference,
  books_path,
  target_book,
  book_col,
  duration_col,
  extension
) {
  if (is.null(target_book) || !is.character(target_book) || length(target_book) == 0) {
    stop("`target_book` must be supplied when `books_path` is used.")
  }
  if (!is.character(book_col) || length(book_col) != 1 || !nzchar(book_col)) {
    stop("`book_col` must be a single non-empty string.")
  }
  if (!book_col %in% names(reference)) {
    stop("`book_col` is not present in `reference`: ", book_col)
  }
  if (!duration_col %in% names(reference)) {
    stop("`duration_col` is not present in `reference`: ", duration_col)
  }

  chapter_list <- list_book_chapters(books_path = books_path, extension = extension)
  chapter_files <- tibble::tibble(
    .book = rep(names(chapter_list), lengths(chapter_list)),
    .chapter_file = unlist(chapter_list, use.names = FALSE)
  )
  names(chapter_files)[names(chapter_files) == ".book"] <- book_col
  chapter_files$.chapter <- basename(chapter_files$.chapter_file)

  missing_books <- setdiff(target_book, unique(chapter_files[[book_col]]))
  if (length(missing_books) > 0) {
    stop(
      "`target_book` was not found under `books_path`: ",
      paste(missing_books, collapse = ", ")
    )
  }

  duration_lookup <- reference[, c(book_col, duration_col), drop = FALSE]
  ref <- merge(chapter_files, duration_lookup, by = book_col, all = FALSE)
  ref <- ref[!ref[[book_col]] %in% target_book, , drop = FALSE]
  ref <- ref[has_duration_value(ref[[duration_col]]), , drop = FALSE]

  target <- chapter_files[
    chapter_files[[book_col]] %in% target_book,
    ,
    drop = FALSE
  ]

  list(
    reference = tibble::as_tibble(ref),
    target = tibble::as_tibble(target)
  )
}

aggregate_reference_books <- function(
  reference,
  measure,
  duration_seconds,
  reference_book_col
) {
  book <- get_required_column(reference, reference_book_col, "reference")
  if (anyNA(book)) {
    stop("`reference_book_col` must not contain missing values.")
  }

  rows <- split(seq_along(book), book)
  grouped_measure <- vapply(rows, function(i) {
    sum(measure[i])
  }, numeric(1))
  grouped_duration <- vapply(rows, function(i) {
    durations <- unique(duration_seconds[i])
    if (length(durations) != 1) {
      stop("`duration_col` must contain one unique duration per `reference_book_col`.")
    }
    durations
  }, numeric(1))

  list(
    measure = unname(grouped_measure),
    duration_seconds = unname(grouped_duration)
  )
}

validate_duration_inputs <- function(reference, target, duration_col) {
  if (!is.character(duration_col) || length(duration_col) != 1 || !nzchar(duration_col)) {
    stop("`duration_col` must be a single non-empty string.")
  }
  if (!duration_col %in% names(reference)) {
    stop("`duration_col` is not present in `reference`: ", duration_col)
  }
  if (nrow(reference) == 0) {
    stop("`reference` must contain at least one row.")
  }
  if (nrow(target) == 0) {
    stop("`target` must contain at least one row.")
  }
}

has_duration_value <- function(x) {
  if (is.character(x)) {
    return(!is.na(x) & nzchar(trimws(x)))
  }
  !is.na(x)
}

audiobook_duration_measure <- function(
  data,
  measure,
  size_col,
  words_col,
  file_col,
  text_col,
  arg_name
) {
  if (measure == "file_size") {
    if (!is.null(size_col)) {
      return(get_required_column(data, size_col, arg_name))
    }
    if (!is.null(file_col)) {
      files <- get_required_column(data, file_col, arg_name)
      return(as.numeric(file.info(files)$size))
    }
    if (!is.null(text_col)) {
      return(nchar(get_required_column(data, text_col, arg_name), type = "bytes"))
    }
    stop("Provide `size_col`, `file_col`, or `text_col` for `measure = \"file_size\"`.")
  }

  if (!is.null(words_col)) {
    return(get_required_column(data, words_col, arg_name))
  }
  if (!is.null(text_col)) {
    return(count_words(get_required_column(data, text_col, arg_name)))
  }
  if (!is.null(file_col)) {
    files <- get_required_column(data, file_col, arg_name)
    text <- vapply(files, readr::read_file, character(1), USE.NAMES = FALSE)
    return(count_words(text))
  }
  stop("Provide `words_col`, `text_col`, or `file_col` for `measure = \"word_count\"`.")
}

get_required_column <- function(data, col, arg_name) {
  if (!is.character(col) || length(col) != 1 || !nzchar(col)) {
    stop("Column arguments must be single non-empty strings.")
  }
  if (!col %in% names(data)) {
    stop("`", col, "` is not present in `", arg_name, "`.")
  }
  data[[col]]
}

validate_numeric_vector <- function(x, label) {
  if (!is.numeric(x)) {
    stop(label, " must be numeric.")
  }
  if (anyNA(x)) {
    stop(label, " must not contain missing values.")
  }
  if (any(!is.finite(x))) {
    stop(label, " must contain only finite values.")
  }
  if (any(x < 0)) {
    stop(label, " must not contain negative values.")
  }
  if (sum(x) <= 0) {
    stop(label, " must contain at least one positive value.")
  }
}

convert_duration_to_seconds <- function(x, unit) {
  if (unit == "hms") {
    return(parse_hms_duration(x))
  }
  multiplier <- switch(
    unit,
    seconds = 1,
    minutes = 60,
    hours = 3600
  )
  as.numeric(x) * multiplier
}

parse_hms_duration <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x) & nzchar(x)

  out[ok] <- vapply(strsplit(x[ok], ":", fixed = TRUE), function(parts) {
    if (length(parts) != 3) {
      return(NA_real_)
    }
    parts <- suppressWarnings(as.numeric(parts))
    if (anyNA(parts)) {
      return(NA_real_)
    }
    parts[1] * 3600 + parts[2] * 60 + parts[3]
  }, numeric(1))

  out
}

convert_seconds_to_duration <- function(x, unit) {
  if (unit == "hms") {
    return(format_seconds_as_hms(x))
  }
  divisor <- switch(
    unit,
    seconds = 1,
    minutes = 60,
    hours = 3600
  )
  x / divisor
}

format_seconds_as_hms <- function(x) {
  x <- round(x)
  hours <- x %/% 3600
  minutes <- (x %% 3600) %/% 60
  seconds <- x %% 60
  sprintf("%d:%02d:%02d", hours, minutes, seconds)
}

count_words <- function(text) {
  matches <- gregexpr("\\b[[:alnum:]']+\\b", text, perl = TRUE)
  vapply(regmatches(text, matches), length, integer(1), USE.NAMES = FALSE)
}
