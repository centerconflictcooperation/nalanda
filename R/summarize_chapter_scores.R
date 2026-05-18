#' Summarize simulated chapter scores
#'
#' Aggregate simulation results by chapter (and book, if present) computing
#' number of simulations, means and SDs for core model outputs. In the current
#' schema, this includes ingroup/outgroup pre-post ratings plus delta and gap
#' metrics (for example delta_outgroup, delta_ingroup, and delta_gap).
#'
#' @param x A data frame or list-like object containing simulation rows as
#'   produced by run_ai_on_chapters(). Expected columns include chapter,
#'   pre/post ingroup-outgroup fields, and the derived difference columns used
#'   in summaries (pre_gap, post_gap, delta_outgroup, delta_ingroup, delta_gap).
#'   If book and party are present, the summary will include those groupings.
#' @param aggregate_level Character. One of "chapter" (default) or "book".
#'   When "book", results are aggregated to the book level.
#' @param book_chapter_strategy Character. One of `"all"` (default) or
#'   `"last"`. Used only when `aggregate_level = "book"`. `"all"` averages
#'   across all chapter rows, while `"last"` first keeps the last non-missing
#'   cumulative chapter row per book, simulation, identity, model, and party.
#'   If `chapter_index` is unavailable, chapter order is inferred from the
#'   `chapter` labels.
#' @param standardize Character. How to standardize metric columns before
#'   summarizing. `"none"` (default) keeps raw scores; `"z"` centers and scales
#'   scores within model; `"minmax"` rescales scores within model to 0--1;
#'   `"max"` divides scores within model by that model's maximum absolute score.
#' @param model_aggregation Character. `"none"` (default) keeps separate rows
#'   per model when a `model` column is present. `"mean"` averages the
#'   per-model summary estimates into one consensus row per book/chapter/party
#'   grouping, adds `n_models`, and adds `sd_model_*` columns measuring
#'   disagreement across model-level mean estimates.
#' @param by_party Logical. If TRUE, summaries are computed separately by party
#'   (if present).
#' @return A tibble summarizing each chapter (and book if present). The returned
#'   object will have the original model attribute copied to it.
#'
#' @examples
#' chapter_summary <- summarize_chapter_scores(toy_sim_results)
#' chapter_summary
#'
#' book_summary <- summarize_chapter_scores(
#'   toy_sim_results,
#'   aggregate_level = "book"
#' )
#' book_summary
#'
#' party_summary <- summarize_chapter_scores(
#'   toy_sim_results,
#'   by_party = TRUE
#' )
#' head(party_summary)
#' @export
summarize_chapter_scores <- function(
  x,
  aggregate_level = c("chapter", "book"),
  book_chapter_strategy = c("all", "last"),
  standardize = c("none", "z", "minmax", "max"),
  model_aggregation = c("none", "mean"),
  by_party = FALSE
) {
  aggregate_level <- match.arg(aggregate_level)
  book_chapter_strategy <- match.arg(book_chapter_strategy)
  standardize <- match.arg(standardize)
  model_aggregation <- match.arg(model_aggregation)
  model <- attr(x, "model")
  models <- attr(x, "models")
  temperature <- attr(x, "temperature")
  n_simulations <- attr(x, "n_simulations")
  chapter_excerpts <- chapter_excerpt_index(x)

  # Fallback: check first list element (covers lapply(files, readRDS) workflow)
  if (is.null(model) && is.list(x) && !inherits(x, "data.frame") &&
    length(x) > 0) {
    model <- rlang::`%||%`(model, attr(x[[1]], "model"))
    temperature <- rlang::`%||%`(temperature, attr(x[[1]], "temperature"))
    n_simulations <- rlang::`%||%`(n_simulations, attr(x[[1]], "n_simulations"))
  }
  model <- normalize_model_name(model)
  models <- normalize_model_metadata(models)

  df <- if (is.list(x) && !inherits(x, "data.frame")) {
    flatten_sim_results(x)
  } else {
    x
  }

  # Ensure we operate on a plain tibble/data.frame to avoid S3 surprises
  df <- tibble::as_tibble(df)

  # ensure chapter column exists (try to infer common alternatives)
  if (!"chapter" %in% names(df)) {
    candidates <- grep(
      "chap|file|name",
      names(df),
      value = TRUE,
      ignore.case = TRUE
    )
    candidate <- if (length(candidates) > 0) candidates[1] else NA_character_
    if (!is.na(candidate) && nzchar(candidate)) {
      df <- dplyr::rename(df, chapter = !!rlang::sym(candidate))
    } else {
      stop(
        "Input data must contain a 'chapter' column (or a close alternative)"
      )
    }
  }

  has_party <- "party" %in% names(df)
  has_book <- "book" %in% names(df)
  has_identity <- "identity" %in% names(df)

  extract_chapter_num <- function(ch) {
    suppressWarnings(as.integer(stringr::str_extract(ch, "\\d+")))
  }

  if (book_chapter_strategy == "last") {
    if (aggregate_level != "book") {
      stop("`book_chapter_strategy = \"last\"` requires `aggregate_level = \"book\"`.")
    }

    required_last_cols <- c("book", "sim", "identity")
    missing_last_cols <- setdiff(required_last_cols, names(df))
    if (length(missing_last_cols) > 0) {
      stop(
        "`book_chapter_strategy = \"last\"` requires columns: ",
        paste(required_last_cols, collapse = ", "),
        ". Missing: ",
        paste(missing_last_cols, collapse = ", "),
        "."
      )
    }
    if (!"chapter_index" %in% names(df)) {
      df <- df |>
        dplyr::mutate(chapter_index = extract_chapter_num(.data$chapter))
      if (!any(!is.na(df$chapter_index))) {
        stop(
          "`book_chapter_strategy = \"last\"` requires `chapter_index` or ",
          "chapter labels containing chapter numbers."
        )
      }
    }

    metric_col <- if ("delta_gap" %in% names(df)) {
      "delta_gap"
    } else if ("delta_outgroup" %in% names(df)) {
      "delta_outgroup"
    } else {
      stop(
        "`book_chapter_strategy = \"last\"` requires either `delta_gap` ",
        "or `delta_outgroup` to identify non-missing cumulative metric rows."
      )
    }

    last_group_cols <- c(
      "book",
      "sim",
      "identity",
      intersect(c("model", "party"), names(df))
    )

    df <- df |>
      dplyr::filter(!is.na(.data[[metric_col]])) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(last_group_cols))) |>
      dplyr::slice_max(.data$chapter_index, n = 1, with_ties = FALSE) |>
      dplyr::ungroup()
  }

  metric_cols <- intersect(
    c(
      "pre_ingroup",
      "post_ingroup",
      "pre_outgroup",
      "post_outgroup",
      "pre_gap",
      "post_gap",
      "delta_outgroup",
      "delta_ingroup",
      "delta_gap"
    ),
    names(df)
  )
  if (standardize != "none") {
    if (!"model" %in% names(df)) {
      stop("`standardize` requires a `model` column.", call. = FALSE)
    }

    df <- df |>
      dplyr::group_by(.data$model) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(metric_cols),
          ~ standardize_top_unit_vector(.x, standardize)
        )
      ) |>
      dplyr::ungroup()
  }

  # --- Determine grouping columns ---
  group_cols <- c()
  if ("model" %in% names(df)) group_cols <- c(group_cols, "model")
  if (has_book) group_cols <- c(group_cols, "book")
  if (aggregate_level == "chapter") group_cols <- c(group_cols, "chapter")
  if (by_party) {
    if (has_party) {
      group_cols <- c(group_cols, "party")
    } else if (has_identity) {
      group_cols <- c(group_cols, "identity")
    }
  }

  # --- Summarise ---
  df <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      sim = dplyr::n(),
      mean_pre_ingroup = mean(.data$pre_ingroup, na.rm = TRUE),
      sd_pre_ingroup = stats::sd(.data$pre_ingroup, na.rm = TRUE),
      mean_post_ingroup = mean(.data$post_ingroup, na.rm = TRUE),
      sd_post_ingroup = stats::sd(.data$post_ingroup, na.rm = TRUE),
      mean_pre_outgroup = mean(.data$pre_outgroup, na.rm = TRUE),
      sd_pre_outgroup = stats::sd(.data$pre_outgroup, na.rm = TRUE),
      mean_post_outgroup = mean(.data$post_outgroup, na.rm = TRUE),
      sd_post_outgroup = stats::sd(.data$post_outgroup, na.rm = TRUE),
      mean_pre_gap = mean(.data$pre_gap, na.rm = TRUE),
      sd_pre_gap = stats::sd(.data$pre_gap, na.rm = TRUE),
      mean_post_gap = mean(.data$post_gap, na.rm = TRUE),
      sd_post_gap = stats::sd(.data$post_gap, na.rm = TRUE),
      mean_delta_outgroup = mean(.data$delta_outgroup, na.rm = TRUE),
      sd_delta_outgroup = stats::sd(.data$delta_outgroup, na.rm = TRUE),
      mean_delta_ingroup = mean(.data$delta_ingroup, na.rm = TRUE),
      sd_delta_ingroup = stats::sd(.data$delta_ingroup, na.rm = TRUE),
      mean_delta_gap = mean(.data$delta_gap, na.rm = TRUE),
      sd_delta_gap = stats::sd(.data$delta_gap, na.rm = TRUE),
      .groups = "drop"
    )

  if (model_aggregation == "mean" && "model" %in% names(df)) {
    model_group_cols <- setdiff(group_cols, "model")
    mean_cols <- grep("^mean_", names(df), value = TRUE)
    sd_cols <- grep("^sd_", names(df), value = TRUE)

    df <- df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(model_group_cols))) |>
      dplyr::summarise(
        sim = sum(.data$sim, na.rm = TRUE),
        n_models = dplyr::n_distinct(.data$model),
        dplyr::across(
          dplyr::all_of(mean_cols),
          ~ mean(.x, na.rm = TRUE)
        ),
        dplyr::across(
          dplyr::all_of(sd_cols),
          ~ mean(.x, na.rm = TRUE)
        ),
        dplyr::across(
          dplyr::all_of(mean_cols),
          ~ stats::sd(.x, na.rm = TRUE),
          .names = "sd_model_{.col}"
        ),
        .groups = "drop"
      )
  }

  if (aggregate_level == "chapter" && nrow(chapter_excerpts) > 0) {
    join_cols <- intersect(c("book", "chapter"), names(df))
    excerpt_join_cols <- intersect(join_cols, names(chapter_excerpts))
    if (length(excerpt_join_cols) > 0 && "chapter_excerpt" %in% names(chapter_excerpts)) {
      excerpt_cols <- unique(c(excerpt_join_cols, "chapter_excerpt"))
      df <- dplyr::left_join(
        df,
        chapter_excerpts |>
          dplyr::select(dplyr::all_of(excerpt_cols)) |>
          dplyr::distinct(),
        by = excerpt_join_cols
      )
    }
  }

  # --- Add chapter ordering for chapter-level results ---
  if (aggregate_level == "chapter") {
    df <- df |>
      dplyr::mutate(chapter_num = extract_chapter_num(.data$chapter))

    if (has_book) {
      df <- df |>
        dplyr::arrange(.data$book, .data$chapter_num, .data$chapter) |>
        dplyr::group_by(.data$book) |>
        dplyr::mutate(chapter_index = dplyr::row_number()) |>
        dplyr::ungroup()
    } else {
      df <- df |>
        dplyr::arrange(.data$chapter_num, .data$chapter) |>
        dplyr::mutate(chapter_index = dplyr::row_number())
    }

    df <- dplyr::select(df, -dplyr::all_of("chapter_num"))
  }

  attr(df, "model") <- model
  if (length(models) > 1) {
    attr(df, "models") <- models
  }
  attr(df, "temperature") <- temperature
  attr(df, "n_simulations") <- n_simulations
  attr(df, "score_scale") <- standardize
  attr(df, "model_aggregation") <- model_aggregation
  df
}

chapter_excerpt_index <- function(x) {
  index <- attr(x, "chapter_excerpts")
  if (inherits(index, "data.frame")) {
    index <- tibble::as_tibble(index)
    if (!"book" %in% names(index) && inherits(x, "data.frame") && "book" %in% names(x)) {
      books <- unique(stats::na.omit(as.character(x$book)))
      if (length(books) == 1) {
        index$book <- books[[1]]
        index <- index[, c("book", setdiff(names(index), "book"))]
      }
    }
    return(index)
  }

  if (is.list(x) && !inherits(x, "data.frame")) {
    collected <- Filter(
      function(tbl) inherits(tbl, "data.frame") && nrow(tbl) > 0,
      lapply(x, chapter_excerpt_index)
    )
    if (length(collected) == 0) {
      return(tibble::tibble())
    }
    return(dplyr::bind_rows(collected))
  }

  tibble::tibble()
}

# helper to robustly flatten various nested formats produced by run_ai_on_chapters
flatten_sim_results <- function(z) {
  # If already a data.frame, return as-is
  if (inherits(z, "data.frame")) {
    return(z)
  }
  if (!is.list(z)) {
    stop("Unsupported input: expected data.frame or list-like object")
  }

  # Recursively collect all data.frames inside the (possibly nested) list.
  collect_dfs <- function(obj, parent_name = NULL) {
    out <- list()
    if (inherits(obj, "data.frame")) {
      df <- obj
      # if parent name exists and there's no book column, attach it
      if (!is.null(parent_name) && !"book" %in% names(df)) {
        df$book <- parent_name
      }
      return(list(df))
    }
    if (is.list(obj)) {
      nm <- names(obj)
      for (i in seq_along(obj)) {
        child_name <- if (!is.null(nm) && nzchar(nm[i])) {
          nm[i]
        } else {
          parent_name
        }
        out <- c(out, collect_dfs(obj[[i]], parent_name = child_name))
      }
    }
    out
  }

  dfs <- collect_dfs(z, parent_name = NULL)
  if (length(dfs) == 0) {
    stop("Unsupported nested structure for simulation results")
  }
  # bind rows; if we added book columns above they will be preserved
  return(dplyr::bind_rows(dfs))
}






