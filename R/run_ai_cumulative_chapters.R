#' Run AI model on books with cumulative chapter context
#'
#' This function implements a cumulative multi-turn design where each
#' simulation creates one persistent chat per book and identity. The chat first
#' establishes a baseline, then processes chapters sequentially in order, one
#' turn per chapter, preserving context across the full book.
#'
#' @param book_texts A nested list of books -> chapters as returned by
#'   `read_book_texts()`.
#' @param groups Character vector of group labels (length >= 2).
#' @param context_text Character. Either a scalar template containing
#'   `{identity}` or a character vector of length `length(groups)`.
#' @param question_text Character scalar. A question template containing the
#'   placeholder `{group}`, which will be replaced with each group label.
#' @param n_simulations Integer. Number of repeated simulations per book per
#'   identity.
#' @param temperature Numeric. Sampling temperature passed to the chat backend.
#' @param seed Integer. Random seed for reproducibility (incremented for each
#'   simulation).
#' @param model Character. Model name for the chat backend.
#' @param integration Optional integration/provider slug.
#' @param virtual_key Optional legacy virtual key.
#' @param base_url Character. Base URL for API calls.
#' @param excerpt_chars Integer. Number of chapter characters to retain in the
#'   stored prompt previews shown in results.
#' @param include_tokens Logical. Return token counts if available.
#' @param include_cost Logical. Return cost info if available.
#'
#' @return A tibble or named list of tibbles with cumulative turn-level rows.
#'   The baseline turn is followed by one post turn per chapter, all within the
#'   same chat per book/identity/simulation.
#'
#' @examples
#' \dontrun{
#' raw_cumulative <- run_ai_cumulative_chapters(
#'   book_texts = list(
#'     "Book A" = list(
#'       chapter_1 = "A first chapter about cooperation.",
#'       chapter_2 = "A second chapter about conflict and repair."
#'     )
#'   ),
#'   groups = c("Democrat", "Republican"),
#'   context_text = "You are simulating an American adult who politically identifies as a {identity}.",
#'   question_text = "On a scale from 0 to 100, how warmly do you feel towards {group}s?",
#'   n_simulations = 1,
#'   temperature = 0,
#'   seed = 42
#' )
#'
#' compute_run_ai_metrics_cumulative(raw_cumulative)
#' }
#' @export
run_ai_cumulative_chapters <- function(
  book_texts,
  groups,
  context_text,
  question_text,
  n_simulations = 1,
  temperature = 0,
  seed = 42,
  model = "gemini-2.5-flash-lite",
  integration = getOption("nalanda.integration"),
  virtual_key = getOption("nalanda.virtual_key"),
  base_url = getOption("nalanda.base_url"),
  excerpt_chars = 200,
  include_tokens = FALSE,
  include_cost = FALSE
) {
  route <- resolve_model_route(
    integration = integration,
    virtual_key = virtual_key,
    integration_missing = missing(integration),
    virtual_key_missing = missing(virtual_key)
  )
  integration <- route$integration
  virtual_key <- route$virtual_key

  if (!is.list(book_texts) || is.character(book_texts)) {
    stop("`book_texts` must be a nested list of books and chapters.")
  }

  book_names <- names(book_texts)
  for (i in seq_along(book_texts)) {
    book <- if (is.null(book_names)) paste0("book_", i) else book_names[[i]]
    chapter_texts <- unlist(book_texts[[i]], use.names = TRUE)
    validate_chapter_order(
      chapter = names(chapter_texts),
      book = book,
      arg_name = "`book_texts` chapter names"
    )
  }

  total_steps <- (length(book_texts) + sum(vapply(book_texts, length, integer(1)))) *
    n_simulations * length(groups)

  out <- run_simulation_pipeline(
    book_texts = book_texts,
    groups = groups,
    context_text = context_text,
    question_text = question_text,
    n_simulations = n_simulations,
    temperature = temperature,
    seed = seed,
    model = model,
    integration = integration,
    virtual_key = virtual_key,
    base_url = base_url,
    excerpt_chars = excerpt_chars,
    include_tokens = include_tokens,
    include_cost = include_cost,
    executor = execute_cumulative_chapter_pipeline,
    total_steps = total_steps
  )

  if (is.list(out) && !inherits(out, "data.frame")) {
    for (nm in names(out)) {
      attr(out[[nm]], "model") <- normalize_model_name(attr(out[[nm]], "model"))
    }
  }
  attr(out, "model") <- normalize_model_name(attr(out, "model"))
  out
}

execute_cumulative_chapter_pipeline <- function(
  chapter_jobs,
  groups,
  context_text,
  per_group,
  question_text,
  n_simulations,
  temperature,
  seed,
  model,
  base_url,
  excerpt_chars,
  include_tokens,
  include_cost,
  pb
) {
  group_keys <- group_keys_from_groups(groups)
  turn_types <- build_turn_types(
    per_group = per_group,
    groups = groups,
    include_party_first = TRUE
  )
  type_baseline <- turn_types$first
  type_post <- turn_types$second

  books <- split(chapter_jobs, chapter_jobs$book)
  all_rows <- list()
  all_row_i <- 0L

  for (book_name in names(books)) {
    book_jobs <- books[[book_name]]

    for (id_idx in seq_along(groups)) {
      identity_label <- groups[[id_idx]]
      identity_context <- context_text[[id_idx]]

      for (k in seq_len(n_simulations)) {
        baseline_label <- format_progress_label(
          book = book_name,
          chapter = "baseline",
          identity = identity_label,
          sim = k
        )
        pb$tick(tokens = list(what = baseline_label))

        chat <- new_portkey_chat(
          model = model,
          base_url = base_url,
          temperature = temperature,
          seed = seed + k - 1L
        )

        baseline_prompt <- make_baseline_prompt(
          identity_context,
          question_text,
          groups,
          identity_label
        )
        baseline_response <- chat$chat_structured(
          baseline_prompt,
          type = type_baseline
        )

        baseline_fields <- make_result_base_fields(
          book = book_name,
          chapter = "baseline",
          sim = k,
          identity = identity_label,
          party = baseline_response$party,
          extra = list(
            baseline_prompt = baseline_prompt,
            post_prompt = NA_character_,
            chapter_index = 0L
          )
        )
        baseline_rows <- build_cumulative_turn_rows(
          base_fields = baseline_fields,
          response = baseline_response,
          turn_index = 1L,
          turn_type = "baseline",
          per_group = per_group,
          groups = groups,
          group_keys = group_keys,
          include_tokens = include_tokens,
          include_cost = include_cost
        )
        for (row in baseline_rows) {
          all_row_i <- all_row_i + 1L
          all_rows[[all_row_i]] <- row
        }

        for (chapter_pos in seq_len(nrow(book_jobs))) {
          chapter_job <- book_jobs[chapter_pos, , drop = FALSE]
          what_text <- format_progress_label(
            book = book_name,
            chapter = chapter_job$chapter[[1]],
            identity = identity_label,
            sim = k
          )
          pb$tick(tokens = list(what = what_text))

          full_post_prompt <- make_post_prompt(
            chapter_job$chapter_text[[1]],
            question_text,
            groups,
            identity_label
          )
          post_prompt <- make_post_prompt_preview(
            chapter_text = chapter_job$chapter_text[[1]],
            question_template = question_text,
            groups = groups,
            identity_label = identity_label,
            excerpt_chars = excerpt_chars
          )
          post_response <- chat$chat_structured(
            full_post_prompt,
            type = type_post
          )

          post_fields <- make_result_base_fields(
            book = book_name,
            chapter = chapter_job$chapter[[1]],
            sim = k,
            identity = identity_label,
            party = baseline_response$party,
            extra = list(
              baseline_prompt = baseline_prompt,
              post_prompt = post_prompt,
              chapter_index = chapter_pos
            )
          )
          post_rows <- build_cumulative_turn_rows(
            base_fields = post_fields,
            response = post_response,
            turn_index = chapter_pos + 1L,
            turn_type = "post",
            per_group = per_group,
            groups = groups,
            group_keys = group_keys,
            include_tokens = include_tokens,
            include_cost = include_cost
          )
          for (row in post_rows) {
            all_row_i <- all_row_i + 1L
            all_rows[[all_row_i]] <- row
          }
        }
      }
    }
  }

  finalize_simulation_output(
    out_rows = all_rows,
    long_cols = c("baseline_prompt", "post_prompt"),
    chapter_jobs = chapter_jobs
  )
}

build_cumulative_turn_rows <- function(
  base_fields,
  response,
  turn_index,
  turn_type,
  per_group,
  groups,
  group_keys,
  include_tokens,
  include_cost
) {
  out <- list()

  if (isTRUE(per_group)) {
    for (g_idx in seq_along(groups)) {
      field <- paste0("rating_", group_keys[[g_idx]])
      out[[length(out) + 1L]] <- attach_usage_fields(
        c(
          base_fields,
          list(
            turn_index = turn_index,
            turn_type = turn_type,
            target_group = groups[[g_idx]],
            rating = response[[field]]
          )
        ),
        response,
        include_tokens,
        include_cost
      )
    }
  } else {
    out[[1]] <- attach_usage_fields(
      c(
        base_fields,
        list(
          turn_index = turn_index,
          turn_type = turn_type,
          target_group = NA_character_,
          rating = response$rating
        )
      ),
      response,
      include_tokens,
      include_cost
    )
  }

  out
}

#' Compute cumulative chapter metrics against the original baseline
#'
#' @param x A data frame or list-like object from [run_ai_cumulative_chapters()]
#'   with turn-level rows including `book`, `chapter`, `chapter_index`, `sim`,
#'   `identity`, `turn_type`, and `rating`.
#' @param per_group Optional logical. Whether the run used per-group mode. If
#'   `NULL`, mode is inferred from `target_group`.
#'
#' @return A chapter-level tibble comparing each post-chapter state against the
#'   baseline turn from the same `book × sim × identity` conversation.
#' @export
compute_run_ai_metrics_cumulative <- function(x, per_group = NULL) {
  input <- x

  if (is.list(input) && !inherits(input, "data.frame")) {
    x <- flatten_sim_results(input)
  }

  x <- tibble::as_tibble(x)

  model <- attr(input, "model")
  temperature <- attr(input, "temperature")
  n_simulations <- attr(input, "n_simulations")
  chapter_excerpts <- chapter_excerpt_index(input)

  if (is.null(model) && is.list(input) && !inherits(input, "data.frame") &&
    length(input) > 0) {
    model <- rlang::`%||%`(model, attr(input[[1]], "model"))
    temperature <- rlang::`%||%`(temperature, attr(input[[1]], "temperature"))
    n_simulations <- rlang::`%||%`(n_simulations, attr(input[[1]], "n_simulations"))
  }
  model <- normalize_model_name(model)

  required_cols <- c("book", "chapter", "chapter_index", "sim", "identity", "turn_type", "rating")
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0) {
    stop(
      "`x` is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      "."
    )
  }
  if (!("target_group" %in% names(x))) {
    x$target_group <- NA_character_
  }

  if (is.null(per_group)) {
    per_group <- any(!is.na(x$target_group) & nzchar(as.character(x$target_group)))
  }

  convo_cols <- c("book", "sim", "identity")
  optional_cols <- c("party", "baseline_prompt")
  convo_cols <- c(convo_cols, intersect(optional_cols, names(x)))

  include_token_col <- "input_tokens" %in% names(x)
  include_cost_col <- "cost" %in% names(x)

  convo_key <- interaction(x[convo_cols], drop = TRUE, lex.order = TRUE)
  convo_indices <- split(seq_len(nrow(x)), convo_key)
  rows <- list()
  row_i <- 0L

  for (idx in convo_indices) {
    xi <- x[idx, , drop = FALSE]
    pre <- xi[xi$turn_type == "baseline", , drop = FALSE]
    post <- xi[xi$turn_type == "post", , drop = FALSE]

    if (nrow(pre) == 0 || nrow(post) == 0) {
      stop("Each cumulative conversation must include baseline and post turns.")
    }

    for (chapter_id in unique(post$chapter)) {
      chapter_post <- post[post$chapter == chapter_id, , drop = FALSE]
      row <- as.list(chapter_post[1, c(convo_cols, intersect(c("chapter", "chapter_index", "post_prompt"), names(chapter_post))), drop = FALSE])

      if (include_token_col) {
        row$input_tokens <- sum(as.numeric(chapter_post$input_tokens), na.rm = TRUE) +
          sum(as.numeric(pre$input_tokens), na.rm = TRUE)
      }
      if (include_cost_col) {
        row$cost <- sum(as.numeric(chapter_post$cost), na.rm = TRUE) +
          sum(as.numeric(pre$cost), na.rm = TRUE)
      }

      if (isTRUE(per_group)) {
        identity_label <- as.character(row$identity)
        row$pre_ingroup <- mean(
          as.numeric(pre$rating[as.character(pre$target_group) == identity_label]),
          na.rm = TRUE
        )
        row$post_ingroup <- mean(
          as.numeric(chapter_post$rating[as.character(chapter_post$target_group) == identity_label]),
          na.rm = TRUE
        )
        row$pre_outgroup <- mean(
          as.numeric(pre$rating[as.character(pre$target_group) != identity_label]),
          na.rm = TRUE
        )
        row$post_outgroup <- mean(
          as.numeric(chapter_post$rating[as.character(chapter_post$target_group) != identity_label]),
          na.rm = TRUE
        )
        row$pre_gap <- row$pre_ingroup - row$pre_outgroup
        row$post_gap <- row$post_ingroup - row$post_outgroup
        row$delta_ingroup <- row$post_ingroup - row$pre_ingroup
        row$delta_outgroup <- row$post_outgroup - row$pre_outgroup
        row$delta_gap <- row$delta_outgroup - row$delta_ingroup
      } else {
        row$pre_rating <- mean(as.numeric(pre$rating), na.rm = TRUE)
        row$post_rating <- mean(as.numeric(chapter_post$rating), na.rm = TRUE)
        row$pre_ingroup <- NA_real_
        row$post_ingroup <- NA_real_
        row$pre_outgroup <- row$pre_rating
        row$post_outgroup <- row$post_rating
        row$delta_outgroup <- row$post_outgroup - row$pre_outgroup
      }

      row_i <- row_i + 1L
      rows[[row_i]] <- row
    }
  }

  out <- tibble::as_tibble(do.call(
    rbind.data.frame,
    lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE))
  ))

  long_cols <- intersect(c("baseline_prompt", "post_prompt"), names(out))
  if (length(long_cols) > 0) {
    out <- out[, c(setdiff(names(out), long_cols), long_cols)]
  }

  attr(out, "model") <- model
  attr(out, "temperature") <- temperature
  attr(out, "n_simulations") <- n_simulations
  attr(out, "chapter_excerpts") <- chapter_excerpts
  out
}
