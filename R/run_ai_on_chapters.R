#' Run AI model on book chapters and collect structured responses
#'
#' This function implements a two-turn sequential chat design to measure the
#' effect of reading book chapters on attitudes. For each simulation and each
#' identity assignment, the function:
#' \enumerate{
#'   \item Establishes a baseline by assigning an identity, then asking for
#'     ratings of each group (ingroup first, outgroup second).
#'   \item Shows the chapter and asks for post-intervention ratings in the same
#'     chat session (same ordering: ingroup first, outgroup second).
#' }
#' This design creates a within-agent pre-post comparison, with conversation
#' memory maintained between turns. Ingroup and outgroup columns are computed
#' post-hoc from the assigned identity and the group labels.
#'
#' @param book_texts A single character (one chapter) or a nested list of
#'   books -> chapters as returned by `read_book_texts()`.
#' @param groups Character vector of group labels (length >= 2). These are the
#'   groups being compared. Example: `c("Democrat", "Republican")`.
#' @param context_text Character. Either:
#'   \itemize{
#'     \item A scalar template containing `{identity}`, which will be expanded
#'       once for each group (e.g., `"You are simulating an American adult who
#'       politically identifies as a {identity}."`), or
#'     \item A character vector of length equal to `length(groups)`, where each
#'       element is the full context for the corresponding group identity.
#'   }
#' @param question_text Character scalar. A question template containing the
#'   placeholder `{group}`, which will be replaced with each group label.
#'   Example: `"On a scale from 0 to 100, how warmly do you feel towards
#'   {group}s?"`
#' @param output_mode Character. `"structured"` (default) uses the backend's
#'   structured-output support. `"text"` is a compatibility mode for models
#'   that do not support structured outputs (for example some Anthropic models):
#'   nalanda appends strict JSON-only instructions to the prompt, calls the
#'   model as free text, then parses the JSON back into the same fields used by
#'   the rest of the pipeline. Text mode is best-effort and stores the original
#'   model reply in `raw_response`.
#' @param n_simulations Integer. Number of repeated simulations per chapter per
#'   identity (each simulation = 2 chat turns).
#' @param temperature Numeric. Sampling temperature passed to the chat backend.
#' @param seed Integer. Random seed for reproducibility (incremented for each
#'   simulation).
#' @param model Character. Model name for the chat backend (for example,
#'   `"gemini-2.5-flash-lite"`). The value is passed directly to
#'   `ellmer::chat_portkey(model = ...)`.
#' @param integration Optional Portkey/gateway route slug. Should look like
#'   `"vertexai"` or another route returned by
#'   `ellmer::models_portkey(base_url = "https://ai-gateway.apps.cloud.rt.nyu.edu/v1/")`.
#'   If supplied and `model` is not fully-qualified (does not start with `"@"`),
#'   nalanda will build `"@{integration}/{model}"`. In some gateways this slug
#'   is not the upstream provider name. When available, a fully-qualified model
#'   string such as `"@gpt-5-mini/gpt-5-mini"` is the most reliable option.
#'   When both `nalanda.integration` and `nalanda.virtual_key` options are set
#'   and neither argument is supplied, `integration` is preferred.
#' @param virtual_key Optional legacy virtual key. Should look like
#'   `"gemini-8c2498"` or similar. If supplied and `model` is not
#'   fully-qualified, nalanda will build `"@{virtual_key}/{model}"`.
#'   Use either `integration` or `virtual_key`, not both when explicitly
#'   supplying function arguments.
#' @param base_url Character. Base URL for API calls.
#' @param excerpt_chars Integer. Number of chapter characters to retain in the
#'   stored post-prompt preview shown in results.
#' @param checkpoint_dir Optional directory. If supplied, each completed
#'   book/chapter/identity/simulation unit is saved as its own `.Rds` file as
#'   soon as it finishes. If the same call is rerun with the same
#'   `checkpoint_dir`, `checkpoint_prefix`, model, books, groups, and
#'   simulations, completed units are loaded from disk and skipped.
#' @param checkpoint_prefix Character scalar used at the start of checkpoint
#'   filenames when `checkpoint_dir` is supplied.
#' @param on_error Character. `"stop"` raises model/API errors immediately.
#'   `"skip"` records the failed chapter/identity/simulation with missing
#'   ratings and continues. Errors that cannot be chapter-specific, such as an
#'   unreachable gateway or invalid model route, always stop the run early.
#'   Defaults to `"stop"`.
#' @param save_dir Optional directory. If supplied, each book is saved as one
#'   `.Rds` file as soon as all of its chapters, identities, and simulations
#'   finish.
#' @param save_prefix Character scalar used in book-level filenames when
#'   `save_dir` is supplied. Files are named `{save_prefix}_{book}.Rds`.
#' @param .repair_units Internal data frame used by
#'   [repair_run_ai_on_chapters()] to select exact simulation units.
#'
#' @details
#' Authentication uses `PORTKEY_API_KEY` via `ellmer::chat_portkey()`. Set it
#' persistently in `.Renviron`:
#' \preformatted{
#' usethis::edit_r_environ()
#' # Add a line like:
#' # PORTKEY_API_KEY=your_api_key_here
#' }
#' Then restart your R session.
#' @return A tibble of raw turn-level ratings, or a named list of tibbles (one
#'   per book). Each row is one rating observation and includes:
#'   `chapter`, `sim`, `identity`, `turn_index`, `turn_type`, `target_group`,
#'   and `rating`, plus prompt and metadata columns.
#'   Use [compute_run_ai_metrics()] to derive ingroup/outgroup summaries and
#'   gap/delta metrics.
#'   The object has class `nalanda` and model attributes.
#'
#' @examples
#' # Per-group mode (asks about each group, ingroup first):
#' make_baseline_prompt(
#'   identity_context = "You are simulating an American Democrat.",
#'   question_template = "How warmly do you feel towards {group}s?",
#'   groups = c("Democrat", "Republican"),
#'   identity_label = "Democrat"
#' )
#'
#' # Single-question mode (asks once, as-is):
#' make_baseline_prompt(
#'   identity_context = "You are simulating an American Democrat.",
#'   question_template = "How warmly do you feel towards your political outgroup?",
#'   groups = c("Democrat", "Republican"),
#'   identity_label = "Democrat"
#' )
#' @export
run_ai_on_chapters <- function(
  book_texts,
  groups,
  context_text,
  question_text,
  output_mode = c("structured", "text"),
  n_simulations = 1,
  temperature = 0,
  seed = 42,
  model = "gemini-2.5-flash-lite",
  integration = getOption("nalanda.integration"),
  virtual_key = getOption("nalanda.virtual_key"),
  base_url = getOption("nalanda.base_url"),
  excerpt_chars = 200,
  checkpoint_dir = NULL,
  checkpoint_prefix = "run_ai_on_chapters",
  on_error = c("stop", "skip"),
  save_dir = NULL,
  save_prefix = "results",
  .repair_units = NULL
) {
  output_mode <- normalize_output_mode(output_mode)
  on_error <- match.arg(on_error)

  if (is_flat_text_list(book_texts) && is.null(flat_text_list_book_name(book_texts))) {
    stop(
      "`book_texts` looks like one book's flat chapter list, such as ",
      "`book_texts$hownottoage`. `run_ai_on_chapters()` needs a nested ",
      "book -> chapters list so it can preserve the book name. For a ",
      "chapter-level run, use `book_texts[\"hownottoage\"]` or ",
      "`list(hownottoage = book_texts$hownottoage)`. For a whole-book run, ",
      "combine chapters first and pass `full_books[\"hownottoage\"]`. ",
      "If `book_texts` came from `read_book_texts()`, re-read it with the ",
      "current nalanda version so `$` selections keep the book name.",
      call. = FALSE
    )
  }

  if (is_flat_text_list(book_texts) && !is.null(flat_text_list_book_name(book_texts))) {
    validate_chapter_order(
      chapter = names(unlist(book_texts, use.names = TRUE)),
      book = flat_text_list_book_name(book_texts),
      arg_name = "`book_texts` chapter names"
    )
  } else if (is.list(book_texts)) {
    book_names <- names(book_texts)
    for (i in seq_along(book_texts)) {
      book <- if (is.null(book_names)) paste0("book_", i) else book_names[[i]]
      chapter_texts <- unlist(book_texts[[i]], use.names = TRUE)
      if (length(chapter_texts) != 1) {
        validate_chapter_order(
          chapter = names(chapter_texts),
          book = book,
          arg_name = "`book_texts` chapter names"
        )
      }
    }
  }

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
    integration_missing = missing(integration),
    virtual_key_missing = missing(virtual_key),
    base_url = base_url,
    excerpt_chars = excerpt_chars,
    executor = execute_two_turn_pipeline,
    checkpoint_dir = checkpoint_dir,
    checkpoint_prefix = checkpoint_prefix,
    save_dir = save_dir,
    save_prefix = save_prefix,
    output_mode = output_mode,
    on_error = on_error,
    repair_units = .repair_units,
    total_steps = if (is.null(.repair_units)) NULL else nrow(.repair_units)
  )

  out
}

execute_two_turn_pipeline <- function(
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
  model_label,
  checkpoint_dir,
  checkpoint_prefix,
  save_dir,
  save_prefix,
  n_models,
  pb,
  progress_tick,
  output_mode,
  on_error = "stop",
  repair_units = NULL
) {
  on_error <- match.arg(on_error, c("stop", "skip"))
  group_keys <- group_keys_from_groups(groups)
  turn_types <- build_turn_types(
    per_group = per_group,
    groups = groups,
    include_party_first = TRUE
  )
  type_baseline <- turn_types$first
  type_post <- turn_types$second

  all_rows <- list()
  all_row_i <- 0L
  book_start <- 1L
  checkpoint_index <- load_checkpoint_index(
    checkpoint_dir = checkpoint_dir,
    checkpoint_prefix = checkpoint_prefix,
    model_label = model_label
  )

  for (chapter_i in seq_len(nrow(chapter_jobs))) {
    chapter_job <- chapter_jobs[chapter_i, , drop = FALSE]
    if (chapter_i == 1L || !identical(
      progress_scope_id(chapter_job$book[[1]]),
      progress_scope_id(chapter_jobs$book[[chapter_i - 1L]])
    )) {
      book_start <- all_row_i + 1L
    }

    for (id_idx in seq_along(groups)) {
      identity_label <- groups[[id_idx]]
      identity_context <- context_text[[id_idx]]

      for (k in seq_len(n_simulations)) {
        if (!is.null(repair_units)) {
          wanted <- repair_units$book == chapter_job$book[[1]] &
            repair_units$chapter == chapter_job$chapter[[1]] &
            repair_units$identity == identity_label &
            repair_units$sim == k &
            repair_units$model == model_label
          if (!any(wanted)) next
        }
        unit_start <- all_row_i + 1L
        progress_tick(
          book = chapter_job$book[[1]],
          chapter = chapter_job$chapter[[1]],
          identity = identity_label,
          sim = k
        )

        checkpoint_rows <- checkpoint_index_get(
          index = checkpoint_index,
          book = chapter_job$book[[1]],
          chapter = chapter_job$chapter[[1]],
          identity = identity_label,
          sim = k,
          model_label = model_label
        )
        if (!is.null(checkpoint_rows)) {
          appended <- append_checkpoint_rows(
            all_rows = all_rows,
            all_row_i = all_row_i,
            rows = checkpoint_rows
          )
          all_rows <- appended$rows
          all_row_i <- appended$row_i
          next
        }

        if (is_missing_chapter_text(chapter_job$chapter_text[[1]])) {
          base_fields <- make_result_base_fields(
            book = chapter_job$book[[1]],
            chapter = chapter_job$chapter[[1]],
            sim = k,
            identity = identity_label,
            party = identity_label,
            extra = c(
              list(
                baseline_prompt = NA_character_,
                post_prompt = NA_character_,
                error = FALSE,
                error_turn = NA_character_,
                error_message = NA_character_
              ),
              if (identical(output_mode, "text")) {
                list(
                  baseline_raw_response = NA_character_,
                  post_raw_response = NA_character_
                )
              } else {
                list()
              }
            )
          )

          if (per_group) {
            for (g_idx in seq_along(groups)) {
              row_pre <- c(
                base_fields,
                list(
                  turn_index = 1L,
                  turn_type = "baseline",
                  target_group = groups[[g_idx]],
                  rating = NA_real_
                )
              )
              row_post <- c(
                base_fields,
                list(
                  turn_index = 2L,
                  turn_type = "post",
                  target_group = groups[[g_idx]],
                  rating = NA_real_
                )
              )

              all_row_i <- all_row_i + 1L
              all_rows[[all_row_i]] <- row_pre
              all_row_i <- all_row_i + 1L
              all_rows[[all_row_i]] <- row_post
            }
          } else {
            row_pre <- c(
              base_fields,
              list(
                turn_index = 1L,
                turn_type = "baseline",
                target_group = NA_character_,
                rating = NA_real_
              )
            )
            row_post <- c(
              base_fields,
              list(
                turn_index = 2L,
                turn_type = "post",
                target_group = NA_character_,
                rating = NA_real_
              )
            )

            all_row_i <- all_row_i + 1L
            all_rows[[all_row_i]] <- row_pre
            all_row_i <- all_row_i + 1L
            all_rows[[all_row_i]] <- row_post
          }

          write_simulation_checkpoint(
            rows = all_rows[unit_start:all_row_i],
            long_cols = c("baseline_prompt", "post_prompt"),
            checkpoint_dir = checkpoint_dir,
            checkpoint_prefix = checkpoint_prefix,
            model_label = model_label,
            book = chapter_job$book[[1]],
            chapter = chapter_job$chapter[[1]],
            identity = identity_label,
            sim = k
          )

          next
        }

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

        baseline_response <- tryCatch(
          chat_model_response(
            chat = chat,
            prompt = baseline_prompt,
            type = type_baseline,
            output_mode = output_mode,
            fields = build_response_fields(
              per_group = per_group,
              groups = groups,
              include_party = TRUE
            )
          ),
          error = function(e) {
            if (is_unrecoverable_model_error(e)) {
              stop_unrecoverable_model_error(e, model_label)
            }
            if (identical(on_error, "stop")) {
              stop(e)
            }
            structure(
              list(error = e, turn = "baseline"),
              class = "nalanda_skipped_model_error"
            )
          }
        )

        if (inherits(baseline_response, "nalanda_skipped_model_error")) {
          skipped <- append_skipped_two_turn_rows(
            all_rows = all_rows,
            all_row_i = all_row_i,
            book = chapter_job$book[[1]],
            chapter = chapter_job$chapter[[1]],
            sim = k,
            identity = identity_label,
            party = identity_label,
            baseline_prompt = baseline_prompt,
            post_prompt = post_prompt,
            error_turn = baseline_response$turn,
            error_message = conditionMessage(baseline_response$error),
            per_group = per_group,
            groups = groups,
            output_mode = output_mode
          )
          all_rows <- skipped$rows
          all_row_i <- skipped$row_i

          write_simulation_checkpoint(
            rows = all_rows[unit_start:all_row_i],
            long_cols = c("baseline_prompt", "post_prompt", "error_message"),
            checkpoint_dir = checkpoint_dir,
            checkpoint_prefix = checkpoint_prefix,
            model_label = model_label,
            book = chapter_job$book[[1]],
            chapter = chapter_job$chapter[[1]],
            identity = identity_label,
            sim = k
          )

          warning(
            "Skipping failed simulation unit: ",
            chapter_job$book[[1]],
            " - ",
            chapter_job$chapter[[1]],
            " [",
            identity_label,
            "] sim ",
            k,
            " (",
            model_label,
            "): ",
            conditionMessage(baseline_response$error),
            call. = FALSE
          )
          next
        }

        post_response <- tryCatch(
          chat_model_response(
            chat = chat,
            prompt = full_post_prompt,
            type = type_post,
            output_mode = output_mode,
            fields = build_response_fields(
              per_group = per_group,
              groups = groups,
              include_party = FALSE
            )
          ),
          error = function(e) {
            if (is_unrecoverable_model_error(e)) {
              stop_unrecoverable_model_error(e, model_label)
            }
            if (identical(on_error, "stop")) {
              stop(e)
            }
            structure(
              list(error = e, turn = "post"),
              class = "nalanda_skipped_model_error"
            )
          }
        )

        if (inherits(post_response, "nalanda_skipped_model_error")) {
          skipped <- append_skipped_two_turn_rows(
            all_rows = all_rows,
            all_row_i = all_row_i,
            book = chapter_job$book[[1]],
            chapter = chapter_job$chapter[[1]],
            sim = k,
            identity = identity_label,
            party = baseline_response$party,
            baseline_prompt = baseline_prompt,
            post_prompt = post_prompt,
            error_turn = post_response$turn,
            error_message = conditionMessage(post_response$error),
            per_group = per_group,
            groups = groups,
            output_mode = output_mode
          )
          all_rows <- skipped$rows
          all_row_i <- skipped$row_i

          write_simulation_checkpoint(
            rows = all_rows[unit_start:all_row_i],
            long_cols = c("baseline_prompt", "post_prompt", "error_message"),
            checkpoint_dir = checkpoint_dir,
            checkpoint_prefix = checkpoint_prefix,
            model_label = model_label,
            book = chapter_job$book[[1]],
            chapter = chapter_job$chapter[[1]],
            identity = identity_label,
            sim = k
          )

          warning(
            "Skipping failed simulation unit: ",
            chapter_job$book[[1]],
            " - ",
            chapter_job$chapter[[1]],
            " [",
            identity_label,
            "] sim ",
            k,
            " (",
            model_label,
            "): ",
            conditionMessage(post_response$error),
            call. = FALSE
          )
          next
        }

        base_fields <- make_result_base_fields(
          book = chapter_job$book[[1]],
          chapter = chapter_job$chapter[[1]],
          sim = k,
          identity = identity_label,
          party = baseline_response$party,
          extra = c(
            list(
              baseline_prompt = baseline_prompt,
              post_prompt = post_prompt,
              error = FALSE,
              error_turn = NA_character_,
              error_message = NA_character_
            ),
            soft_raw_response_field(
              baseline_response,
              "baseline_raw_response",
              output_mode
            ),
            soft_raw_response_field(
              post_response,
              "post_raw_response",
              output_mode
            )
          )
        )

        if (per_group) {
          for (g_idx in seq_along(groups)) {
            field <- paste0("rating_", group_keys[[g_idx]])

            row_pre <- c(
              base_fields,
              list(
                turn_index = 1L,
                turn_type = "baseline",
                target_group = groups[[g_idx]],
                rating = baseline_response[[field]]
              )
            )
            row_post <- c(
              base_fields,
              list(
                turn_index = 2L,
                turn_type = "post",
                target_group = groups[[g_idx]],
                rating = post_response[[field]]
              )
            )

            all_row_i <- all_row_i + 1L
            all_rows[[all_row_i]] <- row_pre
            all_row_i <- all_row_i + 1L
            all_rows[[all_row_i]] <- row_post
          }
        } else {
          row_pre <- c(
            base_fields,
            list(
              turn_index = 1L,
              turn_type = "baseline",
              target_group = NA_character_,
              rating = baseline_response$rating
            )
          )
          row_post <- c(
            base_fields,
            list(
              turn_index = 2L,
              turn_type = "post",
              target_group = NA_character_,
              rating = post_response$rating
            )
          )

          all_row_i <- all_row_i + 1L
          all_rows[[all_row_i]] <- row_pre
          all_row_i <- all_row_i + 1L
          all_rows[[all_row_i]] <- row_post
        }

        write_simulation_checkpoint(
          rows = all_rows[unit_start:all_row_i],
          long_cols = c("baseline_prompt", "post_prompt", "error_message"),
          checkpoint_dir = checkpoint_dir,
          checkpoint_prefix = checkpoint_prefix,
          model_label = model_label,
          book = chapter_job$book[[1]],
          chapter = chapter_job$chapter[[1]],
          identity = identity_label,
          sim = k
        )
      }
    }

    next_book <- if (chapter_i < nrow(chapter_jobs)) {
      chapter_jobs$book[[chapter_i + 1L]]
    } else {
      NA_character_
    }
    if (!identical(
      progress_scope_id(chapter_job$book[[1]]),
      progress_scope_id(next_book)
    )) {
      write_book_result(
        rows = all_rows[book_start:all_row_i],
        long_cols = c("baseline_prompt", "post_prompt", "error_message"),
        chapter_jobs = chapter_jobs,
        save_dir = save_dir,
        save_prefix = save_prefix,
        n_models = n_models,
        model_label = model_label,
        temperature = temperature,
        n_simulations = n_simulations,
        book = chapter_job$book[[1]]
      )
    }
  }

  finalize_simulation_output(
    out_rows = all_rows,
    long_cols = c("baseline_prompt", "post_prompt", "error_message"),
    chapter_jobs = chapter_jobs
  )
}

append_skipped_two_turn_rows <- function(
  all_rows,
  all_row_i,
  book,
  chapter,
  sim,
  identity,
  party,
  baseline_prompt,
  post_prompt,
  error_turn,
  error_message,
  per_group,
  groups,
  output_mode
) {
  base_fields <- make_result_base_fields(
    book = book,
    chapter = chapter,
    sim = sim,
    identity = identity,
    party = party,
    extra = c(
      list(
        baseline_prompt = baseline_prompt,
        post_prompt = post_prompt,
        error = TRUE,
        error_turn = error_turn,
        error_message = error_message
      ),
      if (identical(output_mode, "text")) {
        list(
          baseline_raw_response = NA_character_,
          post_raw_response = NA_character_
        )
      } else {
        list()
      }
    )
  )

  if (per_group) {
    for (g_idx in seq_along(groups)) {
      row_pre <- c(
        base_fields,
        list(
          turn_index = 1L,
          turn_type = "baseline",
          target_group = groups[[g_idx]],
          rating = NA_real_
        )
      )
      row_post <- c(
        base_fields,
        list(
          turn_index = 2L,
          turn_type = "post",
          target_group = groups[[g_idx]],
          rating = NA_real_
        )
      )

      all_row_i <- all_row_i + 1L
      all_rows[[all_row_i]] <- row_pre
      all_row_i <- all_row_i + 1L
      all_rows[[all_row_i]] <- row_post
    }
  } else {
    row_pre <- c(
      base_fields,
      list(
        turn_index = 1L,
        turn_type = "baseline",
        target_group = NA_character_,
        rating = NA_real_
      )
    )
    row_post <- c(
      base_fields,
      list(
        turn_index = 2L,
        turn_type = "post",
        target_group = NA_character_,
        rating = NA_real_
      )
    )

    all_row_i <- all_row_i + 1L
    all_rows[[all_row_i]] <- row_pre
    all_row_i <- all_row_i + 1L
    all_rows[[all_row_i]] <- row_post
  }

  list(rows = all_rows, row_i = all_row_i)
}

#' Build the baseline (Turn 1) prompt
#'
#' Constructs the prompt: identity context + question(s). If the question
#' template contains `{group}`, it is expanded once per group (ingroup first).
#' Otherwise, the question is used as-is (single-question mode).
#'
#' @param identity_context Character scalar. The full context string for this
#'   identity.
#' @param question_template Character scalar. Optionally contains `{group}`
#'   placeholder for per-group expansion.
#' @param groups Character vector of all group labels.
#' @param identity_label Character scalar. The group label assigned as identity
#'   (used to determine ingroup-first ordering).
#' @return Character scalar prompt.
#'
#' @examples
#' # Per-group mode (asks about each group, ingroup first):
#' make_baseline_prompt(
#'   identity_context = "You are simulating an American Democrat.",
#'   question_template = "How warmly (0-100) do you feel towards {group}s?",
#'   groups = c("Democrat", "Republican"),
#'   identity_label = "Democrat"
#' )
#'
#' # Single-question mode (asks once, as-is):
#' make_baseline_prompt(
#'   identity_context = "You are simulating an American Democrat.",
#'   question_template = "How warmly (0-100) do you feel towards your outgroup?",
#'   groups = c("Democrat", "Republican"),
#'   identity_label = "Democrat"
#' )
#'
#' @export
make_baseline_prompt <- function(
  identity_context,
  question_template,
  groups,
  identity_label
) {
  if (grepl("\\{group\\}", question_template)) {
    ordered_groups <- c(identity_label, setdiff(groups, identity_label))
    question_block <- paste(
      vapply(
        ordered_groups,
        function(g) gsub("\\{group\\}", g, question_template),
        character(1),
        USE.NAMES = FALSE
      ),
      collapse = " "
    )
  } else {
    question_block <- question_template
  }

  paste(identity_context, question_block, sep = " ")
}

#' Build the post-intervention (Turn 2) prompt
#'
#' Constructs the prompt: material text + question(s). If the question
#' template contains `{group}`, it is expanded once per group (ingroup first).
#' Otherwise, the question is used as-is (single-question mode).
#'
#' @param chapter_text Character scalar. The full material text.
#' @param question_template Character scalar. Optionally contains `{group}`
#'   placeholder for per-group expansion.
#' @param groups Character vector of all group labels.
#' @param identity_label Character scalar. The group label assigned as identity.
#' @return Character scalar prompt.
#'
#' @examples
#' # Per-group mode:
#' make_post_prompt(
#'   chapter_text = "This is a chapter about cooperation...",
#'   question_template = "How warmly (0-100) do you feel towards {group}s?",
#'   groups = c("Democrat", "Republican"),
#'   identity_label = "Democrat"
#' )
#'
#' # Single-question mode:
#' make_post_prompt(
#'   chapter_text = "This is a chapter about cooperation...",
#'   question_template = "How warmly (0-100) do you feel towards your outgroup?",
#'   groups = c("Democrat", "Republican"),
#'   identity_label = "Democrat"
#' )
#'
#' @export
make_post_prompt <- function(
  chapter_text,
  question_template,
  groups,
  identity_label
) {
  if (grepl("\\{group\\}", question_template)) {
    ordered_groups <- c(identity_label, setdiff(groups, identity_label))
    question_block <- paste(
      vapply(
        ordered_groups,
        function(g) gsub("\\{group\\}", g, question_template),
        character(1),
        USE.NAMES = FALSE
      ),
      collapse = " "
    )
  } else {
    question_block <- question_template
  }

  paste0(
    "You have just read the material below.\n\n",
    chapter_text,
    "\n\n",
    "You have now just finished reading the material. Now that this is done:\n",
    question_block
  )
}

make_post_prompt_preview <- function(
  chapter_text,
  question_template,
  groups,
  identity_label,
  excerpt_chars = 200
) {
  compact_chapter <- compact_chapter_text(
    chapter_text,
    excerpt_chars = excerpt_chars
  )

  make_post_prompt(
    chapter_text = compact_chapter,
    question_template = question_template,
    groups = groups,
    identity_label = identity_label
  )
}

compact_chapter_text <- function(chapter_text, excerpt_chars = 200) {
  if (!is.character(chapter_text) || length(chapter_text) != 1) {
    stop("`chapter_text` must be a single character string.")
  }

  excerpt_chars <- as.integer(excerpt_chars[[1]])
  if (is.na(excerpt_chars) || excerpt_chars < 1) {
    stop("`excerpt_chars` must be a positive integer.")
  }

  chapter_text <- repair_chapter_text_encoding(chapter_text)
  n_chars <- nchar(chapter_text, type = "chars", allowNA = FALSE, keepNA = FALSE)
  if (n_chars <= excerpt_chars) {
    return(chapter_text)
  }

  head_chars <- max(1L, floor(excerpt_chars / 2))
  tail_chars <- max(1L, excerpt_chars - head_chars)

  paste0(
    substr(chapter_text, 1, head_chars),
    "\n\n[... chapter text cropped for storage ...]\n\n",
    substr(chapter_text, n_chars - tail_chars + 1L, n_chars)
  )
}

repair_chapter_text_encoding <- function(chapter_text) {
  is_valid_text <- function(x) {
    tryCatch(
      {
        nchar(x, type = "chars", allowNA = FALSE, keepNA = FALSE)
        TRUE
      },
      error = function(e) FALSE
    )
  }

  if (is_valid_text(chapter_text)) {
    return(chapter_text)
  }

  encodings <- c("", "UTF-8", "WINDOWS-1252", "latin1")
  for (from in encodings) {
    repaired <- suppressWarnings(
      tryCatch(
        iconv(chapter_text, from = from, to = "UTF-8", sub = ""),
        error = function(e) NA_character_
      )
    )

    if (!is.na(repaired) && is_valid_text(repaired)) {
      return(repaired)
    }
  }

  stop(
    "Unable to repair invalid multibyte `chapter_text`.",
    call. = FALSE
  )
}
