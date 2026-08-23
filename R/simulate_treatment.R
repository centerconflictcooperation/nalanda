#' Simulate a generic multi-turn treatment workflow
#'
#' This function provides a simpler, prompt-first interface for running one or
#' more turns against an intervention text. Each element of `prompt` defines one
#' turn in the chat sequence. When `groups` is supplied, the same prompt
#' sequence is repeated for each group identity; groups do not create additional
#' turns. For independent alternative prompts that must start fresh
#' conversations, use [run_prompt_grid()] instead.
#'
#' @param intervention_text A single character string or a nested list of
#'   intervention texts. This is mapped internally onto the same job grid used
#'   by [run_ai_on_chapters()]. Defaults to `""`, which is useful when the full
#'   treatment is already encoded in `prompt` and/or `context_text`.
#' @param prompt Character vector of prompt templates. Each element defines one
#'   ordered turn in the same conversation, not an independent prompt variant.
#'   Prompt templates may include `{intervention_text}`, `{identity}`, and
#'   `{group}` placeholders.
#' @param response_type An `ellmer` structured type specification applied to all
#'   turns (for example `ellmer::type_object(score = ellmer::type_number())`).
#' @param output_mode Character. `"structured"` (default) uses the backend's
#'   structured-output support. `"text"` is a compatibility mode for models
#'   that do not support structured outputs (for example some Anthropic models):
#'   nalanda appends strict JSON-only instructions to the prompt, calls the
#'   model as free text, then parses the JSON back into the same tabular fields.
#'   Text mode is best-effort and stores the original model reply in
#'   `raw_response`.
#' @param groups Optional character vector of group labels. If supplied, the
#'   full prompt sequence is rerun for each group identity.
#' @param context_text Optional character scalar or vector. If provided, it is
#'   prepended to every turn for the corresponding group. Scalar values are
#'   recycled across groups, and `{identity}` is expanded when present.
#' @param n_simulations Integer. Number of repeated simulations per
#'   intervention per identity.
#' @param temperature Numeric. Sampling temperature passed to the chat backend.
#' @param seed Integer. Random seed for reproducibility (incremented for each
#'   simulation index).
#' @param model Character. Model name for the chat backend.
#' @param integration Optional Portkey/gateway route slug. If supplied and
#'   `model` is not fully-qualified, nalanda will build
#'   `"@{integration}/{model}"`. Use a route returned by
#'   `ellmer::models_portkey(base_url = "https://ai-gateway.apps.cloud.rt.nyu.edu/v1/")`
#'   when working with the NYU gateway.
#' @param virtual_key Optional legacy virtual key. If supplied and `model` is
#'   not fully-qualified, nalanda will build `"@{virtual_key}/{model}"`.
#' @param base_url Character. Base URL for API calls.
#' @param excerpt_chars Integer. Number of intervention-text characters to
#'   retain in stored prompt previews.
#' @param checkpoint_dir Optional directory. If supplied, each completed
#'   treatment/identity/simulation unit is saved as its own `.Rds` file as soon
#'   as it finishes. If the same call is rerun with the same `checkpoint_dir`,
#'   `checkpoint_prefix`, model, treatments, groups, and simulations, completed
#'   units are loaded from disk and skipped.
#' @param checkpoint_prefix Character scalar used at the start of checkpoint
#'   filenames when `checkpoint_dir` is supplied.
#' @param save_dir Optional directory. If supplied, each intervention collection
#'   is saved as one `.Rds` file as soon as all of its treatments, identities,
#'   and simulations finish.
#' @param save_prefix Character scalar used in book-level filenames when
#'   `save_dir` is supplied. Files are named `{save_prefix}_{book}.Rds`.
#'
#' @return A tibble of raw turn-level responses, or a named list of tibbles
#'   (one per book/intervention collection). Each row includes `treatment`,
#'   `sim`, `identity`, `turn_index`, `turn_type`, and one column per field
#'   returned by `response_type`, plus stored prompt previews and metadata
#'   columns.
#'
#' @examples
#' \dontrun{
#' simulate_treatment(
#'   intervention_text = "A short passage about people working together.",
#'   prompt = c(
#'     "Read the following text:\n\n{intervention_text}\n\nRate its readability from 0 to 100."
#'   ),
#'   response_type = ellmer::type_object(
#'     score = ellmer::type_number()
#'   ),
#'   n_simulations = 2,
#'   temperature = 0,
#'   seed = 42
#' )
#'
#' simulate_treatment(
#'   groups = c("South African", "Danish"),
#'   context_text = "You are simulating an adult who identifies as {identity}.",
#'   prompt = c(
#'     climate_belief = paste(
#'       "Generally speaking, do you usually think of yourself as Danish or South African?",
#'       "On a scale from 0 to 100, how accurate do you think this statement is?",
#'       "Statement: Human activities are causing climate change"
#'     )
#'   ),
#'   response_type = ellmer::type_object(
#'     rating = ellmer::type_number()
#'   ),
#'   n_simulations = 2,
#'   temperature = 0,
#'   seed = 42
#' )
#' }
#' @export
simulate_treatment <- function(
  intervention_text = "",
  prompt,
  response_type,
  output_mode = c("structured", "text"),
  groups = NULL,
  context_text = NULL,
  n_simulations = 1,
  temperature = 0,
  seed = 42,
  model = "gemini-2.5-flash-lite",
  integration = getOption("nalanda.integration"),
  virtual_key = getOption("nalanda.virtual_key"),
  base_url = getOption("nalanda.base_url"),
  excerpt_chars = 200,
  checkpoint_dir = NULL,
  checkpoint_prefix = "simulate_treatment",
  save_dir = NULL,
  save_prefix = "results"
) {
  if (missing(intervention_text) || is.null(intervention_text)) {
    intervention_text <- ""
  }
  if (missing(prompt) || length(prompt) < 1) {
    stop("`prompt` must contain at least one turn.")
  }
  if (missing(response_type) || is.null(response_type)) {
    stop("Please provide `response_type`.")
  }
  output_mode <- normalize_output_mode(output_mode)

  out <- run_simulation_pipeline(
    book_texts = intervention_text,
    groups = groups,
    context_text = context_text,
    question_text = "generic",
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
    executor = execute_generic_treatment_pipeline,
    require_groups = FALSE,
    default_unit_id = "intervention_1",
    checkpoint_dir = checkpoint_dir,
    checkpoint_prefix = checkpoint_prefix,
    save_dir = save_dir,
    save_prefix = save_prefix,
    prompt = as.character(prompt),
    response_type = response_type,
    output_mode = output_mode
  )

  if (is.list(out) && !inherits(out, "data.frame")) {
    for (nm in names(out)) {
      out[[nm]] <- rename_treatment_output_columns(out[[nm]])
    }
  }
  out <- rename_treatment_output_columns(out)
  out
}

rename_treatment_output_columns <- function(x) {
  if (!inherits(x, "data.frame")) {
    return(x)
  }

  if ("chapter" %in% names(x) && !"treatment" %in% names(x)) {
    x <- dplyr::rename(x, treatment = "chapter")
  }

  if ("chapter_index" %in% names(x) && !"treatment_index" %in% names(x)) {
    x <- dplyr::rename(x, treatment_index = "chapter_index")
  }

  x
}

execute_generic_treatment_pipeline <- function(
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
  prompt,
  response_type,
  output_mode
) {
  turn_labels <- names(prompt)
  if (is.null(turn_labels) || any(!nzchar(turn_labels))) {
    turn_labels <- paste0("turn_", seq_along(prompt))
  }

  identities <- if (all(is.na(groups))) NA_character_ else groups
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

    for (id_idx in seq_along(identities)) {
      identity_label <- identities[[id_idx]]
      identity_context <- context_text[[id_idx]]

      for (k in seq_len(n_simulations)) {
        unit_start <- all_row_i + 1L
        progress_tick(
          book = chapter_job$book[[1]],
          chapter = chapter_job$chapter[[1]],
          identity = if (is.na(identity_label)) "default" else identity_label,
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

        chat <- new_portkey_chat(
          model = model,
          base_url = base_url,
          temperature = temperature,
          seed = seed + k - 1L
        )

        for (turn_idx in seq_along(prompt)) {
          full_prompt <- make_treatment_prompt(
            prompt_template = prompt[[turn_idx]],
            intervention_text = chapter_job$chapter_text[[1]],
            identity_context = identity_context,
            identity_label = identity_label
          )
          prompt_preview <- make_treatment_prompt(
            prompt_template = prompt[[turn_idx]],
            intervention_text = compact_chapter_text(
              chapter_job$chapter_text[[1]],
              excerpt_chars = excerpt_chars
            ),
            identity_context = identity_context,
            identity_label = identity_label
          )
          response <- chat_model_response(
            chat = chat,
            prompt = full_prompt,
            type = response_type,
            output_mode = output_mode,
            fields = infer_response_type_fields(response_type)
          )

          base_fields <- make_result_base_fields(
            book = chapter_job$book[[1]],
            chapter = chapter_job$chapter[[1]],
            sim = k,
            identity = if (is.na(identity_label)) NA_character_ else identity_label,
            party = NULL,
            extra = list(
              turn_index = turn_idx,
              turn_type = turn_labels[[turn_idx]],
              prompt = prompt_preview
            )
          )

          row <- c(base_fields, as.list(response))
          row$party <- NULL

          all_row_i <- all_row_i + 1L
          all_rows[[all_row_i]] <- row
        }

        write_simulation_checkpoint(
          rows = all_rows[unit_start:all_row_i],
          long_cols = "prompt",
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
        long_cols = "prompt",
        chapter_jobs = chapter_jobs,
        save_dir = save_dir,
        save_prefix = save_prefix,
        n_models = n_models,
        model_label = model_label,
        temperature = temperature,
        n_simulations = n_simulations,
        book = chapter_job$book[[1]],
        column_renamer = rename_treatment_output_columns
      )
    }
  }

  finalize_simulation_output(
    out_rows = all_rows,
    long_cols = "prompt",
    chapter_jobs = chapter_jobs
  )
}

#' Build a concrete prompt for `simulate_treatment()`
#'
#' This helper expands a single `simulate_treatment()` prompt template into a
#' concrete prompt string. It is useful for inspecting prompt wording before
#' launching a run, much like [make_baseline_prompt()] and [make_post_prompt()]
#' are useful for `run_ai_on_chapters()`.
#'
#' @param prompt_template Character scalar. A single prompt template that may
#'   include `{intervention_text}`, `{identity}`, and `{group}`.
#' @param intervention_text Character scalar. The intervention text to insert
#'   into `{intervention_text}`. Invalid multibyte text is repaired as UTF-8,
#'   with Windows-1252 used as the primary fallback encoding.
#' @param identity_context Character scalar. Optional identity context to
#'   prepend to the prompt.
#' @param identity_label Character scalar. Optional identity label used to
#'   expand `{identity}` and `{group}`.
#'
#' @return A character scalar containing the concrete prompt.
#'
#' @examples
#' make_treatment_prompt(
#'   prompt_template = "{intervention_text}\n\nRate this as {identity}.",
#'   intervention_text = "A short climate message.",
#'   identity_context = "You are simulating an American adult.",
#'   identity_label = "American"
#' )
#' @export
make_treatment_prompt <- function(
  prompt_template,
  intervention_text,
  identity_context = "",
  identity_label = NA_character_
) {
  out <- repair_chapter_text_encoding(prompt_template)
  intervention_text <- repair_chapter_text_encoding(intervention_text)
  identity_context <- repair_chapter_text_encoding(identity_context)

  out <- gsub("{intervention_text}", intervention_text, out, fixed = TRUE)
  if (!is.na(identity_label)) {
    identity_label <- repair_chapter_text_encoding(identity_label)
    out <- gsub("{identity}", identity_label, out, fixed = TRUE)
    out <- gsub("{group}", identity_label, out, fixed = TRUE)
  }

  out <- paste(identity_context, out)
  trimws(out)
}

#' @rdname make_treatment_prompt
#' @export
build_simulate_treatment_prompt <- function(
  prompt_template,
  intervention_text,
  identity_context = "",
  identity_label = NA_character_
) {
  make_treatment_prompt(
    prompt_template = prompt_template,
    intervention_text = intervention_text,
    identity_context = identity_context,
    identity_label = identity_label
  )
}
