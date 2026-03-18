#' Simulate a generic multi-turn treatment workflow
#'
#' This function provides a simpler, prompt-first interface for running one or
#' more turns against an intervention text. Each element of `prompt` defines one
#' turn in the chat sequence. When `groups` is supplied, the same prompt
#' sequence is repeated for each group identity; groups do not create additional
#' turns.
#'
#' @param intervention_text A single character string or a nested list of
#'   intervention texts. This is mapped internally onto the same job grid used
#'   by [run_ai_on_chapters()]. Defaults to `""`, which is useful when the full
#'   treatment is already encoded in `prompt` and/or `context_text`.
#' @param prompt Character vector of prompt templates. Each element defines one
#'   turn. Prompt templates may include `{intervention_text}`, `{identity}`, and
#'   `{group}` placeholders.
#' @param response_type An `ellmer` structured type specification applied to all
#'   turns (for example `ellmer::type_object(score = ellmer::type_number())`).
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
#' @param integration Optional integration/provider slug. If supplied and
#'   `model` is not fully-qualified, nalanda will build
#'   `"@{integration}/{model}"`.
#' @param virtual_key Optional legacy virtual key. If supplied and `model` is
#'   not fully-qualified, nalanda will build `"@{virtual_key}/{model}"`.
#' @param base_url Character. Base URL for API calls.
#' @param excerpt_chars Integer. Number of intervention-text characters to
#'   retain in stored prompt previews.
#' @param include_tokens Logical. Return token counts if available.
#' @param include_cost Logical. Return cost info if available.
#'
#' @return A tibble of raw turn-level responses, or a named list of tibbles
#'   (one per book/intervention collection). Each row includes `chapter`, `sim`,
#'   `identity`, `turn_index`, `turn_type`, and one column per field returned by
#'   `response_type`, plus stored prompt previews and metadata columns.
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
  include_tokens = FALSE,
  include_cost = FALSE
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

  route <- resolve_model_route(
    integration = integration,
    virtual_key = virtual_key,
    integration_missing = missing(integration),
    virtual_key_missing = missing(virtual_key)
  )
  integration <- route$integration
  virtual_key <- route$virtual_key

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
    base_url = base_url,
    excerpt_chars = excerpt_chars,
    include_tokens = include_tokens,
    include_cost = include_cost,
    executor = execute_generic_treatment_pipeline,
    require_groups = FALSE,
    default_unit_id = "intervention_1",
    prompt = as.character(prompt),
    response_type = response_type
  )

  if (is.list(out) && !inherits(out, "data.frame")) {
    for (nm in names(out)) {
      attr(out[[nm]], "model") <- normalize_model_name(attr(out[[nm]], "model"))
    }
  }
  attr(out, "model") <- normalize_model_name(attr(out, "model"))
  out
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
  include_tokens,
  include_cost,
  pb,
  prompt,
  response_type
) {
  turn_labels <- names(prompt)
  if (is.null(turn_labels) || any(!nzchar(turn_labels))) {
    turn_labels <- paste0("turn_", seq_along(prompt))
  }

  identities <- if (all(is.na(groups))) NA_character_ else groups
  all_rows <- list()
  all_row_i <- 0L

  for (chapter_i in seq_len(nrow(chapter_jobs))) {
    chapter_job <- chapter_jobs[chapter_i, , drop = FALSE]

    for (id_idx in seq_along(identities)) {
      identity_label <- identities[[id_idx]]
      identity_context <- context_text[[id_idx]]

      for (k in seq_len(n_simulations)) {
        what_text <- format_progress_label(
          book = chapter_job$book[[1]],
          chapter = chapter_job$chapter[[1]],
          identity = if (is.na(identity_label)) "default" else identity_label,
          sim = k
        )
        pb$tick(tokens = list(what = what_text))

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
          response <- chat$chat_structured(
            full_prompt,
            type = response_type
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

          row <- attach_usage_fields(
            c(base_fields, as.list(response)),
            response,
            include_tokens,
            include_cost
          )
          row$party <- NULL

          all_row_i <- all_row_i + 1L
          all_rows[[all_row_i]] <- row
        }
      }
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
#'   into `{intervention_text}`.
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
  out <- prompt_template
  out <- gsub("\\{intervention_text\\}", intervention_text, out)
  if (!is.na(identity_label)) {
    out <- gsub("\\{identity\\}", identity_label, out)
    out <- gsub("\\{group\\}", identity_label, out)
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
