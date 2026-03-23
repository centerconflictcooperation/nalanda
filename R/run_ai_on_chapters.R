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
#' @param n_simulations Integer. Number of repeated simulations per chapter per
#'   identity (each simulation = 2 chat turns).
#' @param temperature Numeric. Sampling temperature passed to the chat backend.
#' @param seed Integer. Random seed for reproducibility (incremented for each
#'   simulation).
#' @param model Character. Model name for the chat backend (for example,
#'   `"gemini-2.5-flash-lite"`). The value is passed directly to
#'   `ellmer::chat_portkey(model = ...)`.
#' @param integration Optional integration/provider slug. Should look like
#'   `"vertexai"` or similar. If supplied and `model` is not fully-qualified
#'   (does not start with `"@"`), nalanda will build `"@{integration}/{model}"`.
#'   Preferred for new Portkey/NYU setups. When both `nalanda.integration` and
#'   `nalanda.virtual_key` options are set and neither argument is supplied,
#'   `integration` is preferred.
#' @param virtual_key Optional legacy virtual key. Should look like
#'   `"gemini-8c2498"` or similar. If supplied and `model` is not
#'   fully-qualified, nalanda will build `"@{virtual_key}/{model}"`.
#'   Use either `integration` or `virtual_key`, not both when explicitly
#'   supplying function arguments.
#' @param base_url Character. Base URL for API calls.
#' @param excerpt_chars Integer. Number of chapter characters to retain in the
#'   stored post-prompt preview shown in results.
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
  n_simulations = 1,
  temperature = 0,
  seed = 42,
  model = "gemini-2.5-flash-lite",
  integration = getOption("nalanda.integration"),
  virtual_key = getOption("nalanda.virtual_key"),
  base_url = getOption("nalanda.base_url"),
  excerpt_chars = 200
) {
  route <- resolve_model_route(
    integration = integration,
    virtual_key = virtual_key,
    integration_missing = missing(integration),
    virtual_key_missing = missing(virtual_key)
  )
  integration <- route$integration
  virtual_key <- route$virtual_key

  if (is.list(book_texts)) {
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
    base_url = base_url,
    excerpt_chars = excerpt_chars,
    executor = execute_two_turn_pipeline
  )

  if (is.list(out) && !inherits(out, "data.frame")) {
    for (nm in names(out)) {
      attr(out[[nm]], "model") <- normalize_model_name(attr(out[[nm]], "model"))
    }
  }
  attr(out, "model") <- normalize_model_name(attr(out, "model"))
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
  pb,
  progress_tick
) {
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

  for (chapter_i in seq_len(nrow(chapter_jobs))) {
    chapter_job <- chapter_jobs[chapter_i, , drop = FALSE]

    for (id_idx in seq_along(groups)) {
      identity_label <- groups[[id_idx]]
      identity_context <- context_text[[id_idx]]

      for (k in seq_len(n_simulations)) {
        progress_tick(
          book = chapter_job$book[[1]],
          chapter = chapter_job$chapter[[1]],
          identity = identity_label,
          sim = k
        )

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

        base_fields <- make_result_base_fields(
          book = chapter_job$book[[1]],
          chapter = chapter_job$chapter[[1]],
          sim = k,
          identity = identity_label,
          party = baseline_response$party,
          extra = list(
            baseline_prompt = baseline_prompt,
            post_prompt = post_prompt
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
      }
    }
  }

  finalize_simulation_output(
    out_rows = all_rows,
    long_cols = c("baseline_prompt", "post_prompt"),
    chapter_jobs = chapter_jobs
  )
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
#' Constructs the prompt: chapter text + question(s). If the question
#' template contains `{group}`, it is expanded once per group (ingroup first).
#' Otherwise, the question is used as-is (single-question mode).
#'
#' @param chapter_text Character scalar. The full chapter text.
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
    "You have just read the chapter below.\n\n",
    chapter_text,
    "\n\n",
    "You have now just finished reading the book chapter. Now that this is done:\n",
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
