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
#'   Preferred for new Portkey/NYU setups.
#' @param virtual_key Optional legacy virtual key. Should look like
#'   `"gemini-8c2498"` or similar. If supplied and `model` is not
#'   fully-qualified, nalanda will build `"@{virtual_key}/{model}"`.
#'   Use either `integration` or `virtual_key`, not both.
#' @param base_url Character. Base URL for API calls.
#' @param excerpt_chars Integer. Number of chapter characters to retain in the
#'   stored post-prompt preview shown in results.
#' @param include_tokens Logical. Return token counts if available (summed
#'   across both turns).
#' @param include_cost Logical. Return cost info if available (summed across
#'   both turns).
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
  excerpt_chars = 200,
  include_tokens = FALSE,
  include_cost = FALSE
) {
  # --- Input validation ---
  if (missing(groups) || length(groups) < 2) {
    stop("`groups` must be a character vector with at least 2 group labels.")
  }
  if (missing(question_text) || length(question_text) != 1) {
    stop("`question_text` must be a single character string.")
  }

  # Detect mode: per-group (question asked per group) vs single-question
  per_group <- grepl("\\{group\\}", question_text)
  if (missing(context_text)) {
    stop("Please provide `context_text`.")
  }
  if (n_simulations < 1) {
    stop("`n_simulations` must be >= 1.")
  }
  if (!is.null(integration) && nzchar(integration) &&
      !is.null(virtual_key) && nzchar(virtual_key)) {
    stop("Please provide only one of `integration` or `virtual_key`.")
  }

  # --- Expand context_text from template if scalar ---
  if (length(context_text) == 1 && grepl("\\{identity\\}", context_text)) {
    context_text <- vapply(
      groups,
      function(g) {
        gsub("\\{identity\\}", g, context_text)
      },
      character(1),
      USE.NAMES = FALSE
    )
  }
  if (length(context_text) != length(groups)) {
    stop(
      "`context_text` must be either a scalar template with `{identity}` or ",
      "a vector of length equal to `length(groups)` (",
      length(groups),
      ")."
    )
  }

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

  # --- Calculate total steps for progress bar ---
  total_chapters <- 0
  if (is.character(book_texts)) {
    total_chapters <- 1
  } else if (is.list(book_texts)) {
    total_chapters <- sum(vapply(book_texts, length, integer(1)))
  }

  total_steps <- total_chapters * n_simulations * length(groups)

  pb <- progress::progress_bar$new(
    format = "  running [:bar] :percent eta: :eta :what",
    total = total_steps,
    clear = FALSE,
    width = 60
  )

  # --- Resolve model name ---
  if (identical(model, "")) {
    stop("`model` must be a non-empty string.")
  }
  if (!startsWith(model, "@")) {
    prefix <- NULL
    if (!is.null(integration) && nzchar(integration)) {
      prefix <- integration
    } else if (!is.null(virtual_key) && nzchar(virtual_key)) {
      prefix <- virtual_key
    }
    if (!is.null(prefix)) {
      model <- paste0("@", prefix, "/", model)
    }
  }

  # --- Normalise group labels for structured field names ---
  group_keys <- tolower(gsub(" ", "_", groups))

  # --- Build dynamic structured schemas from groups ---
  if (per_group) {
    # Per-group mode: one rating field per group
    baseline_fields <- list(party = ellmer::type_string())
    for (gk in group_keys) {
      baseline_fields[[paste0("rating_", gk)]] <- ellmer::type_number()
    }
    type_baseline <- do.call(ellmer::type_object, baseline_fields)

    post_fields <- list()
    for (gk in group_keys) {
      post_fields[[paste0("rating_", gk)]] <- ellmer::type_number()
    }
    type_post <- do.call(ellmer::type_object, post_fields)
  } else {
    # Single-question mode: one rating field
    type_baseline <- ellmer::type_object(
      party = ellmer::type_string(),
      rating = ellmer::type_number()
    )
    type_post <- ellmer::type_object(
      rating = ellmer::type_number()
    )
  }

  # --- Core simulation helper (closure over all params) ---
  run_simulate_chapter <- function(chapter_text, chapter_id, book = NULL) {
    chapter_excerpt <- chapter_text
    # Run one simulation for each identity assignment × n_simulations
    all_rows <- list()

    for (id_idx in seq_along(groups)) {
      identity_label <- groups[id_idx]
      identity_context <- context_text[id_idx]

      for (k in seq_len(n_simulations)) {
        # Progress bar
        what_text <- if (!is.null(book)) {
          paste0(
            book,
            " - ",
            chapter_id,
            " [",
            identity_label,
            "] (sim ",
            k,
            ")"
          )
        } else {
          paste0(chapter_id, " [", identity_label, "] (sim ", k, ")")
        }
        pb$tick(tokens = list(what = what_text))

        # Fresh chat instance per simulation
        chat <- tryCatch(
          ellmer::chat_portkey(
            model = model,
            base_url = base_url,
            params = ellmer::params(
              temperature = temperature,
              seed = seed + k - 1
            ),
            api_args = list(temperature = temperature, seed = seed + k - 1)
          ),
          error = function(e) {
            msg <- conditionMessage(e)
            if (grepl("PORTKEY_VIRTUAL_KEY", msg, fixed = TRUE)) {
              stop(
                "Please provide `integration` (or set ",
                "`options(nalanda.integration=...)`). ",
                "If you are still using the older NYU setup, `virtual_key` ",
                "and `options(nalanda.virtual_key=...)` are also supported. ",
                "Your installed `ellmer` expects `PORTKEY_VIRTUAL_KEY` when ",
                "`model` is not fully-qualified. ",
                "Alternative: use a fully-qualified model string in ",
                "`model` (e.g., '@provider/model') or update ",
                "`ellmer`.",
                call. = FALSE
              )
            }
            stop(e)
          }
        )

        # --- Turn 1: Baseline ---
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

        # --- Turn 2: Post-intervention ---
        full_post_prompt <- make_post_prompt(
          chapter_text,
          question_text,
          groups,
          identity_label
        )
        post_prompt <- make_post_prompt_preview(
          chapter_text = chapter_text,
          question_template = question_text,
          groups = groups,
          identity_label = identity_label,
          excerpt_chars = excerpt_chars
        )
        post_response <- chat$chat_structured(
          full_post_prompt,
          type = type_post
        )

        # --- Assemble long-format raw rows ---
        base_fields <- list(
          chapter = chapter_id,
          sim = k,
          identity = identity_label,
          party = baseline_response$party,
          baseline_prompt = baseline_prompt,
          post_prompt = post_prompt
        )

        if (per_group) {
          for (g_idx in seq_along(groups)) {
            g <- groups[[g_idx]]
            gk <- group_keys[[g_idx]]
            field <- paste0("rating_", gk)

            row_pre <- c(
              base_fields,
              list(
                turn_index = 1L,
                turn_type = "baseline",
                target_group = g,
                rating = baseline_response[[field]]
              )
            )
            row_post <- c(
              base_fields,
              list(
                turn_index = 2L,
                turn_type = "post",
                target_group = g,
                rating = post_response[[field]]
              )
            )

            if (include_tokens) {
              row_pre$input_tokens <- if (!is.null(baseline_response$input_tokens)) {
                baseline_response$input_tokens
              } else {
                NA_real_
              }
              row_post$input_tokens <- if (!is.null(post_response$input_tokens)) {
                post_response$input_tokens
              } else {
                NA_real_
              }
            }
            if (include_cost) {
              row_pre$cost <- if (!is.null(baseline_response$cost)) {
                baseline_response$cost
              } else {
                NA_real_
              }
              row_post$cost <- if (!is.null(post_response$cost)) {
                post_response$cost
              } else {
                NA_real_
              }
            }

            all_rows <- c(all_rows, list(row_pre, row_post))
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

          if (include_tokens) {
            row_pre$input_tokens <- if (!is.null(baseline_response$input_tokens)) {
              baseline_response$input_tokens
            } else {
              NA_real_
            }
            row_post$input_tokens <- if (!is.null(post_response$input_tokens)) {
              post_response$input_tokens
            } else {
              NA_real_
            }
          }
          if (include_cost) {
            row_pre$cost <- if (!is.null(baseline_response$cost)) {
              baseline_response$cost
            } else {
              NA_real_
            }
            row_post$cost <- if (!is.null(post_response$cost)) {
              post_response$cost
            } else {
              NA_real_
            }
          }

          all_rows <- c(all_rows, list(row_pre, row_post))
        }
      }
    }

    # Convert list of rows to tibble
    out_tbl <- tibble::as_tibble(do.call(
      rbind.data.frame,
      lapply(
        all_rows,
        function(r) as.data.frame(r, stringsAsFactors = FALSE)
      )
    ))

    if (!is.null(book)) {
      out_tbl <- tibble::add_column(out_tbl, book = book, .before = 1)
    }

    attr(out_tbl, "chapter_excerpts") <- tibble::tibble(
      chapter = chapter_id,
      chapter_excerpt = chapter_excerpt,
      book = if (is.null(book)) NA_character_ else book
    )

    long_cols <- c("baseline_prompt", "post_prompt")
    present_long <- intersect(long_cols, names(out_tbl))
    if (length(present_long) > 0) {
      out_tbl <- out_tbl[, c(setdiff(names(out_tbl), present_long), present_long)]
    }

    out_tbl
  }

  # --- Dispatch based on book_texts type ---
  if (is.character(book_texts) && length(book_texts) == 1) {
    chapter_text <- book_texts[[1]]
    chapter_id <- if (!is.null(names(book_texts))) {
      names(book_texts)[1]
    } else {
      "chapter_1"
    }
    out <- run_simulate_chapter(chapter_text, chapter_id)
  } else if (is.list(book_texts)) {
    book_names <- names(book_texts)
    out_list <- purrr::map(seq_along(book_texts), function(i) {
      book <- if (is.null(book_names)) paste0("book_", i) else book_names[[i]]
      chapter_texts <- unlist(book_texts[[i]], use.names = TRUE)

      book_tbl <- purrr::map_dfr(seq_along(chapter_texts), function(j) {
        run_simulate_chapter(
          chapter_text = chapter_texts[[j]],
          chapter_id = names(chapter_texts)[j],
          book = book
        )
      })

      # Set attributes on each book tibble so they survive individual saveRDS
      attr(book_tbl, "model") <- normalize_model_name(model)
      attr(book_tbl, "temperature") <- temperature
      attr(book_tbl, "n_simulations") <- n_simulations
      attr(book_tbl, "chapter_excerpts") <- tibble::tibble(
        book = book,
        chapter = names(chapter_texts),
        chapter_excerpt = unname(chapter_texts)
      )
      book_tbl
    })

    if (!is.null(book_names)) {
      names(out_list) <- book_names
    }
    out <- out_list
  } else {
    stop(
      "`book_texts` must be either a single chapter text or ",
      "a nested list like `read_book_texts()`."
    )
  }

  class(out) <- c(class(out), "nalanda")
  attr(out, "model") <- normalize_model_name(model)
  attr(out, "temperature") <- temperature
  attr(out, "n_simulations") <- n_simulations
  out
}

# --- Prompt builders ---

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
    # Per-group mode: ingroup first, then outgroups
    ordered_groups <- c(
      identity_label,
      setdiff(groups, identity_label)
    )
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
    # Single-question mode
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
    # Per-group mode: ingroup first, then outgroups
    ordered_groups <- c(
      identity_label,
      setdiff(groups, identity_label)
    )
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
    # Single-question mode
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
