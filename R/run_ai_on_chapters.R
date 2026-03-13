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
#' @param virtual_key Optional legacy provider/integration key. Should look
#'   like `"gemini-8c2498"` or similar. If supplied and `model` is not
#'   fully-qualified (does not start with `"@"`), nalanda will build
#'   `"@{virtual_key}/{model}"`.
#' @param base_url Character. Base URL for API calls.
#' @param excerpt_chars Integer. Number of characters to keep as excerpt in
#'   results.
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
#' @return A tibble of results, or a named list of tibbles (one per book). Each
#'   row represents one simulation x identity combination and includes:
#'   `chapter`, `sim`, `identity`, `party`, and (depending on mode):
#'   \describe{
#'     \item{Per-group mode (`{group}` in question)}{Per-group raw ratings
#'       (`pre_rating_{group}`, `post_rating_{group}`), computed
#'       `pre_ingroup`, `pre_outgroup`, `post_ingroup`, `post_outgroup`,
#'       and difference scores: `pre_gap`, `post_gap`, `delta_ingroup`,
#'       `delta_outgroup`, and `delta_gap`.}
#'     \item{Single-question mode (no `{group}`)}{`pre_rating`, `post_rating`,
#'       `pre_outgroup`, `post_outgroup` (= ratings), `pre_ingroup = NA`,
#'       `post_ingroup = NA`, and `delta_outgroup`.}
#'   }
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
  if (!startsWith(model, "@") && !is.null(virtual_key) && nzchar(virtual_key)) {
    model <- paste0("@", virtual_key, "/", model)
  }

  # --- Normalise group labels for column names ---
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
    excerpt <- substr(chapter_text, 1, excerpt_chars)

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
                "Please provide `virtual_key` (or set ",
                "`options(nalanda.virtual_key=...)`), which is currently ",
                "Nalanda's officially supported method. ",
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
        post_prompt <- make_post_prompt(
          chapter_text,
          question_text,
          groups,
          identity_label
        )
        post_response <- chat$chat_structured(
          post_prompt,
          type = type_post
        )

        # --- Assemble row data ---
        row <- list(
          chapter = chapter_id,
          sim = k,
          identity = identity_label,
          party = baseline_response$party,
          baseline_prompt = baseline_prompt,
          post_prompt = post_prompt
        )

        if (per_group) {
          # Raw per-group ratings (pre and post)
          for (gk in group_keys) {
            field <- paste0("rating_", gk)
            row[[paste0("pre_", field)]] <- baseline_response[[field]]
            row[[paste0("post_", field)]] <- post_response[[field]]
          }

          # Post-hoc ingroup/outgroup computation
          ingroup_key <- paste0("rating_", group_keys[id_idx])
          outgroup_keys <- setdiff(
            paste0("rating_", group_keys),
            ingroup_key
          )

          row$pre_ingroup <- baseline_response[[ingroup_key]]
          row$post_ingroup <- post_response[[ingroup_key]]
          # For 2 groups, outgroup is the other one; for >2, average
          row$pre_outgroup <- mean(vapply(
            outgroup_keys,
            function(ok) baseline_response[[ok]],
            numeric(1)
          ))
          row$post_outgroup <- mean(vapply(
            outgroup_keys,
            function(ok) post_response[[ok]],
            numeric(1)
          ))
        } else {
          # Single-question mode: rating = outgroup, ingroup = NA
          row$pre_rating <- baseline_response$rating
          row$post_rating <- post_response$rating
          row$pre_ingroup <- NA_real_
          row$post_ingroup <- NA_real_
          row$pre_outgroup <- baseline_response$rating
          row$post_outgroup <- post_response$rating
        }

        # Tokens and cost
        if (include_tokens) {
          row$input_tokens <- if (!is.null(baseline_response$input_tokens)) {
            baseline_response$input_tokens + post_response$input_tokens
          } else {
            NA_real_
          }
        }
        if (include_cost) {
          row$cost <- if (!is.null(baseline_response$cost)) {
            baseline_response$cost + post_response$cost
          } else {
            NA_real_
          }
        }

        all_rows <- c(all_rows, list(row))
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

    # Add computed difference columns
    if (per_group) {
      out_tbl$pre_gap <- out_tbl$pre_ingroup - out_tbl$pre_outgroup
      out_tbl$post_gap <- out_tbl$post_ingroup - out_tbl$post_outgroup
      out_tbl$delta_ingroup <- out_tbl$post_ingroup - out_tbl$pre_ingroup
      out_tbl$delta_outgroup <- out_tbl$post_outgroup - out_tbl$pre_outgroup
      out_tbl$delta_gap <- out_tbl$delta_outgroup - out_tbl$delta_ingroup
    } else {
      out_tbl$delta_outgroup <- out_tbl$post_outgroup - out_tbl$pre_outgroup
    }

    # Add metadata columns
    out_tbl$chapter_excerpt <- excerpt

    if (!is.null(book)) {
      out_tbl <- tibble::add_column(out_tbl, book = book, .before = 1)
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
      attr(book_tbl, "model") <- model
      attr(book_tbl, "temperature") <- temperature
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
  attr(out, "model") <- model
  attr(out, "temperature") <- temperature
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
