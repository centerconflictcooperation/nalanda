#' Run AI model on book chapters and collect structured responses
#'
#' This function implements a two-turn sequential chat design to measure the effect
#' of reading book chapters on attitudes. For each simulation, the function:
#' \enumerate{
#'   \item Establishes a baseline by asking the model to choose a party and rate the outgroup
#'   \item Shows the chapter and asks for a post-intervention rating in the same chat session
#' }
#' This design creates a within-agent pre-post comparison, with conversation memory
#' maintained between turns.
#'
#' @param book_texts A single character (one chapter) or a nested list of books -> chapters as returned by `read_book_texts()`.
#' @param context_text Character vector. Context used in the baseline prompt to establish party identity.
#'   Can be a vector of multiple contexts - the function will run once for each context and combine results.
#'   Example: "You are simulating an American adult who politically identifies as a Democrat."
#' @param question_text Character. Question to ask the model in both baseline and post-intervention turns.
#'   The post-intervention turn will append " after reading this chapter?" to this question.
#'   Example: "On a scale from 0 to 100, how warmly do you feel towards your political outgroup?"
#' @param n_simulations Integer. Number of repeated simulations per chapter (each simulation = 2 chat turns).
#' @param temperature Numeric. Sampling temperature passed to the chat backend.
#' @param seed Integer. Random seed for reproducibility (incremented for each simulation).
#' @param base_model Character. Model name to label results with.
#' @param virtual_key Optional API key/virtual key prefix used by chat_portkey.
#' @param base_url Optional base url for API calls.
#' @param excerpt_chars Integer. Number of characters to keep as excerpt in results.
#' @param include_tokens Logical. Return token counts if available (summed across both turns).
#' @param include_cost Logical. Return cost info if available (summed across both turns).
#' @return A tibble of results, or a named list of tibbles (one per book). Each row represents
#'   one simulation and includes: `chapter`, `sim`, `party`, `baseline_score` (pre-intervention),
#'   `score` (post-intervention), `chapter_excerpt`, `context`, and `question`.
#'   The object has class `nalanda` and an attribute `model` with the model name.
#' @export
run_ai_on_chapters <- function(
  book_texts,
  context_text,
  question_text,
  n_simulations = 1,
  temperature = 0,
  seed = 42,
  base_model = "gemini-2.5-flash-lite",
  virtual_key = getOption("nalanda.virtual_key"),
  base_url = getOption("nalanda.base_url"),
  excerpt_chars = 200,
  include_tokens = FALSE,
  include_cost = FALSE
) {
  if (missing(context_text) || missing(question_text)) {
    stop("Please provide both `context_text` and `question_text`.")
  }
  if (n_simulations < 1) {
    stop("`n_simulations` must be >= 1.")
  }

  # Handle multiple contexts by running the core function for each and combining
  if (length(context_text) > 1) {
    results <- lapply(seq_along(context_text), function(k) {
      .run_ai_single_context(
        book_texts = book_texts,
        context_text = context_text[k],
        question_text = question_text,
        n_simulations = n_simulations,
        temperature = temperature,
        seed = seed,
        virtual_key = virtual_key,
        base_model = base_model,
        base_url = base_url,
        excerpt_chars = excerpt_chars,
        include_tokens = include_tokens,
        include_cost = include_cost
      )
    })

    # Combine results appropriately
    # First, strip the nalanda class from individual results to avoid bind_rows issues
    results_clean <- lapply(results, function(res) {
      if (is.list(res) && !inherits(res, "data.frame")) {
        # List of tibbles (one per book)
        lapply(res, function(tbl) {
          class(tbl) <- setdiff(class(tbl), "nalanda")
          tbl
        })
      } else {
        # Single tibble
        class(res) <- setdiff(class(res), "nalanda")
        res
      }
    })

    first_res <- results_clean[[1]]
    if (is.list(first_res) && !inherits(first_res, "data.frame")) {
      # Results are lists of tibbles (one per book)
      book_names <- names(first_res)
      combined <- lapply(seq_along(first_res), function(i) {
        dplyr::bind_rows(lapply(results_clean, function(r) r[[i]]))
      })
      if (!is.null(book_names)) {
        names(combined) <- book_names
      }
      out <- combined
    } else {
      # Results are single tibbles
      out <- dplyr::bind_rows(results_clean)
    }
    class(out) <- c(class(out), "nalanda")
    attr(out, "model") <- base_model
    return(out)
  }

  # Single context - call the internal function
  .run_ai_single_context(
    book_texts = book_texts,
    context_text = context_text,
    question_text = question_text,
    n_simulations = n_simulations,
    temperature = temperature,
    seed = seed,
    virtual_key = virtual_key,
    base_model = base_model,
    base_url = base_url,
    excerpt_chars = excerpt_chars,
    include_tokens = include_tokens,
    include_cost = include_cost
  )
}

#' Internal function to run AI on chapters with a single context
#'
#' @keywords internal
#' @noRd
.run_ai_single_context <- function(
  book_texts,
  context_text,
  question_text,
  n_simulations,
  temperature,
  seed,
  base_model,
  virtual_key,
  base_url,
  excerpt_chars,
  include_tokens,
  include_cost
) {
  model <- paste0("@", virtual_key, "/", base_model)

  # Create prompt functions for the two-turn design
  make_baseline_prompt <- function() {
    paste(
      context_text,
      "\n\n",
      question_text,
      sep = ""
    )
  }

  make_post_prompt <- function(chapter_text) {
    paste(
      "You have just read the chapter below.\n\n",
      chapter_text,
      "\n\n",
      question_text,
      " after reading this chapter?",
      sep = ""
    )
  }

  # Type schemas for structured responses
  type_baseline <- ellmer::type_object(
    party = ellmer::type_string(),
    score = ellmer::type_number()
  )

  type_post <- ellmer::type_object(
    score = ellmer::type_number()
  )

  if (is.character(book_texts) && length(book_texts) == 1) {
    chapter_text <- book_texts[[1]]
    chapter_id <- if (!is.null(names(book_texts))) {
      names(book_texts)[1]
    } else {
      "chapter_1"
    }
    excerpt <- substr(chapter_text, 1, excerpt_chars)

    # Run simulations sequentially (each simulation = 2 turns in same chat)
    results <- lapply(seq_len(n_simulations), function(i) {
      # Create a new chat instance for this simulation
      chat <- ellmer::chat_portkey(
        model = model,
        base_url = base_url,
        params = ellmer::params(temperature = temperature, seed = seed + i - 1),
        api_args = list(temperature = temperature, seed = seed + i - 1)
      )

      # Turn 1: Baseline (establish party and get initial score)
      baseline_prompt <- make_baseline_prompt()
      baseline_response <- chat$chat_structured(
        baseline_prompt,
        type = type_baseline
      )

      # Turn 2: Post-intervention (show chapter and get final score)
      post_prompt <- make_post_prompt(chapter_text)
      post_response <- chat$chat_structured(
        post_prompt,
        type = type_post
      )

      # Combine results
      list(
        party = baseline_response$party,
        baseline_score = baseline_response$score,
        score = post_response$score,
        input_tokens = if (
          include_tokens && !is.null(baseline_response$input_tokens)
        ) {
          baseline_response$input_tokens + post_response$input_tokens
        } else {
          NA_real_
        },
        cost = if (include_cost && !is.null(baseline_response$cost)) {
          baseline_response$cost + post_response$cost
        } else {
          NA_real_
        }
      )
    })

    # Convert results to tibble
    out <- tibble::tibble(
      chapter = chapter_id,
      sim = seq_len(n_simulations),
      party = sapply(results, function(r) r$party),
      baseline_score = sapply(results, function(r) r$baseline_score),
      score = sapply(results, function(r) r$score),
      chapter_excerpt = excerpt,
      context = context_text,
      question = question_text
    )

    if (include_tokens) {
      out$input_tokens <- sapply(results, function(r) r$input_tokens)
    }
    if (include_cost) {
      out$cost <- sapply(results, function(r) r$cost)
    }

    attr(out, "model") <- base_model
    out
  } else if (is.list(book_texts)) {
    book_names <- names(book_texts)
    out_list <- purrr::map(seq_along(book_texts), function(i) {
      book <- if (is.null(book_names)) paste0("book_", i) else book_names[[i]]
      chapters <- book_texts[[i]]
      chapter_texts <- unlist(chapters, use.names = TRUE)
      df_book <- purrr::map_dfr(seq_along(chapter_texts), function(j) {
        chapter_id <- names(chapter_texts)[j]
        chapter_text <- chapter_texts[[j]]
        excerpt <- substr(chapter_text, 1, excerpt_chars)

        # Run simulations sequentially for this chapter
        results <- lapply(seq_len(n_simulations), function(k) {
          # Create a new chat instance for this simulation
          chat <- ellmer::chat_portkey(
            model = model,
            base_url = base_url,
            params = ellmer::params(
              temperature = temperature,
              seed = seed + k - 1
            ),
            api_args = list(temperature = temperature, seed = seed + k - 1)
          )

          # Turn 1: Baseline
          baseline_prompt <- make_baseline_prompt()
          baseline_response <- chat$chat_structured(
            baseline_prompt,
            type = type_baseline
          )

          # Turn 2: Post-intervention
          post_prompt <- make_post_prompt(chapter_text)
          post_response <- chat$chat_structured(
            post_prompt,
            type = type_post
          )

          # Combine results
          list(
            party = baseline_response$party,
            baseline_score = baseline_response$score,
            score = post_response$score,
            input_tokens = if (
              include_tokens && !is.null(baseline_response$input_tokens)
            ) {
              baseline_response$input_tokens + post_response$input_tokens
            } else {
              NA_real_
            },
            cost = if (include_cost && !is.null(baseline_response$cost)) {
              baseline_response$cost + post_response$cost
            } else {
              NA_real_
            }
          )
        })

        # Convert results to tibble
        df <- tibble::tibble(
          book = book,
          chapter = chapter_id,
          sim = seq_len(n_simulations),
          party = sapply(results, function(r) r$party),
          baseline_score = sapply(results, function(r) r$baseline_score),
          score = sapply(results, function(r) r$score),
          chapter_excerpt = excerpt,
          context = context_text,
          question = question_text
        )

        if (include_tokens) {
          df$input_tokens <- sapply(results, function(r) r$input_tokens)
        }
        if (include_cost) {
          df$cost <- sapply(results, function(r) r$cost)
        }

        attr(df, "model") <- base_model
        df
      })
      attr(df_book, "model") <- base_model
      df_book
    })
    if (!is.null(book_names)) {
      names(out_list) <- book_names
    }
    out <- out_list
  } else {
    stop(
      "`book_texts` must be either a single chapter text or a nested list like `read_book_texts()`."
    )
  }
  class(out) <- c(class(out), "nalanda")
  attr(out, "model") <- base_model
  out
}
