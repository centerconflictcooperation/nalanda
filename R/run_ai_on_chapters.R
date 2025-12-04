#' Run AI model on book chapters and collect structured responses
#'
#' This wraps model/chat utilities to run the same question against one or more
#' chapter texts and return a tibble (or list of tibbles by book) with predictions.
#'
#' @param book_texts A single character (one chapter) or a nested list of books -> chapters as returned by `read_book_texts()`.
#' @param context_text Character. Context to prepend to each chapter when prompting.
#' @param question_text Character. Question to ask the model.
#' @param n_simulations Integer. Number of repeated prompts/simulations per chapter.
#' @param temperature Numeric. Sampling temperature passed to the chat backend.
#' @param seed Integer. Random seed for reproducibility.
#' @param base_model Character. Model name to label results with.
#' @param virtual_key Optional API key/virtual key prefix used by chat_portkey.
#' @param base_url Optional base url for API calls.
#' @param excerpt_chars Integer. Number of characters to keep as excerpt in results.
#' @param include_tokens Logical. Return token counts if available.
#' @param include_cost Logical. Return cost info if available.
#' @return A tibble of results, or a named list of tibbles (one per book). The object
#'   has class `nalanda` and an attribute `model` with the model name.
#' @export
run_ai_on_chapters <- function(
  book_texts,
  context_text,
  question_text,
  n_simulations = 1,
  temperature = 0,
  seed = 42,
  base_model = "gemini-2.5-flash-lite",
  virtual_key = NULL,
  base_url = NULL,
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
  if (length(context_text) > 1) {
    results <- lapply(seq_along(context_text), function(k) {
      Recall(
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
    first_res <- results[[1]]
    if (is.list(first_res) && !inherits(first_res, "data.frame")) {
      book_names <- names(first_res)
      combined <- lapply(seq_along(first_res), function(i) {
        dplyr::bind_rows(lapply(results, function(r) r[[i]]))
      })
      if (!is.null(book_names)) {
        names(combined) <- book_names
      }
      out <- combined
    } else {
      out <- dplyr::bind_rows(results)
    }
  }
  model <- paste0("@", virtual_key, "/", base_model)
  chat <- ellmer::chat_portkey(
    model = model,
    base_url = base_url,
    params = ellmer::params(temperature = temperature, seed = seed),
    api_args = list(temperature = temperature, seed = seed)
  )
  make_prompt <- function(chapter_text) {
    paste(
      context_text,
      "\n\n",
      chapter_text,
      "\n\nNow answer this question:\n",
      question_text,
      "\n\nAnswer with a number only:",
      sep = ""
    )
  }
  type_score <- ellmer::type_object(
    party = ellmer::type_string(),
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
    full_prompt <- make_prompt(chapter_text)
    prompts <- as.list(rep(full_prompt, n_simulations))
    reps <- ellmer::parallel_chat_structured(
      chat = chat,
      prompts = prompts,
      type = type_score,
      include_tokens = include_tokens,
      include_cost = include_cost
    )
    out <- tibble::tibble(
      chapter = chapter_id,
      sim = seq_len(n_simulations),
      party = reps$party,
      score = reps$score,
      chapter_excerpt = excerpt,
      context = context_text,
      question = question_text
    )
    if (include_tokens && "input_tokens" %in% names(reps)) {
      out$input_tokens <- reps$input_tokens
    }
    if (include_cost && "cost" %in% names(reps)) {
      out$cost <- reps$cost
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
        full_prompt <- make_prompt(chapter_text)
        prompts <- as.list(rep(full_prompt, n_simulations))
        reps <- ellmer::parallel_chat_structured(
          chat = chat,
          prompts = prompts,
          type = type_score,
          include_tokens = include_tokens,
          include_cost = include_cost
        )
        df <- tibble::tibble(
          book = book,
          chapter = chapter_id,
          sim = seq_len(n_simulations),
          party = reps$party,
          score = reps$score,
          chapter_excerpt = excerpt,
          context = context_text,
          question = question_text
        )
        if (include_tokens && "input_tokens" %in% names(reps)) {
          df$input_tokens <- reps$input_tokens
        }
        if (include_cost && "cost" %in% names(reps)) {
          df$cost <- reps$cost
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
