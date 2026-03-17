run_simulation_pipeline <- function(
  book_texts,
  groups,
  context_text,
  question_text,
  n_simulations,
  temperature,
  seed,
  model,
  integration,
  virtual_key,
  base_url,
  excerpt_chars,
  include_tokens,
  include_cost,
  executor,
  ...
) {
  if (missing(groups) || length(groups) < 2) {
    stop("`groups` must be a character vector with at least 2 group labels.")
  }
  if (missing(question_text) || length(question_text) != 1) {
    stop("`question_text` must be a single character string.")
  }
  if (missing(context_text)) {
    stop("Please provide `context_text`.")
  }
  if (n_simulations < 1) {
    stop("`n_simulations` must be >= 1.")
  }
  if (identical(model, "")) {
    stop("`model` must be a non-empty string.")
  }
  per_group <- grepl("\\{group\\}", question_text)

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

  chapter_jobs <- build_chapter_jobs(book_texts)
  total_steps <- nrow(chapter_jobs) * n_simulations * length(groups)
  pb <- progress::progress_bar$new(
    format = "  running [:bar] :percent eta: :eta :what",
    total = total_steps,
    clear = FALSE,
    width = 60
  )

  out <- executor(
    chapter_jobs = chapter_jobs,
    groups = groups,
    context_text = context_text,
    per_group = per_group,
    question_text = question_text,
    n_simulations = n_simulations,
    temperature = temperature,
    seed = seed,
    model = model,
    base_url = base_url,
    excerpt_chars = excerpt_chars,
    include_tokens = include_tokens,
    include_cost = include_cost,
    pb = pb,
    ...
  )

  class(out) <- c(class(out), "nalanda")
  attr(out, "model") <- model
  attr(out, "temperature") <- temperature
  attr(out, "n_simulations") <- n_simulations

  if (is.list(out) && !inherits(out, "data.frame")) {
    for (nm in names(out)) {
      attr(out[[nm]], "model") <- model
      attr(out[[nm]], "temperature") <- temperature
      attr(out[[nm]], "n_simulations") <- n_simulations
    }
  }

  out
}

build_chapter_jobs <- function(book_texts) {
  if (is.character(book_texts) && length(book_texts) == 1) {
    chapter_id <- if (!is.null(names(book_texts))) {
      names(book_texts)[1]
    } else {
      "chapter_1"
    }

    return(tibble::tibble(
      book = NA_character_,
      chapter = chapter_id,
      chapter_text = unname(book_texts[[1]])
    ))
  }

  if (is.list(book_texts)) {
    out <- list()
    out_i <- 0L
    book_names <- names(book_texts)

    for (i in seq_along(book_texts)) {
      book <- if (is.null(book_names)) paste0("book_", i) else book_names[[i]]
      chapter_texts <- unlist(book_texts[[i]], use.names = TRUE)

      for (j in seq_along(chapter_texts)) {
        out_i <- out_i + 1L
        out[[out_i]] <- tibble::tibble(
          book = book,
          chapter = names(chapter_texts)[[j]],
          chapter_text = unname(chapter_texts[[j]])
        )
      }
    }

    return(dplyr::bind_rows(out))
  }

  stop(
    "`book_texts` must be either a single chapter text or ",
    "a nested list like `read_book_texts()`."
  )
}

chapter_jobs_to_excerpt_index <- function(chapter_jobs) {
  chapter_excerpts <- tibble::tibble(
    book = chapter_jobs$book,
    chapter = chapter_jobs$chapter,
    chapter_excerpt = chapter_jobs$chapter_text
  )
  chapter_excerpts <- dplyr::distinct(chapter_excerpts)

  if (all(is.na(chapter_jobs$book))) {
    chapter_excerpts <- dplyr::select(
      chapter_excerpts,
      -dplyr::all_of("book")
    )
  }

  chapter_excerpts
}

new_portkey_chat <- function(model, base_url, temperature, seed) {
  tryCatch(
    ellmer::chat_portkey(
      model = model,
      base_url = base_url,
      params = ellmer::params(
        temperature = temperature,
        seed = seed
      ),
      api_args = list(
        temperature = temperature,
        seed = seed
      )
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
          "`model` (e.g., '@provider/model') or update `ellmer`.",
          call. = FALSE
        )
      }
      stop(e)
    }
  )
}
