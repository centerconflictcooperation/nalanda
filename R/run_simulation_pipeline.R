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
  require_groups = TRUE,
  total_steps = NULL,
  default_unit_id = "chapter_1",
  ...
) {
  if (missing(question_text) || length(question_text) != 1) {
    stop("`question_text` must be a single character string.")
  }
  if (n_simulations < 1) {
    stop("`n_simulations` must be >= 1.")
  }
  if (identical(model, "")) {
    stop("`model` must be a non-empty string.")
  }
  if (missing(groups) || is.null(groups) || length(groups) == 0) {
    if (isTRUE(require_groups)) {
      stop("`groups` must be a character vector with at least 2 group labels.")
    }
    groups <- NA_character_
  }
  if (isTRUE(require_groups) && length(groups) < 2) {
    stop("`groups` must be a character vector with at least 2 group labels.")
  }

  per_group <- grepl("\\{group\\}", question_text) && !all(is.na(groups))

  if (missing(context_text) || is.null(context_text)) {
    context_text <- rep("", length(groups))
  }
  if (length(context_text) == 1 && grepl("\\{identity\\}", context_text) && !all(is.na(groups))) {
    context_text <- vapply(
      groups,
      function(g) {
        gsub("\\{identity\\}", g, context_text)
      },
      character(1),
      USE.NAMES = FALSE
    )
  } else if (length(context_text) == 1 && length(groups) > 1) {
    context_text <- rep(context_text, length(groups))
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

  chapter_jobs <- build_chapter_jobs(
    book_texts,
    default_unit_id = default_unit_id
  )
  if (nrow(chapter_jobs) == 0) {
    stop(
      "No chapters found in `book_texts`. ",
      "Ensure the input contains at least one chapter before running simulations.",
      call. = FALSE
    )
  }
  if (is.null(total_steps)) {
    total_steps <- nrow(chapter_jobs) * n_simulations * length(groups)
  }
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

build_chapter_jobs <- function(book_texts, default_unit_id = "chapter_1") {
  if (is.character(book_texts) && length(book_texts) == 1) {
    chapter_id <- if (!is.null(names(book_texts))) {
      names(book_texts)[1]
    } else {
      default_unit_id
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

group_keys_from_groups <- function(groups) {
  tolower(gsub(" ", "_", groups))
}

build_structured_type <- function(groups, include_party = TRUE) {
  group_keys <- group_keys_from_groups(groups)
  fields <- list()
  if (isTRUE(include_party)) {
    fields$party <- ellmer::type_string()
  }
  for (gk in group_keys) {
    fields[[paste0("rating_", gk)]] <- ellmer::type_number()
  }
  do.call(ellmer::type_object, fields)
}

build_single_rating_type <- function(include_party = TRUE) {
  if (isTRUE(include_party)) {
    return(ellmer::type_object(
      party = ellmer::type_string(),
      rating = ellmer::type_number()
    ))
  }

  ellmer::type_object(rating = ellmer::type_number())
}

build_turn_types <- function(per_group, groups, include_party_first = TRUE) {
  if (isTRUE(per_group)) {
    return(list(
      first = build_structured_type(groups, include_party = include_party_first),
      second = build_structured_type(groups, include_party = FALSE)
    ))
  }

  list(
    first = build_single_rating_type(include_party = include_party_first),
    second = build_single_rating_type(include_party = FALSE)
  )
}

format_progress_label <- function(book, chapter, identity, sim) {
  if (!is.na(book) && nzchar(book)) {
    paste0(book, " - ", chapter, " [", identity, "] (sim ", sim, ")")
  } else {
    paste0(chapter, " [", identity, "] (sim ", sim, ")")
  }
}

make_result_base_fields <- function(book, chapter, sim, identity, party, extra = list()) {
  fields <- c(
    list(
      chapter = chapter,
      sim = sim,
      identity = identity,
      party = party
    ),
    extra
  )

  if (!is.na(book) && nzchar(book)) {
    fields <- c(list(book = book), fields)
  }

  fields
}

attach_usage_fields <- function(row, response, include_tokens, include_cost) {
  if (isTRUE(include_tokens) && !is.null(response$input_tokens)) {
    row$input_tokens <- response$input_tokens
  }
  if (isTRUE(include_cost) && !is.null(response$cost)) {
    row$cost <- response$cost
  }
  row
}

finalize_simulation_output <- function(out_rows, long_cols, chapter_jobs) {
  out_tbl <- tibble::as_tibble(do.call(
    rbind.data.frame,
    lapply(out_rows, function(r) as.data.frame(r, stringsAsFactors = FALSE))
  ))

  present_long <- intersect(long_cols, names(out_tbl))
  if (length(present_long) > 0) {
    out_tbl <- out_tbl[, c(setdiff(names(out_tbl), present_long), present_long)]
  }

  chapter_excerpts <- chapter_jobs_to_excerpt_index(chapter_jobs)

  if (all(is.na(chapter_jobs$book))) {
    out <- out_tbl
  } else {
    out <- split(out_tbl, out_tbl$book)
    out <- lapply(out, tibble::as_tibble)
    for (book_name in names(out)) {
      attr(out[[book_name]], "chapter_excerpts") <- dplyr::filter(
        chapter_excerpts,
        .data$book == book_name
      )
    }
  }

  attr(out, "chapter_excerpts") <- chapter_excerpts
  out
}
