#' Run AI model on book chapters with a single prompt per simulation
#'
#' This function implements a one-turn design where identity context, chapter
#' text, and the rating question are combined into a single prompt. Independent
#' prompts are executed in parallel with [ellmer::parallel_chat_structured()].
#'
#' @param book_texts A single character (one chapter) or a nested list of
#'   books -> chapters as returned by `read_book_texts()`.
#' @param groups Character vector of group labels (length >= 2). These are the
#'   groups being compared. Example: `c("Democrat", "Republican")`.
#' @param context_text Character. Either:
#'   \itemize{
#'     \item A scalar template containing `{identity}`, which will be expanded
#'       once for each group.
#'     \item A character vector of length equal to `length(groups)`, where each
#'       element is the full context for the corresponding group identity.
#'   }
#' @param question_text Character scalar. A question template containing the
#'   placeholder `{group}`, which will be replaced with each group label in
#'   per-group mode.
#' @param n_simulations Integer. Number of repeated simulations per chapter per
#'   identity.
#' @param temperature Numeric. Sampling temperature passed to the chat backend.
#' @param seed Integer. Random seed for reproducibility. As in
#'   [run_ai_on_chapters()], the seed varies by simulation index only, so all
#'   chapters and identities within the same `sim` share the same seed.
#' @param model Character. Model name for the chat backend.
#' @param integration Optional integration/provider slug. If supplied and
#'   `model` is not fully-qualified, nalanda will build
#'   `"@{integration}/{model}"`. Preferred for new Portkey/NYU setups.
#' @param virtual_key Optional legacy virtual key. If supplied and `model` is
#'   not fully-qualified, nalanda will build `"@{virtual_key}/{model}"`.
#'   Use either `integration` or `virtual_key`, not both.
#' @param base_url Character. Base URL for API calls.
#' @param excerpt_chars Integer. Number of chapter characters to retain in the
#'   stored prompt preview shown in results.
#' @param include_tokens Logical. Return token counts if available.
#' @param include_cost Logical. Return cost info if available.
#' @param max_active Integer. Maximum number of concurrent requests passed to
#'   [ellmer::parallel_chat_structured()].
#' @param rpm Integer. Requests-per-minute cap passed to
#'   [ellmer::parallel_chat_structured()].
#'
#' @return A tibble of raw single-turn ratings, or a named list of tibbles (one
#'   per book). Each row is one rating observation and includes `chapter`,
#'   `sim`, `identity`, `turn_index`, `turn_type`, `target_group`, and
#'   `rating`, plus prompt and metadata columns. Use
#'   [compute_run_ai_metrics_one_turn()] to derive chapter-level one-turn
#'   summaries.
#' @export
run_ai_on_chapters_one_turn <- function(
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
  include_cost = FALSE,
  max_active = 10,
  rpm = 500
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
  if (!is.null(integration) && nzchar(integration) &&
      !is.null(virtual_key) && nzchar(virtual_key)) {
    stop("Please provide only one of `integration` or `virtual_key`.")
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

  chapter_jobs <- build_one_turn_chapter_jobs(book_texts)
  total_steps <- nrow(chapter_jobs) * n_simulations * length(groups)

  pb <- progress::progress_bar$new(
    format = "  running [:bar] :percent eta: :eta :what",
    total = total_steps,
    clear = FALSE,
    width = 60
  )

  group_keys <- tolower(gsub(" ", "_", groups))
  if (per_group) {
    response_fields <- list(party = ellmer::type_string())
    for (gk in group_keys) {
      response_fields[[paste0("rating_", gk)]] <- ellmer::type_number()
    }
    type_response <- do.call(ellmer::type_object, response_fields)
  } else {
    type_response <- ellmer::type_object(
      party = ellmer::type_string(),
      rating = ellmer::type_number()
    )
  }

  all_rows <- list()
  all_row_i <- 0L

  for (k in seq_len(n_simulations)) {
    chat <- tryCatch(
      ellmer::chat_portkey(
        model = model,
        base_url = base_url,
        params = ellmer::params(
          temperature = temperature,
          seed = seed + k - 1L
        ),
        api_args = list(
          temperature = temperature,
          seed = seed + k - 1L
        )
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
            "`model` (e.g., '@provider/model') or update `ellmer`.",
            call. = FALSE
          )
        }
        stop(e)
      }
    )

    prompt_jobs <- vector("list", nrow(chapter_jobs) * length(groups))
    prompts <- character(length(prompt_jobs))
    prompt_i <- 0L

    for (chapter_i in seq_len(nrow(chapter_jobs))) {
      chapter_job <- chapter_jobs[chapter_i, , drop = FALSE]

      for (id_idx in seq_along(groups)) {
        identity_label <- groups[[id_idx]]
        identity_context <- context_text[[id_idx]]
        full_prompt <- make_one_turn_prompt(
          chapter_text = chapter_job$chapter_text[[1]],
          identity_context = identity_context,
          question_template = question_text,
          groups = groups,
          identity_label = identity_label
        )
        prompt_preview <- make_one_turn_prompt_preview(
          chapter_text = chapter_job$chapter_text[[1]],
          identity_context = identity_context,
          question_template = question_text,
          groups = groups,
          identity_label = identity_label,
          excerpt_chars = excerpt_chars
        )

        prompt_i <- prompt_i + 1L
        prompts[[prompt_i]] <- full_prompt
        prompt_jobs[[prompt_i]] <- list(
          book = chapter_job$book[[1]],
          chapter = chapter_job$chapter[[1]],
          sim = k,
          identity = identity_label,
          prompt = prompt_preview
        )
      }
    }

    responses <- ellmer::parallel_chat_structured(
      chat = chat,
      prompts = prompts,
      type = type_response,
      convert = TRUE,
      include_tokens = include_tokens,
      include_cost = include_cost,
      max_active = max_active,
      rpm = rpm,
      on_error = "stop"
    )
    response_list <- normalize_parallel_chat_structured_output(responses)

    if (length(response_list) != length(prompt_jobs)) {
      stop(
        "Expected ",
        length(prompt_jobs),
        " responses from `ellmer::parallel_chat_structured()`, got ",
        length(response_list),
        "."
      )
    }

    for (i in seq_along(prompt_jobs)) {
      meta <- prompt_jobs[[i]]
      response <- response_list[[i]]
      what_text <- if (!is.na(meta$book) && nzchar(meta$book)) {
        paste0(
          meta$book,
          " - ",
          meta$chapter,
          " [",
          meta$identity,
          "] (sim ",
          meta$sim,
          ")"
        )
      } else {
        paste0(meta$chapter, " [", meta$identity, "] (sim ", meta$sim, ")")
      }
      pb$tick(tokens = list(what = what_text))

      base_fields <- list(
        chapter = meta$chapter,
        sim = meta$sim,
        identity = meta$identity,
        party = response$party,
        turn_index = 1L,
        turn_type = "single",
        prompt = meta$prompt
      )

      if (!is.na(meta$book) && nzchar(meta$book)) {
        base_fields <- c(list(book = meta$book), base_fields)
      }

      if (per_group) {
        for (g_idx in seq_along(groups)) {
          g <- groups[[g_idx]]
          field <- paste0("rating_", group_keys[[g_idx]])
          row <- c(
            base_fields,
            list(
              target_group = g,
              rating = response[[field]]
            )
          )
          if (include_tokens && !is.null(response$input_tokens)) {
            row$input_tokens <- response$input_tokens
          }
          if (include_cost && !is.null(response$cost)) {
            row$cost <- response$cost
          }

          all_row_i <- all_row_i + 1L
          all_rows[[all_row_i]] <- row
        }
      } else {
        row <- c(
          base_fields,
          list(
            target_group = NA_character_,
            rating = response$rating
          )
        )
        if (include_tokens && !is.null(response$input_tokens)) {
          row$input_tokens <- response$input_tokens
        }
        if (include_cost && !is.null(response$cost)) {
          row$cost <- response$cost
        }

        all_row_i <- all_row_i + 1L
        all_rows[[all_row_i]] <- row
      }
    }
  }

  out_tbl <- tibble::as_tibble(do.call(
    rbind.data.frame,
    lapply(
      all_rows,
      function(r) as.data.frame(r, stringsAsFactors = FALSE)
    )
  ))

  long_cols <- intersect("prompt", names(out_tbl))
  if (length(long_cols) > 0) {
    out_tbl <- out_tbl[, c(setdiff(names(out_tbl), long_cols), long_cols)]
  }

  chapter_excerpts <- tibble::tibble(
    book = chapter_jobs$book,
    chapter = chapter_jobs$chapter,
    chapter_excerpt = chapter_jobs$chapter_text
  )
  chapter_excerpts <- dplyr::distinct(chapter_excerpts)

  if (all(is.na(chapter_jobs$book))) {
    chapter_excerpts <- dplyr::select(chapter_excerpts, -dplyr::all_of("book"))
    out <- out_tbl
  } else {
    out <- split(out_tbl, out_tbl$book)
    out <- lapply(out, tibble::as_tibble)

    for (book_name in names(out)) {
      attr(out[[book_name]], "model") <- model
      attr(out[[book_name]], "temperature") <- temperature
      attr(out[[book_name]], "chapter_excerpts") <- dplyr::filter(
        chapter_excerpts,
        .data$book == book_name
      )
    }
  }

  class(out) <- c(class(out), "nalanda")
  attr(out, "model") <- model
  attr(out, "temperature") <- temperature
  attr(out, "chapter_excerpts") <- chapter_excerpts
  out
}

#' Compute one-turn ingroup/outgroup metrics from raw output
#'
#' @param x A data frame or list-like object from
#'   [run_ai_on_chapters_one_turn()] with single-turn rows including `chapter`,
#'   `sim`, `identity`, and `rating`.
#' @param per_group Optional logical. Whether the run used per-group mode. If
#'   `NULL` (default), mode is inferred from `target_group`.
#'
#' @return A simulation-level tibble with one-turn metrics. In per-group mode
#'   this includes `ingroup_rating`, `outgroup_rating`, and `gap`. In
#'   single-question mode it includes `overall_rating` and `outgroup_rating`.
#' @export
compute_run_ai_metrics_one_turn <- function(x, per_group = NULL) {
  input <- x

  if (is.list(input) && !inherits(input, "data.frame")) {
    x <- flatten_sim_results(input)
  }

  x <- tibble::as_tibble(x)

  model <- attr(input, "model")
  temperature <- attr(input, "temperature")
  chapter_excerpts <- chapter_excerpt_index(input)

  if (is.null(model) && is.list(input) && !inherits(input, "data.frame") &&
    length(input) > 0) {
    model <- rlang::`%||%`(model, attr(input[[1]], "model"))
    temperature <- rlang::`%||%`(temperature, attr(input[[1]], "temperature"))
  }

  required_cols <- c("chapter", "sim", "identity", "rating")
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0) {
    stop(
      "`x` is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      "."
    )
  }
  if (!("target_group" %in% names(x))) {
    x$target_group <- NA_character_
  }

  if (is.null(per_group)) {
    per_group <- any(!is.na(x$target_group) & nzchar(as.character(x$target_group)))
  }

  id_cols <- c("chapter", "sim", "identity")
  optional_id <- c("book", "party", "prompt")
  id_cols <- c(id_cols, intersect(optional_id, names(x)))

  unit_key <- interaction(x[id_cols], drop = TRUE, lex.order = TRUE)
  unit_indices <- split(seq_len(nrow(x)), unit_key)
  rows <- vector("list", length(unit_indices))

  row_i <- 0L
  for (idx in unit_indices) {
    row_i <- row_i + 1L
    xi <- x[idx, , drop = FALSE]
    row <- as.list(xi[1, id_cols, drop = FALSE])

    if (isTRUE(per_group)) {
      identity_label <- as.character(row$identity)

      row$ingroup_rating <- mean(
        as.numeric(xi$rating[as.character(xi$target_group) == identity_label]),
        na.rm = TRUE
      )
      row$outgroup_rating <- mean(
        as.numeric(xi$rating[as.character(xi$target_group) != identity_label]),
        na.rm = TRUE
      )
      row$gap <- row$ingroup_rating - row$outgroup_rating
    } else {
      row$overall_rating <- mean(as.numeric(xi$rating), na.rm = TRUE)
      row$ingroup_rating <- NA_real_
      row$outgroup_rating <- row$overall_rating
      row$gap <- NA_real_
    }

    if ("input_tokens" %in% names(xi)) {
      row$input_tokens <- sum(as.numeric(xi$input_tokens), na.rm = TRUE)
    }
    if ("cost" %in% names(xi)) {
      row$cost <- sum(as.numeric(xi$cost), na.rm = TRUE)
    }

    rows[[row_i]] <- row
  }

  out <- tibble::as_tibble(do.call(
    rbind.data.frame,
    lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE))
  ))

  long_cols <- intersect("prompt", names(out))
  if (length(long_cols) > 0) {
    out <- out[, c(setdiff(names(out), long_cols), long_cols)]
  }

  attr(out, "model") <- model
  attr(out, "temperature") <- temperature
  attr(out, "chapter_excerpts") <- chapter_excerpts
  out
}

#' Plot chapter trajectories for one-turn simulations
#'
#' @param chapters Raw output from [run_ai_on_chapters_one_turn()] or processed
#'   output from [compute_run_ai_metrics_one_turn()].
#' @param dv Character. Name of the metric to plot. Defaults to
#'   `"outgroup_rating"`.
#' @param ... Additional arguments passed to [plot_chapters_over_time()].
#'
#' @return A ggplot2 object.
#' @export
plot_chapters_over_time_one_turn <- function(
  chapters,
  dv = "outgroup_rating",
  ...
) {
  args <- list(...)
  df <- bind_simulation_results(chapters)

  if (!dv %in% names(df)) {
    df <- compute_run_ai_metrics_one_turn(chapters)
  }

  p <- do.call(
    plot_chapters_over_time,
    c(list(chapters = df, dv = dv), args)
  )

  plot_title <- args$plot_title
  if (is.null(plot_title) || isTRUE(plot_title)) {
    n_sims <- max(df$sim, na.rm = TRUE)
    p <- p + ggplot2::labs(
      title = paste0(
        "Results of ",
        n_sims,
        " simulations per book per chapter"
      )
    )
  }

  p
}

make_one_turn_prompt <- function(
  chapter_text,
  identity_context,
  question_template,
  groups,
  identity_label
) {
  question_block <- build_group_question_block(
    question_template = question_template,
    groups = groups,
    identity_label = identity_label
  )

  paste0(
    identity_context,
    "\n\n",
    "You have just read the chapter below.\n\n",
    chapter_text,
    "\n\n",
    "Now answer the following question(s):\n",
    question_block
  )
}

make_one_turn_prompt_preview <- function(
  chapter_text,
  identity_context,
  question_template,
  groups,
  identity_label,
  excerpt_chars = 200
) {
  compact_chapter <- compact_chapter_text(
    chapter_text,
    excerpt_chars = excerpt_chars
  )

  make_one_turn_prompt(
    chapter_text = compact_chapter,
    identity_context = identity_context,
    question_template = question_template,
    groups = groups,
    identity_label = identity_label
  )
}

build_group_question_block <- function(question_template, groups, identity_label) {
  if (grepl("\\{group\\}", question_template)) {
    ordered_groups <- c(identity_label, setdiff(groups, identity_label))
    return(paste(
      vapply(
        ordered_groups,
        function(g) gsub("\\{group\\}", g, question_template),
        character(1),
        USE.NAMES = FALSE
      ),
      collapse = " "
    ))
  }

  question_template
}

build_one_turn_chapter_jobs <- function(book_texts) {
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
      validate_chapter_order(
        chapter = names(chapter_texts),
        book = book,
        arg_name = "`book_texts` chapter names"
      )

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

normalize_parallel_chat_structured_output <- function(x) {
  if (inherits(x, "data.frame")) {
    return(lapply(seq_len(nrow(x)), function(i) {
      as.list(x[i, , drop = FALSE])
    }))
  }

  if (is.list(x)) {
    return(lapply(x, function(elt) {
      if (inherits(elt, "data.frame")) {
        return(as.list(elt[1, , drop = FALSE]))
      }
      if (is.list(elt)) {
        return(elt)
      }
      as.list(elt)
    }))
  }

  stop(
    "Unsupported output type from `ellmer::parallel_chat_structured()`: ",
    paste(class(x), collapse = "/")
  )
}
