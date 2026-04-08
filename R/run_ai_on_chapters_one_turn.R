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
#' @param integration Optional Portkey/gateway route slug. If supplied and
#'   `model` is not fully-qualified, nalanda will build
#'   `"@{integration}/{model}"`. Use a route returned by
#'   `ellmer::models_portkey(base_url = "https://ai-gateway.apps.cloud.rt.nyu.edu/v1/")`
#'   when working with the NYU gateway. When both `nalanda.integration` and
#'   `nalanda.virtual_key` options are set and neither argument is supplied,
#'   `integration` is preferred.
#' @param virtual_key Optional legacy virtual key. If supplied and `model` is
#'   not fully-qualified, nalanda will build `"@{virtual_key}/{model}"`.
#'   Use either `integration` or `virtual_key`, not both when explicitly
#'   supplying function arguments.
#' @param base_url Character. Base URL for API calls.
#' @param excerpt_chars Integer. Number of chapter characters to retain in the
#'   stored prompt preview shown in results.
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
  max_active = 10,
  rpm = 500
) {
  run_simulation_pipeline(
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
    integration_missing = missing(integration),
    virtual_key_missing = missing(virtual_key),
    base_url = base_url,
    excerpt_chars = excerpt_chars,
    executor = execute_one_turn_pipeline,
    max_active = max_active,
    rpm = rpm
  )
}

execute_one_turn_pipeline <- function(
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
  progress_tick,
  max_active,
  rpm
) {
  group_keys <- group_keys_from_groups(groups)
  type_response <- if (isTRUE(per_group)) {
    build_structured_type(groups, include_party = TRUE)
  } else {
    build_single_rating_type(include_party = TRUE)
  }

  all_rows <- list()
  all_row_i <- 0L

  for (k in seq_len(n_simulations)) {
    chat <- new_portkey_chat(
      model = model,
      base_url = base_url,
      temperature = temperature,
      seed = seed + k - 1L
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
          book_index = chapter_job$book_index[[1]],
          total_books = chapter_job$total_books[[1]],
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
      progress_tick(
        book = meta$book,
        chapter = meta$chapter,
        identity = meta$identity,
        sim = meta$sim
      )

      base_fields <- make_result_base_fields(
        book = meta$book,
        chapter = meta$chapter,
        sim = meta$sim,
        identity = meta$identity,
        party = response$party,
        extra = list(
          turn_index = 1L,
          turn_type = "single",
          prompt = meta$prompt
        )
      )

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

        all_row_i <- all_row_i + 1L
        all_rows[[all_row_i]] <- row
      }
    }
  }

  finalize_simulation_output(
    out_rows = all_rows,
    long_cols = "prompt",
    chapter_jobs = chapter_jobs
  )
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
  models <- attr(input, "models")
  temperature <- attr(input, "temperature")
  n_simulations <- attr(input, "n_simulations")
  chapter_excerpts <- chapter_excerpt_index(input)

  if (is.null(model) && is.list(input) && !inherits(input, "data.frame") &&
    length(input) > 0) {
    model <- rlang::`%||%`(model, attr(input[[1]], "model"))
    temperature <- rlang::`%||%`(temperature, attr(input[[1]], "temperature"))
  }
  model <- normalize_model_name(model)
  models <- normalize_model_metadata(models)

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
  optional_id <- c("model", "book", "party", "prompt")
  id_cols <- c(id_cols, intersect(optional_id, names(x)))

  unit_key <- interaction_key(x, id_cols)
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
  if (length(models) > 1) {
    attr(out, "models") <- models
  }
  attr(out, "temperature") <- temperature
  attr(out, "n_simulations") <- n_simulations
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
    "You have just been shown the material below.\n\n",
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
