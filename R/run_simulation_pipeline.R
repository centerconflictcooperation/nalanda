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
  integration_missing = FALSE,
  virtual_key_missing = FALSE,
  base_url,
  excerpt_chars,
  executor,
  require_groups = TRUE,
  total_steps = NULL,
  default_unit_id = "chapter_1",
  checkpoint_dir = NULL,
  checkpoint_prefix = "checkpoint",
  save_dir = NULL,
  save_prefix = "results",
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
  if (!is.null(checkpoint_dir)) {
    if (!is.character(checkpoint_dir) || length(checkpoint_dir) != 1 || !nzchar(checkpoint_dir)) {
      stop("`checkpoint_dir` must be a single non-empty path, or `NULL`.")
    }
    dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(checkpoint_dir)) {
      stop("Could not create `checkpoint_dir`: ", checkpoint_dir)
    }
  }
  if (!is.character(checkpoint_prefix) || length(checkpoint_prefix) != 1 || !nzchar(checkpoint_prefix)) {
    stop("`checkpoint_prefix` must be a single non-empty string.")
  }
  if (!is.null(save_dir)) {
    if (!is.character(save_dir) || length(save_dir) != 1 || !nzchar(save_dir)) {
      stop("`save_dir` must be a single non-empty path, or `NULL`.")
    }
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(save_dir)) {
      stop("Could not create `save_dir`: ", save_dir)
    }
  }
  if (!is.character(save_prefix) || length(save_prefix) != 1 || !nzchar(save_prefix)) {
    stop("`save_prefix` must be a single non-empty string.")
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

  model_specs <- expand_model_specs(
    model = model,
    integration = integration,
    virtual_key = virtual_key,
    integration_missing = integration_missing,
    virtual_key_missing = virtual_key_missing
  )
  validate_model_parameters(
    model = vapply(model_specs, `[[`, character(1), "model"),
    temperature = temperature
  )

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
  inform_missing_chapter_jobs(chapter_jobs)
  if (is.null(total_steps)) {
    total_steps <- nrow(chapter_jobs) * n_simulations * length(groups)
  }
  total_steps <- total_steps * length(model_specs)
  progress_tracker <- new_progress_tracker(
    chapter_jobs = chapter_jobs,
    n_simulations = n_simulations,
    n_identities = length(groups),
    total_steps = total_steps,
    n_models = length(model_specs)
  )

  out_list <- vector("list", length(model_specs))
  for (i in seq_along(model_specs)) {
    spec <- model_specs[[i]]
    out_i <- executor(
      chapter_jobs = chapter_jobs,
      groups = groups,
      context_text = context_text,
      per_group = per_group,
      question_text = question_text,
      n_simulations = n_simulations,
      temperature = temperature,
      seed = seed,
      model = spec$model,
      base_url = base_url,
      excerpt_chars = excerpt_chars,
      model_label = spec$model_label,
      checkpoint_dir = checkpoint_dir,
      checkpoint_prefix = checkpoint_prefix,
      save_dir = save_dir,
      save_prefix = save_prefix,
      n_models = length(model_specs),
      pb = progress_tracker$pb,
      progress_tick = function(book, chapter, identity, sim) {
        progress_tracker$tick(
          book = book,
          chapter = chapter,
          identity = identity,
          sim = sim,
          model = spec$model_label
        )
      },
      ...
    )
    out_list[[i]] <- add_model_column_to_output(out_i, spec$model_label)
  }

  out <- merge_simulation_outputs(out_list)

  class(out) <- unique(c("nalanda", class(out)))
  model_labels <- normalize_model_metadata(vapply(model_specs, `[[`, character(1), "model_label"))
  if (length(model_labels) == 1) {
    attr(out, "model") <- model_labels
  } else {
    attr(out, "model") <- NULL
    attr(out, "models") <- model_labels
  }
  attr(out, "temperature") <- temperature
  attr(out, "n_simulations") <- n_simulations

  if (is.list(out) && !inherits(out, "data.frame")) {
    for (nm in names(out)) {
      if (length(model_labels) == 1) {
        attr(out[[nm]], "model") <- model_labels
      } else {
        attr(out[[nm]], "model") <- NULL
        attr(out[[nm]], "models") <- model_labels
      }
      class(out[[nm]]) <- unique(c("nalanda", class(out[[nm]])))
      attr(out[[nm]], "temperature") <- temperature
      attr(out[[nm]], "n_simulations") <- n_simulations
    }
  }

  out
}

new_progress_tracker <- function(
  chapter_jobs,
  n_simulations,
  n_identities,
  total_steps,
  n_models = 1L
) {
  units_per_stage <- n_simulations * n_identities * n_models
  book_ids <- progress_scope_id(chapter_jobs$book)
  chapter_ids <- progress_scope_id(chapter_jobs$book, chapter_jobs$chapter)
  book_stage_counts <- table(book_ids)
  chapter_stage_counts <- table(chapter_ids)

  pb <- progress::progress_bar$new(
    format = "  running [:bar] :percent eta: :eta :what",
    total = total_steps,
    clear = FALSE
  )

  state <- new.env(parent = emptyenv())
  state$total_done <- 0L
  state$current_book_id <- NULL
  state$current_book_done <- 0L
  state$current_chapter_id <- NULL
  state$current_chapter_done <- 0L

  tick <- function(book, chapter, identity, sim, model = NULL) {
    book_id <- progress_scope_id(book)
    chapter_id <- progress_scope_id(book, chapter)

    if (!identical(state$current_book_id, book_id)) {
      state$current_book_id <- book_id
      state$current_book_done <- 0L
    }
    if (!identical(state$current_chapter_id, chapter_id)) {
      state$current_chapter_id <- chapter_id
      state$current_chapter_done <- 0L
    }

    state$total_done <- state$total_done + 1L
    state$current_book_done <- state$current_book_done + 1L
    state$current_chapter_done <- state$current_chapter_done + 1L

    book_total <- as.integer(book_stage_counts[[book_id]]) * units_per_stage
    chapter_count <- if (chapter_id %in% names(chapter_stage_counts)) {
      as.integer(chapter_stage_counts[[chapter_id]])
    } else {
      1L
    }
    chapter_total <- chapter_count * units_per_stage

    pb$tick(tokens = list(
      what = format_progress_label(
        book = book,
        chapter = chapter,
        identity = identity,
        sim = sim,
        model = model,
        book_pct = format_progress_pct(state$current_book_done, book_total),
        chapter_pct = format_progress_pct(state$current_chapter_done, chapter_total)
      )
    ))
  }

  list(
    pb = pb,
    tick = tick
  )
}

progress_scope_id <- function(book, chapter = NULL) {
  if (length(book) == 0) {
    book_part <- character()
  } else {
    book_part <- as.character(book)
    book_part[is.na(book_part) | !nzchar(book_part)] <- "__default_book__"
  }

  if (is.null(chapter)) {
    return(book_part)
  }

  if (length(chapter) == 0) {
    chapter_part <- character()
  } else {
    chapter_part <- as.character(chapter)
    chapter_part[is.na(chapter_part) | !nzchar(chapter_part)] <- "__default_chapter__"
  }

  paste(book_part, chapter_part, sep = "::")
}

is_flat_text_list <- function(x) {
  is.list(x) &&
    length(x) > 0 &&
    all(vapply(
      x,
      function(value) is.character(value) && length(value) == 1,
      logical(1)
    ))
}

flat_text_list_book_name <- function(x) {
  book <- attr(x, "book", exact = TRUE)
  if (is.null(book)) {
    book <- attr(x, "book_name", exact = TRUE)
  }
  if (is.null(book) || length(book) != 1 || is.na(book) || !nzchar(as.character(book))) {
    return(NULL)
  }
  as.character(book)
}

format_progress_pct <- function(done, total) {
  if (is.na(total) || total < 1) {
    return("--%")
  }

  sprintf("%3.0f%%", 100 * done / total)
}

sanitize_checkpoint_part <- function(x, fallback) {
  if (length(x) == 0 || is.na(x) || !nzchar(as.character(x))) {
    x <- fallback
  }
  x <- as.character(x)[1]
  x <- gsub("[^A-Za-z0-9._-]+", "-", x)
  x <- gsub("^-+|-+$", "", x)
  if (!nzchar(x)) {
    x <- fallback
  }
  x
}

rows_to_tibble <- function(rows, long_cols = character()) {
  out_tbl <- dplyr::bind_rows(lapply(rows, tibble::as_tibble_row))

  present_long <- intersect(long_cols, names(out_tbl))
  if (length(present_long) > 0) {
    out_tbl <- out_tbl[, c(setdiff(names(out_tbl), present_long), present_long)]
  }

  out_tbl
}

write_simulation_checkpoint <- function(
  rows,
  long_cols,
  checkpoint_dir,
  checkpoint_prefix,
  model_label,
  book,
  chapter,
  identity,
  sim
) {
  if (is.null(checkpoint_dir) || length(rows) == 0) {
    return(invisible(NULL))
  }

  out_tbl <- rows_to_tibble(rows, long_cols = long_cols)
  out_tbl$model <- model_label
  out_tbl <- out_tbl[, c("model", setdiff(names(out_tbl), "model"))]

  file_name <- paste(
    sanitize_checkpoint_part(checkpoint_prefix, "checkpoint"),
    sanitize_checkpoint_part(model_label, "model"),
    sanitize_checkpoint_part(book, "book"),
    sanitize_checkpoint_part(chapter, "chapter"),
    sanitize_checkpoint_part(identity, "identity"),
    sprintf("sim-%03d.Rds", as.integer(sim)),
    sep = "__"
  )
  path <- file.path(checkpoint_dir, file_name)
  tmp <- tempfile(pattern = paste0(".", basename(path), "."), tmpdir = checkpoint_dir, fileext = ".tmp")

  saveRDS(out_tbl, tmp)
  if (file.exists(path)) {
    unlink(path)
  }
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    stop("Could not write checkpoint file: ", path)
  }

  invisible(path)
}

checkpoint_key <- function(book, chapter, identity, sim, model_label) {
  parts <- list(
    model = model_label,
    book = book,
    chapter = chapter,
    identity = identity,
    sim = sim
  )

  parts <- lapply(parts, function(x) {
    x <- as.character(x)[1]
    if (length(x) == 0 || is.na(x) || !nzchar(x)) {
      x <- "<NA>"
    }
    x
  })

  paste(unlist(parts, use.names = FALSE), collapse = "\r")
}

read_checkpoint_file <- function(path) {
  out <- tryCatch(
    readRDS(path),
    error = function(e) NULL
  )
  if (!inherits(out, "data.frame") || nrow(out) == 0) {
    return(NULL)
  }
  tibble::as_tibble(out)
}

checkpoint_value <- function(x, name, default = NA_character_) {
  if (!name %in% names(x)) {
    return(default)
  }
  values <- unique(as.character(x[[name]]))
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) != 1) {
    return(default)
  }
  values[[1]]
}

load_checkpoint_index <- function(
  checkpoint_dir,
  checkpoint_prefix,
  model_label,
  cumulative = FALSE
) {
  if (is.null(checkpoint_dir) || !dir.exists(checkpoint_dir)) {
    return(list())
  }

  expected_start <- paste0(
    sanitize_checkpoint_part(checkpoint_prefix, "checkpoint"),
    "__",
    sanitize_checkpoint_part(model_label, "model"),
    "__"
  )
  paths <- list.files(checkpoint_dir, pattern = "\\.Rds$", full.names = TRUE)
  paths <- paths[startsWith(basename(paths), expected_start)]
  if (length(paths) == 0) {
    return(list())
  }

  index <- list()
  for (path in paths) {
    rows <- read_checkpoint_file(path)
    if (is.null(rows)) {
      next
    }

    model <- checkpoint_value(rows, "model", model_label)
    if (!identical(model, model_label)) {
      next
    }

    book <- checkpoint_value(rows, "book")
    identity <- checkpoint_value(rows, "identity")
    sim <- checkpoint_value(rows, "sim")
    chapter <- if (isTRUE(cumulative)) {
      "cumulative"
    } else {
      checkpoint_value(rows, "chapter")
    }

    key <- checkpoint_key(
      book = book,
      chapter = chapter,
      identity = identity,
      sim = sim,
      model_label = model_label
    )
    index[[key]] <- rows
  }

  index
}

checkpoint_index_get <- function(index, book, chapter, identity, sim, model_label) {
  key <- checkpoint_key(
    book = book,
    chapter = chapter,
    identity = identity,
    sim = sim,
    model_label = model_label
  )
  index[[key]]
}

append_checkpoint_rows <- function(all_rows, all_row_i, rows) {
  rows <- tibble::as_tibble(rows)
  if ("model" %in% names(rows)) {
    rows$model <- NULL
  }

  for (i in seq_len(nrow(rows))) {
    all_row_i <- all_row_i + 1L
    all_rows[[all_row_i]] <- as.list(rows[i, , drop = FALSE])
  }

  list(rows = all_rows, row_i = all_row_i)
}

write_book_result <- function(
  rows,
  long_cols,
  chapter_jobs,
  save_dir,
  save_prefix,
  n_models,
  model_label,
  temperature,
  n_simulations,
  book,
  column_renamer = NULL
) {
  if (is.null(save_dir) || length(rows) == 0) {
    return(invisible(NULL))
  }

  out_tbl <- rows_to_tibble(rows, long_cols = long_cols)
  out_tbl$model <- model_label
  out_tbl <- out_tbl[, c("model", setdiff(names(out_tbl), "model"))]
  out_tbl <- tibble::as_tibble(out_tbl)
  if (!is.null(column_renamer)) {
    out_tbl <- column_renamer(out_tbl)
  }
  class(out_tbl) <- unique(c("nalanda", class(out_tbl)))
  attr(out_tbl, "model") <- normalize_model_name(model_label)
  attr(out_tbl, "temperature") <- temperature
  attr(out_tbl, "n_simulations") <- n_simulations

  chapter_excerpts <- chapter_jobs_to_excerpt_index(chapter_jobs)
  if ("book" %in% names(chapter_excerpts)) {
    chapter_excerpts <- dplyr::filter(chapter_excerpts, .data$book == book)
  }
  attr(out_tbl, "chapter_excerpts") <- chapter_excerpts

  file_stem <- paste0(
    sanitize_checkpoint_part(save_prefix, "results"),
    "_",
    sanitize_checkpoint_part(book, "book")
  )
  if (n_models > 1L) {
    file_stem <- paste(
      file_stem,
      sanitize_checkpoint_part(model_label, "model"),
      sep = "_"
    )
  }
  file_name <- paste0(file_stem, ".Rds")
  path <- file.path(save_dir, file_name)
  tmp <- tempfile(pattern = paste0(".", basename(path), "."), tmpdir = save_dir, fileext = ".tmp")

  saveRDS(out_tbl, tmp)
  if (file.exists(path)) {
    unlink(path)
  }
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    stop("Could not write book result file: ", path)
  }

  invisible(path)
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
      book_index = NA_integer_,
      total_books = 1L,
      chapter = chapter_id,
      chapter_text = unname(book_texts[[1]])
    ))
  }

  if (is.list(book_texts)) {
    flat_book <- flat_text_list_book_name(book_texts)
    if (is_flat_text_list(book_texts) && !is.null(flat_book)) {
      chapter_texts <- unlist(book_texts, use.names = TRUE)
      return(tibble::tibble(
        book = flat_book,
        book_index = 1L,
        total_books = 1L,
        chapter = names(chapter_texts),
        chapter_text = unname(chapter_texts)
      ))
    }

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
          book_index = i,
          total_books = length(book_texts),
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
  validate_model_parameters(model = model, temperature = temperature)

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
          "Please set `options(nalanda.integration=...)` first ",
          "(preferred), or provide `integration` directly. ",
          "Legacy fallback: set `options(nalanda.virtual_key=...)` ",
          "or provide `virtual_key` directly. ",
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

normalize_output_mode <- function(output_mode) {
  match.arg(output_mode, c("structured", "text"))
}

build_response_fields <- function(per_group, groups, include_party = TRUE) {
  fields <- character()
  if (isTRUE(include_party)) {
    fields <- c(fields, "party")
  }

  if (isTRUE(per_group)) {
    group_keys <- group_keys_from_groups(groups)
    fields <- c(fields, paste0("rating_", group_keys))
  } else {
    fields <- c(fields, "rating")
  }

  fields
}

infer_response_type_fields <- function(response_type) {
  if (is.null(response_type)) {
    return(NULL)
  }
  if (methods::is(response_type, "TypeObject") ||
    methods::is(response_type, "ellmer::TypeObject") ||
    (isS4(response_type) && "properties" %in% methods::slotNames(response_type))) {
    properties <- tryCatch(
      methods::slot(response_type, "properties"),
      error = function(e) NULL
    )
    property_names <- names(properties)
    if (!is.null(property_names) && length(property_names) > 0) {
      return(property_names[nzchar(property_names)])
    }
  }
  if (is.list(response_type)) {
    for (slot in c("fields", "properties", "schema")) {
      if (!is.null(response_type[[slot]]) && is.list(response_type[[slot]])) {
        slot_names <- names(response_type[[slot]])
        if (!is.null(slot_names) && length(slot_names) > 0) {
          return(slot_names[nzchar(slot_names)])
        }
      }
    }

    field_names <- names(response_type)
    if (!is.null(field_names) && length(field_names) > 0) {
      metadata_names <- c(
        "type", "description", "required", "additionalProperties",
        "class", "name", "schema"
      )
      field_names <- setdiff(field_names[nzchar(field_names)], metadata_names)
      if (length(field_names) > 0) {
        return(field_names)
      }
    }
  }

  NULL
}

chat_model_response <- function(chat, prompt, type, output_mode, fields = NULL) {
  output_mode <- normalize_output_mode(output_mode)

  if (identical(output_mode, "structured")) {
    return(chat$chat_structured(prompt, type = type))
  }

  prompt_with_format <- append_soft_structured_instructions(prompt, fields)
  raw_response <- chat$chat(prompt_with_format)
  parse_soft_structured_response(raw_response, fields = fields)
}

append_soft_structured_instructions <- function(prompt, fields = NULL) {
  if (!is.null(fields) && length(fields) > 0) {
    field_json <- paste0(
      "{",
      paste(
        vapply(fields, function(field) {
          paste0('"', field, '": <value>')
        }, character(1)),
        collapse = ", "
      ),
      "}"
    )
    format_instruction <- paste0(
      "Return only valid JSON with exactly this object shape:\n",
      field_json,
      "\nUse numbers for rating fields. Do not include markdown, prose, or code fences."
    )
  } else {
    format_instruction <- paste(
      "Return only valid JSON.",
      "Do not include markdown, prose, or code fences."
    )
  }

  paste(
    prompt,
    "",
    "Response format requirement:",
    format_instruction,
    sep = "\n"
  )
}

parse_soft_structured_response <- function(raw_response, fields = NULL) {
  raw_text <- normalize_chat_text(raw_response)
  parsed <- tryCatch(
    parse_json_object_response(raw_text),
    error = function(e) {
      if (!is.null(fields) && length(fields) == 1) {
        return(parse_single_field_response(raw_text, fields[[1]]))
      }
      stop(e)
    }
  )

  parsed <- as.list(parsed)
  if (!is.null(fields) && length(fields) > 0) {
    missing_fields <- setdiff(fields, names(parsed))
    if (length(missing_fields) > 0) {
      stop(
        "Text output could not be parsed into the expected fields: ",
        paste(missing_fields, collapse = ", "),
        ". Raw response: ",
        raw_text,
        call. = FALSE
      )
    }
    parsed <- parsed[fields]
  }

  parsed <- lapply(parsed, coerce_soft_structured_value)
  parsed$raw_response <- raw_text
  parsed
}

normalize_chat_text <- function(raw_response) {
  if (is.character(raw_response)) {
    return(paste(raw_response, collapse = "\n"))
  }
  if (is.list(raw_response) && !is.null(raw_response$text)) {
    return(as.character(raw_response$text))
  }
  if (is.list(raw_response) && !is.null(raw_response$content)) {
    return(as.character(raw_response$content))
  }
  as.character(raw_response)
}

parse_json_object_response <- function(raw_text) {
  json_text <- trimws(raw_text)
  json_text <- sub("^```(?:json)?\\s*", "", json_text, ignore.case = TRUE)
  json_text <- sub("\\s*```$", "", json_text)

  start <- regexpr("\\{", json_text)[[1]]
  end_matches <- gregexpr("\\}", json_text)[[1]]
  end_matches <- end_matches[end_matches > 0]
  if (start < 0 || length(end_matches) == 0 || max(end_matches) < start) {
    stop("No JSON object found in text response.", call. = FALSE)
  }

  json_text <- substr(json_text, start, max(end_matches))
  parsed <- jsonlite::fromJSON(json_text, simplifyVector = FALSE)
  if (!is.list(parsed) || is.null(names(parsed))) {
    stop("Text response JSON must be an object.", call. = FALSE)
  }
  parsed
}

parse_single_field_response <- function(raw_text, field) {
  text <- trimws(raw_text)
  numeric_match <- regexpr("[-+]?[0-9]*\\.?[0-9]+", text, perl = TRUE)
  value <- if (numeric_match[[1]] > 0) {
    as.numeric(regmatches(text, numeric_match))
  } else {
    text
  }
  stats::setNames(list(value), field)
}

coerce_soft_structured_value <- function(value) {
  if (is.character(value) && length(value) == 1) {
    text <- trimws(value)
    if (grepl("^[-+]?[0-9]*\\.?[0-9]+$", text)) {
      return(as.numeric(text))
    }
  }
  value
}

soft_raw_response_field <- function(response, name, output_mode) {
  if (!identical(output_mode, "text")) {
    return(list())
  }
  stats::setNames(list(response_raw_response(response)), name)
}

response_raw_response <- function(response) {
  raw_response <- if (is.list(response) && !is.null(response$raw_response)) {
    response$raw_response
  } else {
    NA_character_
  }
  raw_response
}

format_progress_label <- function(
  book,
  chapter,
  identity,
  sim,
  model = NULL,
  book_pct = NULL,
  chapter_pct = NULL
) {
  book_label <- if (length(book) == 1 && !is.na(book) && nzchar(as.character(book))) {
    as.character(book)
  } else {
    ""
  }
  if (!is.null(book_pct) && nzchar(book_label)) {
    book_label <- paste0(book_label, " [", book_pct, "]")
  }

  chapter_label <- if (length(chapter) == 1 && !is.na(chapter) && nzchar(as.character(chapter))) {
    as.character(chapter)
  } else {
    ""
  }
  if (!is.null(chapter_pct) && nzchar(chapter_label)) {
    chapter_label <- paste0(chapter_label, " [", chapter_pct, "]")
  }

  model_suffix <- if (!is.null(model) && length(model) == 1 && !is.na(model) && nzchar(as.character(model))) {
    paste0("; ", model)
  } else {
    ""
  }

  if (nzchar(book_label)) {
    paste0(book_label, " - ", chapter_label, " [", identity, "] (sim ", sim, model_suffix, ")")
  } else {
    paste0(chapter_label, " [", identity, "] (sim ", sim, model_suffix, ")")
  }
}

is_missing_chapter_text <- function(chapter_text) {
  length(chapter_text) != 1L || is.na(chapter_text)
}

inform_missing_chapter_jobs <- function(chapter_jobs) {
  missing <- vapply(
    chapter_jobs$chapter_text,
    is_missing_chapter_text,
    logical(1)
  )
  if (!any(missing)) {
    return(invisible(NULL))
  }

  labels <- paste0(chapter_jobs$chapter[missing])
  has_book <- !is.na(chapter_jobs$book[missing]) & nzchar(chapter_jobs$book[missing])
  labels[has_book] <- paste0(
    chapter_jobs$book[missing][has_book],
    " - ",
    labels[has_book]
  )

  message(
    "Skipping ",
    sum(missing),
    " chapter(s) with missing `chapter_text`: ",
    paste(labels, collapse = ", ")
  )

  invisible(NULL)
}

is_unrecoverable_model_error <- function(error) {
  !is.null(unrecoverable_model_error_type(error))
}

unrecoverable_model_error_type <- function(error) {
  message <- conditionMessage(error)

  if (grepl("Model .+ is not allowed for this integration", message)) {
    return("model_route")
  }

  # curl error 7: the request never reached the gateway. Retrying the same
  # request for every chapter cannot recover a missing VPN/network route.
  if (
    grepl("Failed to connect to .+ port [0-9]+", message, ignore.case = TRUE) ||
      grepl("Could not connect to server", message, fixed = TRUE)
  ) {
    return("gateway_connection")
  }

  NULL
}

stop_unrecoverable_model_error <- function(error, model_label) {
  error_type <- unrecoverable_model_error_type(error)

  if (identical(error_type, "gateway_connection")) {
    stop(
      "Could not connect to the AI gateway for `",
      model_label,
      "`: ",
      conditionMessage(error),
      "\nThis is a gateway/network failure, not a chapter-specific error, so ",
      "`on_error = \"skip\"` cannot recover. Check your network connection ",
      "and connect to the required VPN before rerunning.",
      call. = FALSE
    )
  }

  stop(
    "Model route validation failed for `",
    model_label,
    "`: ",
    conditionMessage(error),
    "\nThis error is not chapter-specific, so `on_error = \"skip\"` cannot recover. ",
    "Check `model`, `integration`, and `nalanda.integration`.",
    call. = FALSE
  )
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

finalize_simulation_output <- function(out_rows, long_cols, chapter_jobs) {
  out_tbl <- rows_to_tibble(out_rows, long_cols = long_cols)

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

add_model_column_to_output <- function(out, model_label) {
  if (inherits(out, "data.frame")) {
    out <- tibble::as_tibble(out)
    out$model <- model_label
    return(out[, c("model", setdiff(names(out), "model"))])
  }

  if (is.list(out)) {
    for (nm in names(out)) {
      out[[nm]] <- tibble::as_tibble(out[[nm]])
      out[[nm]]$model <- model_label
      out[[nm]] <- out[[nm]][, c("model", setdiff(names(out[[nm]]), "model"))]
    }
    return(out)
  }

  out
}

merge_simulation_outputs <- function(outputs) {
  outputs <- Filter(Negate(is.null), outputs)
  if (length(outputs) == 0) {
    return(NULL)
  }

  first <- outputs[[1]]
  if (inherits(first, "data.frame")) {
    out <- dplyr::bind_rows(outputs)
    attrs <- attributes(first)
    attr(out, "chapter_excerpts") <- attrs$chapter_excerpts
    return(out)
  }

  book_names <- unique(unlist(lapply(outputs, names), use.names = FALSE))
  out <- stats::setNames(vector("list", length(book_names)), book_names)

  for (book_name in book_names) {
    pieces <- Filter(
      function(x) !is.null(x[[book_name]]),
      outputs
    )
    out[[book_name]] <- dplyr::bind_rows(lapply(pieces, `[[`, book_name))
    first_piece <- pieces[[1]][[book_name]]
    attr(out[[book_name]], "chapter_excerpts") <- attr(first_piece, "chapter_excerpts")
  }

  attr(out, "chapter_excerpts") <- attr(first, "chapter_excerpts")
  out
}
