#' Plan a multi-model, multi-prompt grid
#'
#' Validate model and prompt configuration tables and estimate the number of
#' model responses before making any calls. This is the dry-run companion to
#' [run_prompt_grid()]. Prompt variants are independent calls, not sequential
#' turns in one conversation.
#'
#' @param data A data frame accepted by [run_structured_responses()].
#' @param prompt_variants A named character vector of independent prompt
#'   templates, or a data frame
#'   with `prompt_id` and `prompt` columns. An optional logical `active` column
#'   can disable prompt variants without deleting them.
#' @param model_config A character vector of model names, or a data frame with a
#'   required `model` column. Supported optional columns are `model_id`,
#'   `family`, `integration`, `virtual_key`, `base_url`, `temperature`,
#'   `output_mode`, `seed`, `max_active`, `rpm`, `n_completions`, and `active`.
#'   Missing settings use the same defaults as [run_structured_responses()].
#' @param smoke_n Optional positive integer limiting the workflow to the first
#'   `smoke_n` input rows. `smoke_n = 1` is useful before an expensive run.
#'
#' @return A tibble with one row per active model-prompt combination and
#'   columns for input rows, completion batches, and estimated response calls.
#' @export
plan_prompt_grid <- function(data, prompt_variants, model_config, smoke_n = NULL) {
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data frame.")
  }

  model_config <- normalize_prompt_model_config(model_config)
  prompt_config <- normalize_prompt_variant_config(prompt_variants)
  n_rows <- workflow_n_rows(data, smoke_n)

  rows <- vector("list", nrow(model_config) * nrow(prompt_config))
  row_i <- 0L
  for (model_i in seq_len(nrow(model_config))) {
    for (prompt_i in seq_len(nrow(prompt_config))) {
      row_i <- row_i + 1L
      model_row <- model_config[model_i, , drop = FALSE]
      prompt_row <- prompt_config[prompt_i, , drop = FALSE]
      rows[[row_i]] <- tibble::tibble(
        model_id = model_row$model_id,
        model = model_row$model,
        family = model_row$family,
        integration = model_row$integration,
        temperature = model_row$temperature,
        output_mode = model_row$output_mode,
        prompt_id = prompt_row$prompt_id,
        n_rows = n_rows,
        n_completions = model_row$n_completions,
        completion_batches = model_row$n_completions,
        estimated_calls = n_rows * model_row$n_completions
      )
    }
  }

  out <- dplyr::bind_rows(rows)
  attr(out, "total_estimated_calls") <- sum(out$estimated_calls)
  attr(out, "smoke_n") <- smoke_n
  out
}

#' Run a multi-model, multi-prompt grid
#'
#' This function is a deliberately thin orchestration layer over
#' [run_structured_responses()]. It expands validated active model and prompt
#' configurations, runs each completion as an independently resumable unit,
#' and keeps model, family, prompt, and completion provenance in the raw
#' results. [run_text_analysis()] remains available as the text-annotation
#' use-case wrapper.
#'
#' @inheritParams plan_prompt_grid
#' @param content_col,id_col,response_type Passed to
#'   [run_structured_responses()].
#' @param dry_run Logical. If `TRUE`, return the call plan without constructing
#'   a chat or making model calls.
#' @param output_dir Optional directory for one RDS checkpoint per completed
#'   model-prompt-completion unit. Files contain raw, unaggregated results.
#' @param resume Logical. If `TRUE` and `output_dir` is supplied, compatible
#'   completed units are read instead of rerun. Checkpoint identity includes
#'   inputs, prompts, response type, and model settings.
#' @param on_error Either `"stop"` or `"continue"`. With `"continue"`, failed
#'   units are recorded in the returned `errors` table and are not checkpointed,
#'   so a later resumed run retries them.
#' @param progress Logical. Emit one concise progress message per completion.
#' @param integration,virtual_key,base_url,excerpt_chars,max_active,rpm Global
#'   defaults passed to [run_structured_responses()]. A non-missing value in a model
#'   configuration row overrides the corresponding global default.
#'
#' @return If `dry_run = TRUE`, the plan tibble. Otherwise, a list with
#'   `results` (combined raw results), `plan`, `tasks`, and `errors`.
#' @export
run_prompt_grid <- function(
  data,
  content_col = "text",
  prompt_variants,
  response_type,
  model_config,
  id_col = NULL,
  smoke_n = NULL,
  dry_run = FALSE,
  output_dir = NULL,
  resume = TRUE,
  on_error = c("stop", "continue"),
  progress = interactive(),
  integration = getOption("nalanda.integration"),
  virtual_key = getOption("nalanda.virtual_key"),
  base_url = getOption("nalanda.base_url"),
  excerpt_chars = 200,
  max_active = 10,
  rpm = 500
) {
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data frame.")
  }
  if (!is.character(content_col) || length(content_col) != 1 || !nzchar(content_col) ||
      !content_col %in% names(data)) {
    stop("`content_col` was not found in `data`.")
  }
  if (!is.null(id_col) &&
      (!is.character(id_col) || length(id_col) != 1 || !nzchar(id_col) ||
       !id_col %in% names(data))) {
    stop("`id_col` was not found in `data`.")
  }
  if (missing(response_type) || is.null(response_type)) {
    stop("Please provide `response_type`.")
  }
  if (!is.logical(dry_run) || length(dry_run) != 1 || is.na(dry_run)) {
    stop("`dry_run` must be TRUE or FALSE.")
  }
  if (!is.logical(resume) || length(resume) != 1 || is.na(resume)) {
    stop("`resume` must be TRUE or FALSE.")
  }
  if (!is.logical(progress) || length(progress) != 1 || is.na(progress)) {
    stop("`progress` must be TRUE or FALSE.")
  }
  on_error <- match.arg(on_error)

  reserved <- c(
    "model_id", "model", "family", "prompt_id", "prompt_template",
    "completion", "integration", "temperature", "output_mode", "seed",
    "prompt"
  )
  collisions <- intersect(reserved, names(data))
  if (length(collisions) > 0) {
    stop(
      "`data` contains workflow provenance column(s): ",
      paste(collisions, collapse = ", "), "."
    )
  }

  model_config <- normalize_prompt_model_config(model_config)
  prompt_config <- normalize_prompt_variant_config(prompt_variants)
  plan <- plan_prompt_grid(
    data = data,
    prompt_variants = prompt_config,
    model_config = model_config,
    smoke_n = smoke_n
  )
  if (dry_run) {
    return(plan)
  }
  if (nrow(data) == 0) {
    stop("`data` must contain at least one row when `dry_run = FALSE`.")
  }

  if (!is.null(output_dir)) {
    if (!is.character(output_dir) || length(output_dir) != 1 || !nzchar(output_dir)) {
      stop("`output_dir` must be a single non-empty path, or `NULL`.")
    }
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(output_dir)) {
      stop("Could not create `output_dir`: ", output_dir)
    }
  }

  data_run <- tibble::as_tibble(data)[seq_len(workflow_n_rows(data, smoke_n)), , drop = FALSE]
  tasks <- expand_prompt_grid_tasks(model_config, prompt_config)
  result_rows <- vector("list", nrow(tasks))
  task_rows <- vector("list", nrow(tasks))
  error_rows <- list()
  error_i <- 0L

  for (task_i in seq_len(nrow(tasks))) {
    task <- tasks[task_i, , drop = FALSE]
    task_label <- paste0(
      task$model_id, "/", task$prompt_id, "/completion-", task$completion
    )
    if (progress) {
      message("[", task_i, "/", nrow(tasks), "] ", task_label)
    }

    task_route <- workflow_route_settings(
      configured_integration = task$integration,
      configured_virtual_key = task$virtual_key,
      default_integration = integration,
      default_virtual_key = virtual_key
    )
    task_integration <- task_route$integration
    task_virtual_key <- task_route$virtual_key
    task_base_url <- workflow_setting(task$base_url, base_url)
    task_max_active <- workflow_setting(task$max_active, max_active)
    task_rpm <- workflow_setting(task$rpm, rpm)
    task_seed <- task$seed + task$completion - 1L

    spec <- list(
      version = 1L,
      data = data_run,
      content_col = content_col,
      id_col = id_col,
      prompt_id = task$prompt_id,
      prompt = task$prompt,
      response_type = response_type,
      model_id = task$model_id,
      model = task$model,
      family = task$family,
      integration = task_integration,
      virtual_key = task_virtual_key,
      base_url = task_base_url,
      temperature = task$temperature,
      output_mode = task$output_mode,
      seed = task_seed,
      excerpt_chars = excerpt_chars,
      max_active = task_max_active,
      rpm = task_rpm
    )
    spec_hash <- workflow_object_md5(spec)
    checkpoint_path <- if (is.null(output_dir)) {
      NA_character_
    } else {
      file.path(
        output_dir,
        paste0(
          workflow_file_part(task$model_id), "--",
          workflow_file_part(task$prompt_id), "--completion-",
          sprintf("%03d", task$completion), "--",
          substr(spec_hash, 1, 12), ".rds"
        )
      )
    }

    checkpoint <- NULL
    if (resume && !is.na(checkpoint_path) && file.exists(checkpoint_path)) {
      checkpoint <- tryCatch(readRDS(checkpoint_path), error = function(e) NULL)
      if (!is.list(checkpoint) || !identical(checkpoint$spec_hash, spec_hash) ||
          !inherits(checkpoint$result, "data.frame")) {
        checkpoint <- NULL
      }
    }

    if (!is.null(checkpoint)) {
      result_rows[[task_i]] <- checkpoint$result
      task_rows[[task_i]] <- workflow_task_status(task, "resumed", checkpoint_path)
      next
    }

    result <- tryCatch(
      run_structured_responses(
        data = data_run,
        content_col = content_col,
        prompt = task$prompt,
        response_type = response_type,
        output_mode = task$output_mode,
        id_col = id_col,
        n_completions = 1L,
        temperature = task$temperature,
        seed = task_seed,
        model = task$model,
        integration = task_integration,
        virtual_key = task_virtual_key,
        base_url = task_base_url,
        excerpt_chars = excerpt_chars,
        max_active = task_max_active,
        rpm = task_rpm
      ),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      if (identical(on_error, "stop")) {
        stop("Workflow unit `", task_label, "` failed: ", conditionMessage(result), call. = FALSE)
      }
      error_i <- error_i + 1L
      error_rows[[error_i]] <- tibble::tibble(
        model_id = task$model_id,
        prompt_id = task$prompt_id,
        completion = task$completion,
        message = conditionMessage(result)
      )
      task_rows[[task_i]] <- workflow_task_status(task, "error", checkpoint_path)
      next
    }

    result$completion <- task$completion
    result$prompt_id <- task$prompt_id
    result$prompt_template <- task$prompt
    result$model_id <- task$model_id
    result$model <- task$model
    result$family <- task$family
    result$integration <- if (is.null(task_integration)) NA_character_ else task_integration
    result$temperature <- task$temperature
    result$output_mode <- task$output_mode
    result$seed <- task_seed

    if (!is.na(checkpoint_path)) {
      write_prompt_grid_checkpoint(
        path = checkpoint_path,
        checkpoint = list(
          version = 1L,
          spec_hash = spec_hash,
          result = result
        )
      )
    }

    result_rows[[task_i]] <- result
    task_rows[[task_i]] <- workflow_task_status(task, "completed", checkpoint_path)
  }

  results <- dplyr::bind_rows(result_rows)
  if (nrow(results) > 0) {
    class(results) <- unique(c("nalanda", class(results)))
  }
  errors <- if (length(error_rows) == 0) {
    tibble::tibble(
      model_id = character(), prompt_id = character(),
      completion = integer(), message = character()
    )
  } else {
    dplyr::bind_rows(error_rows)
  }

  list(
    results = results,
    plan = plan,
    tasks = dplyr::bind_rows(task_rows),
    errors = errors
  )
}

#' Aggregate structured forecasts through an explicit weighting hierarchy
#'
#' Average repeated completions within prompt, prompts within model, models
#' within family, and then families into a consensus. Each stage operates on
#' the already-aggregated rows from the previous stage, so unequal completion
#' counts do not give a model more weight. Supplying `family_col` explicitly
#' requests equal family weight at the final stage; with `family_col = NULL`,
#' the final consensus gives models equal weight.
#'
#' This helper only computes arithmetic means. Choosing this hierarchy, deciding
#' whether model outputs represent simulations or expert forecasts, and any
#' uncertainty analysis remain scientific decisions for the caller.
#'
#' @param data Raw workflow results in long row form.
#' @param outcomes Character vector naming numeric forecast columns.
#' @param unit_by Character vector of columns that uniquely identify a forecast
#'   target, such as a condition ID. Include every necessary unit column.
#' @param completion_col,prompt_col,model_col Column names identifying the
#'   repeated completion, prompt variant, and model.
#' @param family_col Optional column identifying model families. Set to `NULL`
#'   to average models directly into the consensus.
#'
#' @return A named list of tibbles: `prompt`, `model`, optional `family`, and
#'   `consensus`. Count columns make the weight at each stage auditable.
#' @export
aggregate_model_forecasts <- function(
  data,
  outcomes,
  unit_by,
  completion_col = "completion",
  prompt_col = "prompt_id",
  model_col = "model_id",
  family_col = "family"
) {
  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data frame.")
  }
  if (!is.character(outcomes) || length(outcomes) < 1 || any(!nzchar(outcomes))) {
    stop("`outcomes` must contain at least one column name.")
  }
  if (!is.character(unit_by) || length(unit_by) < 1 || any(!nzchar(unit_by))) {
    stop("`unit_by` must contain at least one column name.")
  }
  scalar_ids <- list(completion_col, prompt_col, model_col)
  if (any(vapply(
    scalar_ids,
    function(x) !is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x),
    logical(1)
  ))) {
    stop("Completion, prompt, and model columns must each be one column name.")
  }
  if (!is.null(family_col) &&
      (!is.character(family_col) || length(family_col) != 1 ||
       is.na(family_col) || !nzchar(family_col))) {
    stop("`family_col` must be one column name or `NULL`.")
  }
  id_cols <- c(unit_by, completion_col, prompt_col, model_col, family_col)
  if (anyDuplicated(outcomes)) stop("`outcomes` column names must be unique.")
  overlap <- intersect(outcomes, id_cols)
  if (length(overlap) > 0) {
    stop("Outcome columns cannot also be grouping columns: ", paste(overlap, collapse = ", "))
  }
  required <- unique(c(id_cols, outcomes))
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))
  }
  non_numeric <- outcomes[!vapply(data[outcomes], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    stop("All `outcomes` columns must be numeric: ", paste(non_numeric, collapse = ", "))
  }

  key_cols <- unique(id_cols)
  if (anyDuplicated(data[key_cols])) {
    stop(
      "Rows are not unique within unit, model, prompt, and completion. ",
      "Add columns to `unit_by` or resolve duplicates before aggregation."
    )
  }

  family_group <- if (is.null(family_col)) character() else family_col
  prompt_groups <- unique(c(unit_by, family_group, model_col, prompt_col))
  prompt_level <- workflow_mean_stage(
    data, prompt_groups, outcomes, completion_col, "n_completions"
  )

  model_groups <- unique(c(unit_by, family_group, model_col))
  model_level <- workflow_mean_stage(
    prompt_level, model_groups, outcomes, prompt_col, "n_prompts"
  )

  if (is.null(family_col)) {
    family_level <- NULL
    consensus <- workflow_mean_stage(
      model_level, unit_by, outcomes, model_col, "n_models"
    )
  } else {
    family_groups <- unique(c(unit_by, family_col))
    family_level <- workflow_mean_stage(
      model_level, family_groups, outcomes, model_col, "n_models"
    )
    consensus <- workflow_mean_stage(
      family_level, unit_by, outcomes, family_col, "n_families"
    )
  }

  list(
    prompt = prompt_level,
    model = model_level,
    family = family_level,
    consensus = consensus
  )
}

normalize_prompt_model_config <- function(model_config) {
  if (is.character(model_config)) {
    model_ids <- names(model_config)
    if (is.null(model_ids) || any(!nzchar(model_ids))) {
      model_ids <- unname(model_config)
    }
    model_config <- tibble::tibble(
      model_id = model_ids,
      model = unname(model_config)
    )
  }
  if (!inherits(model_config, "data.frame") ||
      !"model" %in% names(model_config)) {
    stop(
      "`model_config` must be a character vector or a data frame with a ",
      "`model` column."
    )
  }

  out <- tibble::as_tibble(model_config)
  n <- nrow(out)
  if (n < 1) stop("`model_config` must contain at least one row.")
  if (!"model_id" %in% names(out)) out$model_id <- out$model
  if (!"family" %in% names(out)) out$family <- out$model_id
  if (!"integration" %in% names(out)) out$integration <- NA_character_
  if (!"virtual_key" %in% names(out)) out$virtual_key <- NA_character_
  if (!"base_url" %in% names(out)) out$base_url <- NA_character_
  if (!"temperature" %in% names(out)) out$temperature <- 0
  if (!"output_mode" %in% names(out)) out$output_mode <- "structured"
  if (!"seed" %in% names(out)) out$seed <- 42L
  if (!"max_active" %in% names(out)) out$max_active <- NA_integer_
  if (!"rpm" %in% names(out)) out$rpm <- NA_integer_
  if (!"n_completions" %in% names(out)) out$n_completions <- 1L
  if (!"active" %in% names(out)) out$active <- TRUE

  character_cols <- c("model_id", "model", "family", "output_mode")
  for (nm in character_cols) {
    if (!is.character(out[[nm]]) || anyNA(out[[nm]]) || any(!nzchar(out[[nm]]))) {
      stop("`model_config$", nm, "` must contain non-empty strings.")
    }
  }
  nullable_character <- c("integration", "virtual_key", "base_url")
  for (nm in nullable_character) {
    if (!is.character(out[[nm]]) || any(!is.na(out[[nm]]) & !nzchar(out[[nm]]))) {
      stop("`model_config$", nm, "` must contain strings or `NA`.")
    }
  }
  if (anyDuplicated(out$model_id)) {
    stop("`model_config$model_id` values must be unique.")
  }
  if (!is.logical(out$active) || anyNA(out$active)) {
    stop("`model_config$active` must contain TRUE or FALSE.")
  }
  if (!is.numeric(out$temperature) || anyNA(out$temperature)) {
    stop("`model_config$temperature` must be numeric and non-missing.")
  }
  if (!is.numeric(out$n_completions) || anyNA(out$n_completions) ||
      any(out$n_completions < 1) || any(out$n_completions %% 1 != 0)) {
    stop("`model_config$n_completions` must contain positive integers.")
  }
  if (!is.numeric(out$seed) || anyNA(out$seed) || any(out$seed %% 1 != 0)) {
    stop("`model_config$seed` must contain integers.")
  }
  if (any(!out$output_mode %in% c("structured", "text"))) {
    stop("`model_config$output_mode` must contain `structured` or `text`.")
  }
  for (nm in c("max_active", "rpm")) {
    value <- out[[nm]]
    if (!is.numeric(value) || any(!is.na(value) & (value < 1 | value %% 1 != 0))) {
      stop("`model_config$", nm, "` must contain positive integers or `NA`.")
    }
  }

  out$n_completions <- as.integer(out$n_completions)
  out$seed <- as.integer(out$seed)
  out$max_active <- as.integer(out$max_active)
  out$rpm <- as.integer(out$rpm)
  out <- out[out$active, , drop = FALSE]
  if (nrow(out) < 1) stop("`model_config` has no active rows.")
  both_routes <- !is.na(out$integration) & !is.na(out$virtual_key)
  if (any(both_routes)) {
    stop("Each model row may specify only one of `integration` and `virtual_key`.")
  }
  for (i in seq_len(nrow(out))) {
    validate_model_parameters(out$model[[i]], out$temperature[[i]])
  }
  out
}

normalize_prompt_variant_config <- function(prompt_variants) {
  if (is.character(prompt_variants)) {
    prompt_ids <- names(prompt_variants)
    if (is.null(prompt_ids) || any(!nzchar(prompt_ids))) {
      prompt_ids <- paste0("prompt_", seq_along(prompt_variants))
    }
    prompt_variants <- tibble::tibble(
      prompt_id = prompt_ids,
      prompt = unname(prompt_variants)
    )
  }
  if (!inherits(prompt_variants, "data.frame") ||
      !all(c("prompt_id", "prompt") %in% names(prompt_variants))) {
    stop(
      "`prompt_variants` must be a character vector or a data frame with ",
      "`prompt_id` and `prompt` columns."
    )
  }
  out <- tibble::as_tibble(prompt_variants)
  if (nrow(out) < 1) stop("`prompt_variants` must contain at least one row.")
  for (nm in c("prompt_id", "prompt")) {
    if (!is.character(out[[nm]]) || anyNA(out[[nm]]) || any(!nzchar(out[[nm]]))) {
      stop("`prompt_variants$", nm, "` must contain non-empty strings.")
    }
  }
  if (anyDuplicated(out$prompt_id)) {
    stop("`prompt_variants$prompt_id` values must be unique.")
  }
  if (!"active" %in% names(out)) out$active <- TRUE
  if (!is.logical(out$active) || anyNA(out$active)) {
    stop("`prompt_variants$active` must contain TRUE or FALSE.")
  }
  out <- out[out$active, , drop = FALSE]
  if (nrow(out) < 1) stop("`prompt_variants` has no active rows.")
  out
}

workflow_n_rows <- function(data, smoke_n) {
  if (is.null(smoke_n)) return(nrow(data))
  if (!is.numeric(smoke_n) || length(smoke_n) != 1 || is.na(smoke_n) ||
      smoke_n < 1 || smoke_n %% 1 != 0) {
    stop("`smoke_n` must be a positive integer or `NULL`.")
  }
  min(nrow(data), as.integer(smoke_n))
}

expand_prompt_grid_tasks <- function(models, prompts) {
  rows <- list()
  row_i <- 0L
  for (model_i in seq_len(nrow(models))) {
    for (prompt_i in seq_len(nrow(prompts))) {
      for (completion_i in seq_len(models$n_completions[[model_i]])) {
        row_i <- row_i + 1L
        rows[[row_i]] <- cbind(
          models[model_i, , drop = FALSE],
          prompts[prompt_i, setdiff(names(prompts), "active"), drop = FALSE],
          completion = completion_i
        )
      }
    }
  }
  tibble::as_tibble(dplyr::bind_rows(rows))
}

workflow_setting <- function(value, fallback) {
  value <- value[[1]]
  if (length(value) == 0 || is.na(value)) fallback else value
}

workflow_route_settings <- function(
  configured_integration,
  configured_virtual_key,
  default_integration,
  default_virtual_key
) {
  configured_integration <- configured_integration[[1]]
  configured_virtual_key <- configured_virtual_key[[1]]
  if (!is.na(configured_integration)) {
    return(list(integration = configured_integration, virtual_key = NULL))
  }
  if (!is.na(configured_virtual_key)) {
    return(list(integration = NULL, virtual_key = configured_virtual_key))
  }
  resolve_model_route(
    integration = default_integration,
    virtual_key = default_virtual_key,
    integration_missing = TRUE,
    virtual_key_missing = TRUE
  )
}

workflow_object_md5 <- function(x) {
  path <- tempfile("nalanda-workflow-", fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(x, path, version = 2)
  unname(tools::md5sum(path)[[1]])
}

workflow_file_part <- function(x) {
  out <- gsub("[^A-Za-z0-9._-]+", "-", as.character(x))
  out <- gsub("^-+|-+$", "", out)
  substr(ifelse(nzchar(out), out, "unit"), 1, 60)
}

write_prompt_grid_checkpoint <- function(path, checkpoint) {
  tmp <- tempfile(paste0(".", basename(path), "."), dirname(path), ".tmp")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(checkpoint, tmp, version = 2)
  if (!file.copy(tmp, path, overwrite = TRUE)) {
    stop("Could not write workflow checkpoint: ", path)
  }
  invisible(path)
}

workflow_task_status <- function(task, status, path) {
  tibble::tibble(
    model_id = task$model_id,
    prompt_id = task$prompt_id,
    completion = task$completion,
    status = status,
    path = path
  )
}

workflow_mean_stage <- function(data, groups, outcomes, count_col, count_name) {
  grouped <- dplyr::group_by(data, dplyr::across(dplyr::all_of(groups)))
  dplyr::summarise(
    grouped,
    dplyr::across(
      dplyr::all_of(outcomes),
      function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
    ),
    !!count_name := dplyr::n_distinct(.data[[count_col]]),
    .groups = "drop"
  )
}
