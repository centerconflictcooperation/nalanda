#' Plan and run a multi-turn survey simulation
#'
#' `run_survey_simulation()` simulates respondents moving through ordered survey
#' screens.  Unlike [run_prompt_grid()], screens are turns for the *same*
#' respondent.  `participants` is always used row-wise: it is never crossed
#' with conditions or profile fields.
#'
#' @param participants A data frame containing a unique `participant_id` column
#'   (or the column named by `participant_id`).  Other columns are available as
#'   `{placeholders}` in every screen prompt.
#' @param survey_flow A data frame with unique `screen_id` and `prompt` columns.
#'   Optional columns are `block`, `response_type` (a list-column of ellmer
#'   types), `output_mode`, and `display_if` (a logical scalar or a function of
#'   `(participant, answers)`).  `answers` is a named list of prior responses.
#' @param participant_id Stable participant identifier column.
#' @param model_config Model configuration accepted by [run_prompt_grid()].
#' @param memory One of `"conversation"` (all preceding turns remain in the
#'   chat), `"profile"` (a fresh chat for each screen), or `"block"` (a fresh
#'   chat at each block).  Profile/context placeholders are included in every
#'   prompt under all policies.
#' @param dry_run Return a task plan without model calls.
#' @param smoke_n Restrict to the first participants for an inexpensive run.
#' @param output_dir Optional directory for atomic per-turn checkpoints.
#' @param resume Reuse compatible completed turn checkpoints.
#' @param on_error Either `"stop"` or `"continue"`.
#' @param ... Global connection defaults: `integration`, `virtual_key`,
#'   `base_url`, `excerpt_chars`, `max_active`, and `rpm`.  The latter two are
#'   recorded as provenance; turns for one respondent are deliberately serial.
#'
#' @return `plan_survey_simulation()` returns task rows.  The runner returns a
#'   list with turn-level `results`, respondent-level `wide`, `plan`, `tasks`,
#'   and `errors`.
#' @export
plan_survey_simulation <- function(
  participants,
  survey_flow,
  participant_id = "participant_id",
  model_config,
  smoke_n = NULL
) {
  p <- survey_participants(participants, participant_id, smoke_n)
  f <- survey_flow_config(survey_flow)
  m <- normalize_prompt_model_config(model_config)
  out <- list()
  k <- 0L
  for (i in seq_len(nrow(m))) {
    for (j in seq_len(nrow(p))) {
      for (z in seq_len(m$n_completions[[i]])) {
        k <- k + 1L
        out[[k]] <- tibble::tibble(
          model_id = m$model_id[[i]],
          model = m$model[[i]],
          family = m$family[[i]],
          completion = z,
          participant_id = as.character(p[[participant_id]][[j]]),
          n_screens = nrow(f),
          estimated_calls = nrow(f)
        )
      }
    }
  }
  dplyr::bind_rows(out)
}

#' @rdname plan_survey_simulation
#' @export
run_survey_simulation <- function(
  participants,
  survey_flow,
  participant_id = "participant_id",
  model_config,
  memory = c("conversation", "profile", "block"),
  dry_run = FALSE,
  smoke_n = NULL,
  output_dir = NULL,
  resume = TRUE,
  on_error = c("stop", "continue"),
  integration = getOption("nalanda.integration"),
  virtual_key = getOption("nalanda.virtual_key"),
  base_url = getOption("nalanda.base_url"),
  excerpt_chars = 200,
  max_active = 10,
  rpm = 500
) {
  memory <- match.arg(memory)
  on_error <- match.arg(on_error)
  p <- survey_participants(participants, participant_id, smoke_n)
  f <- survey_flow_config(survey_flow)
  m <- normalize_prompt_model_config(model_config)
  plan <- plan_survey_simulation(p, f, participant_id, m)
  if (dry_run) {
    return(plan)
  }
  if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  rows <- list()
  tasks <- list()
  errors <- list()
  ri <- ti <- ei <- 0L
  for (mi in seq_len(nrow(m))) {
    for (pi in seq_len(nrow(p))) {
      for (co in seq_len(m$n_completions[[mi]])) {
        participant <- p[pi, , drop = FALSE]
        pid <- as.character(participant[[participant_id]][[1]])
        spec <- list(
          version = 1L,
          participant = participant,
          flow = f,
          model = m[mi, , drop = FALSE],
          completion = co,
          memory = memory,
          participant_id = participant_id
        )
        task_hash <- workflow_object_md5(spec)
        answers <- list()
        history <- list()
        chat <- NULL
        previous_block <- NULL
        rebuilt_history <- FALSE
        for (si in seq_len(nrow(f))) {
          screen <- f[si, , drop = FALSE]
          screen_id <- screen$screen_id[[1]]
          if (
            !survey_screen_visible(screen$display_if[[1]], participant, answers)
          ) {
            next
          }
          path <- if (is.null(output_dir)) {
            NA_character_
          } else {
            file.path(
              output_dir,
              paste0(
                workflow_file_part(m$model_id[[mi]]),
                "--",
                workflow_file_part(pid),
                "--",
                sprintf("%03d", co),
                "--",
                workflow_file_part(screen_id),
                "--",
                substr(task_hash, 1, 12),
                ".rds"
              )
            )
          }
          checkpoint <- if (resume && !is.na(path) && file.exists(path)) {
            tryCatch(readRDS(path), error = function(e) NULL)
          } else {
            NULL
          }
          if (
            is.list(checkpoint) &&
              identical(checkpoint$task_hash, task_hash) &&
              inherits(checkpoint$row, "data.frame")
          ) {
            one <- checkpoint$row
            answers[[screen_id]] <- one$response[[1]]
            history[[screen_id]] <- list(
              prompt = one$prompt[[1]],
              response = one$response[[1]]
            )
            rebuilt_history <- TRUE
            ri <- ri + 1L
            rows[[ri]] <- one
            ti <- ti + 1L
            tasks[[ti]] <- survey_task_row(
              m,
              mi,
              pid,
              co,
              screen_id,
              task_hash,
              "resumed",
              path
            )
            next
          }
          block <- screen$block[[1]]
          if (
            is.null(chat) ||
              identical(memory, "profile") ||
              (identical(memory, "block") && !identical(block, previous_block))
          ) {
            chat <- survey_new_chat(
              m[mi, , drop = FALSE],
              co,
              integration,
              virtual_key,
              base_url
            )
          }
          previous_block <- block
          prompt <- interpolate_prompt_template(
            screen$prompt[[1]],
            as.list(participant)
          )
          if (
            identical(memory, "conversation") &&
              rebuilt_history &&
              length(history) > 0L
          ) {
            prompt <- paste0(
              "Earlier completed survey screens (do not answer them again):\n",
              survey_history_text(history),
              "\n\nCurrent screen:\n",
              prompt
            )
          }
          type <- screen$response_type[[1]]
          mode <- screen$output_mode[[1]]
          response <- tryCatch(
            chat_model_response(
              chat,
              prompt,
              type,
              mode,
              infer_response_type_fields(type)
            ),
            error = function(e) e
          )
          if (inherits(response, "error")) {
            if (on_error == "stop") {
              stop(
                "Survey turn `",
                pid,
                "/",
                screen_id,
                "` failed: ",
                conditionMessage(response),
                call. = FALSE
              )
            }
            ei <- ei + 1L
            errors[[ei]] <- tibble::tibble(
              participant_id = pid,
              screen_id = screen_id,
              model_id = m$model_id[[mi]],
              completion = co,
              message = conditionMessage(response)
            )
            ti <- ti + 1L
            tasks[[ti]] <- survey_task_row(
              m,
              mi,
              pid,
              co,
              screen_id,
              task_hash,
              "error",
              path
            )
            next
          }
          answers[[screen_id]] <- response
          history[[screen_id]] <- list(prompt = prompt, response = response)
          one <- cbind(
            tibble::as_tibble(participant),
            tibble::tibble(
              screen_id = screen_id,
              screen_index = si,
              block = block,
              prompt = prompt,
              response = list(response),
              model_id = m$model_id[[mi]],
              model = m$model[[mi]],
              family = m$family[[mi]],
              completion = co,
              memory = memory,
              output_mode = mode,
              task_hash = task_hash,
              checkpoint_path = path
            )
          )
          ri <- ri + 1L
          rows[[ri]] <- one
          ti <- ti + 1L
          tasks[[ti]] <- survey_task_row(
            m,
            mi,
            pid,
            co,
            screen_id,
            task_hash,
            "completed",
            path
          )
          if (!is.na(path)) {
            write_prompt_grid_checkpoint(
              path,
              list(task_hash = task_hash, row = one)
            )
          }
        }
      }
    }
  }
  results <- dplyr::bind_rows(rows)
  list(
    results = results,
    wide = survey_responses_wide(results, participant_id),
    plan = plan,
    tasks = dplyr::bind_rows(tasks),
    errors = dplyr::bind_rows(errors)
  )
}

#' Convert long survey turns to one row per simulated respondent
#' @param results Turn-level results returned by [run_survey_simulation()].
#' @param participant_id Participant identifier column.
#' @export
survey_responses_wide <- function(results, participant_id = "participant_id") {
  if (
    !inherits(results, "data.frame") ||
      !all(c(participant_id, "screen_id", "response") %in% names(results))
  ) {
    stop("`results` must be survey turn-level results.")
  }
  key <- c(participant_id, "model_id", "completion")
  base <- results[,
    intersect(
      c(
        key,
        setdiff(
          names(results),
          c("screen_id", "response", "prompt", "task_hash", "checkpoint_path")
        )
      ),
      names(results)
    ),
    drop = FALSE
  ]
  base <- base[!duplicated(base[key]), , drop = FALSE]
  for (i in seq_len(nrow(results))) {
    r <- results$response[[i]]
    if (!is.list(r)) {
      r <- list(value = r)
    }
    for (nm in names(r)) {
      col <- paste0(results$screen_id[[i]], "__", nm)
      if (!col %in% names(base)) {
        base[[col]] <- NA
      }
      at <- match(
        interaction_key(results[i, , drop = FALSE], key),
        interaction_key(base, key)
      )
      base[[col]][at] <- list(r[[nm]])
    }
  }
  tibble::as_tibble(base)
}

survey_participants <- function(x, id, smoke_n) {
  if (
    !inherits(x, "data.frame") ||
      !id %in% names(x) ||
      anyNA(x[[id]]) ||
      anyDuplicated(x[[id]])
  ) {
    stop(
      "`participants` must contain unique, non-missing `participant_id` values."
    )
  }
  tibble::as_tibble(x)[seq_len(workflow_n_rows(x, smoke_n)), , drop = FALSE]
}
survey_flow_config <- function(x) {
  if (
    !inherits(x, "data.frame") || !all(c("screen_id", "prompt") %in% names(x))
  ) {
    stop("`survey_flow` must have `screen_id` and `prompt` columns.")
  }
  x <- tibble::as_tibble(x)
  if (
    anyDuplicated(x$screen_id) || anyNA(x$screen_id) || any(!nzchar(x$prompt))
  ) {
    stop("Survey screen IDs and prompts must be unique/non-empty.")
  }
  if (!"block" %in% names(x)) {
    x$block <- x$screen_id
  }
  if (!"response_type" %in% names(x)) {
    stop("`survey_flow$response_type` must be a list-column.")
  }
  if (!is.list(x$response_type)) {
    stop("`survey_flow$response_type` must be a list-column.")
  }
  if (!"output_mode" %in% names(x)) {
    x$output_mode <- "structured"
  }
  x$output_mode <- vapply(x$output_mode, normalize_output_mode, character(1))
  if (!"display_if" %in% names(x)) {
    x$display_if <- rep(list(TRUE), nrow(x))
  }
  x
}
survey_screen_visible <- function(rule, participant, answers) {
  if (is.function(rule)) {
    return(isTRUE(rule(participant, answers)))
  }
  isTRUE(rule)
}
survey_history_text <- function(history) {
  paste(
    vapply(
      names(history),
      function(id) {
        paste0(
          "[",
          id,
          "] ",
          history[[id]]$prompt,
          "\nAnswer: ",
          jsonlite::toJSON(history[[id]]$response, auto_unbox = TRUE)
        )
      },
      character(1)
    ),
    collapse = "\n\n"
  )
}
survey_new_chat <- function(
  model,
  completion,
  integration,
  virtual_key,
  base_url
) {
  route <- workflow_route_settings(
    model$integration,
    model$virtual_key,
    integration,
    virtual_key
  )
  name <- model$model[[1]]
  if (
    !startsWith(name, "@") &&
      !is.null(route$integration) &&
      nzchar(route$integration)
  ) {
    name <- paste0("@", route$integration, "/", name)
  }
  new_portkey_chat(
    name,
    workflow_setting(model$base_url, base_url),
    model$temperature[[1]],
    model$seed[[1]] + completion - 1L
  )
}
survey_task_row <- function(m, i, pid, co, sid, hash, status, path) {
  tibble::tibble(
    participant_id = pid,
    screen_id = sid,
    model_id = m$model_id[[i]],
    completion = co,
    task_hash = hash,
    status = status,
    path = path
  )
}
