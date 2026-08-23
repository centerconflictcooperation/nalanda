test_that("prompt-grid planning validates config and reports smoke call budgets", {
  data <- tibble::tibble(condition_id = 1:16, text = paste("text", 1:16))
  models <- tibble::tibble(
    model_id = c("small", "large", "disabled"),
    model = c("model-small", "model-large", "model-off"),
    family = c("vendor-a", "vendor-b", "vendor-c"),
    n_completions = c(2L, 3L, 99L),
    active = c(TRUE, TRUE, FALSE)
  )
  prompts <- c(direct = "Forecast {text}", deliberative = "Consider, then forecast {text}")

  plan <- plan_prompt_grid(data, prompts, models, smoke_n = 1)

  expect_equal(nrow(plan), 4)
  expect_equal(plan$n_rows, rep(1L, 4))
  expect_equal(sum(plan$estimated_calls), 10)
  expect_equal(attr(plan, "total_estimated_calls"), 10)
  expect_false("disabled" %in% plan$model_id)

  expect_error(
    plan_prompt_grid(data, prompts, transform(models, model_id = "same")),
    "must be unique"
  )
  expect_error(
    plan_prompt_grid(
      data,
      prompts,
      transform(models, n_completions = c(0L, 1L, 1L))
    ),
    "positive integers"
  )
})

test_that("prompt grid runs model-specific completions and resumes checkpoints", {
  calls <- 0L
  testthat::local_mocked_bindings(
    run_structured_responses = function(data, prompt, seed, model, ...) {
      calls <<- calls + 1L
      out <- tibble::as_tibble(data)
      out$completion <- 1L
      out$prompt <- prompt
      out$forecast <- seed
      out
    },
    .package = "nalanda"
  )

  data <- tibble::tibble(
    condition_id = c("control", "treatment"),
    text = c("short control", "short treatment")
  )
  models <- tibble::tibble(
    model_id = c("a", "b"),
    model = c("model-a", "model-b"),
    family = c("family-a", "family-b"),
    temperature = c(0, 0.5),
    seed = c(10L, 20L),
    n_completions = c(1L, 2L)
  )
  prompts <- c(p1 = "Forecast {text}", p2 = "Estimate {text}")
  output_dir <- file.path(tempdir(), paste0("nalanda-text-workflow-", Sys.getpid()))
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  first <- run_prompt_grid(
    data = data,
    id_col = "condition_id",
    prompt_variants = prompts,
    response_type = structure(list(), class = "mock_type"),
    model_config = models,
    smoke_n = 1,
    output_dir = output_dir,
    progress = FALSE,
    integration = NULL,
    virtual_key = NULL,
    base_url = "https://example.invalid"
  )

  expect_equal(calls, 6L)
  expect_equal(nrow(first$results), 6)
  expect_equal(sort(unique(first$results$completion[first$results$model_id == "b"])), 1:2)
  expect_equal(unique(first$results$condition_id), "control")
  expect_true(all(c(
    "model_id", "model", "family", "prompt_id", "prompt_template",
    "completion", "temperature", "output_mode", "seed", "task_hash",
    "input_row"
  ) %in% names(first$results)))
  expect_equal(length(list.files(output_dir, pattern = "\\.rds$")), 6)
  expect_true(all(first$tasks$status == "completed"))

  second <- run_prompt_grid(
    data = data,
    id_col = "condition_id",
    prompt_variants = prompts,
    response_type = structure(list(), class = "mock_type"),
    model_config = models,
    smoke_n = 1,
    output_dir = output_dir,
    progress = FALSE,
    integration = NULL,
    virtual_key = NULL,
    base_url = "https://example.invalid"
  )

  expect_equal(calls, 6L)
  expect_true(all(second$tasks$status == "resumed"))
  expect_equal(second$results, first$results)
})

test_that("prior results make phased plans and runs pending-call aware", {
  calls <- character()
  testthat::local_mocked_bindings(
    run_structured_responses = function(data, model, ...) {
      calls <<- c(calls, model)
      tibble::tibble(text = data$text, completion = 1L, prompt = "preview", forecast = 1)
    },
    .package = "nalanda"
  )

  data <- tibble::tibble(condition_id = c("a", "b"), text = c("A", "B"))
  schema <- structure(list(fields = "forecast"), class = "mock_type")
  cheap <- tibble::tibble(
    model_id = "cheap", model = "cheap-model", family = "A",
    n_completions = 1L, phase = "first"
  )
  later <- tibble::tibble(
    model_id = "later", model = "later-model", family = "B",
    n_completions = 2L, phase = "later"
  )

  phase_one <- run_prompt_grid(
    data = data,
    id_col = "condition_id",
    prompt_variants = c(main = "Forecast {text}"),
    response_type = schema,
    model_config = cheap,
    progress = FALSE,
    integration = NULL,
    virtual_key = NULL,
    base_url = "https://example.invalid"
  )
  expect_equal(calls, "cheap-model")

  phased_config <- dplyr::bind_rows(cheap, later)
  plan <- run_prompt_grid(
    data = data,
    id_col = "condition_id",
    prompt_variants = c(main = "Forecast {text}"),
    response_type = schema,
    model_config = phased_config,
    existing_results = phase_one,
    dry_run = TRUE,
    progress = FALSE,
    integration = NULL,
    virtual_key = NULL,
    base_url = "https://example.invalid"
  )

  expect_equal(plan$configured_calls, c(2, 4))
  expect_equal(plan$reused_calls, c(2, 0))
  expect_equal(plan$pending_calls, c(0, 4))
  expect_equal(attr(plan, "total_pending_calls"), 4)

  phase_two <- run_prompt_grid(
    data = data,
    id_col = "condition_id",
    prompt_variants = c(main = "Forecast {text}"),
    response_type = schema,
    model_config = later,
    existing_results = phase_one$results,
    progress = FALSE,
    integration = NULL,
    virtual_key = NULL,
    base_url = "https://example.invalid"
  )

  expect_equal(calls, c("cheap-model", "later-model", "later-model"))
  expect_setequal(unique(phase_two$results$model_id), c("cheap", "later"))
  expect_true(all(phase_two$tasks$phase == "later"))
  expect_true(all(phase_two$tasks$status == "completed"))
})

test_that("strong task hashes do not reuse changed prompts or response specs", {
  calls <- 0L
  testthat::local_mocked_bindings(
    run_structured_responses = function(data, ...) {
      calls <<- calls + 1L
      tibble::tibble(text = data$text, completion = 1L, prompt = "preview", forecast = 1)
    },
    .package = "nalanda"
  )
  args <- list(
    data = tibble::tibble(id = 1:2, text = c("A", "B")),
    id_col = "id",
    response_type = structure(list(fields = "forecast"), class = "mock_type"),
    model_config = c(m = "model"),
    progress = FALSE,
    integration = NULL,
    virtual_key = NULL,
    base_url = "https://example.invalid"
  )
  first <- do.call(
    run_prompt_grid,
    c(args, list(prompt_variants = c(p = "Forecast {text}")))
  )

  changed_prompt <- do.call(
    run_prompt_grid,
    c(args, list(
      prompt_variants = c(p = "Revised forecast {text}"),
      existing_results = first,
      dry_run = TRUE
    ))
  )
  changed_schema_args <- args
  changed_schema_args$response_type <- structure(
    list(fields = c("forecast", "confidence")), class = "mock_type"
  )
  changed_schema <- do.call(
    run_prompt_grid,
    c(changed_schema_args, list(
      prompt_variants = c(p = "Forecast {text}"),
      existing_results = first,
      dry_run = TRUE
    ))
  )

  expect_equal(calls, 1L)
  expect_equal(changed_prompt$pending_calls, 2)
  expect_equal(changed_schema$pending_calls, 2)
})

test_that("legacy reuse requires explicit trust and exact inputs and prompts", {
  calls <- 0L
  testthat::local_mocked_bindings(
    run_structured_responses = function(...) {
      calls <<- calls + 1L
      stop("should not be called")
    },
    .package = "nalanda"
  )
  data <- tibble::tibble(id = 1:2, text = c("A", "B"))
  legacy <- tibble::tibble(
    id = 1:2,
    text = c("A", "B"),
    prompt = c("Forecast A", "Forecast B"),
    model_id = "m",
    prompt_id = "p",
    completion = 1L,
    forecast = c(1, 2)
  )
  args <- list(
    data = data,
    id_col = "id",
    prompt_variants = c(p = "Forecast {text}"),
    response_type = structure(list(fields = "forecast"), class = "mock_type"),
    model_config = c(m = "model"),
    existing_results = legacy,
    progress = FALSE,
    integration = NULL,
    virtual_key = NULL,
    base_url = "https://example.invalid"
  )

  expect_error(do.call(run_prompt_grid, args), "trust_legacy_results")
  migrated <- do.call(
    run_prompt_grid,
    c(args, list(trust_legacy_results = TRUE))
  )
  expect_equal(calls, 0L)
  expect_true(all(nzchar(migrated$results$task_hash)))
  expect_equal(migrated$results$input_row, 1:2)
  expect_equal(migrated$tasks$status, "reused")

  changed_args <- args
  changed_args$prompt_variants <- c(p = "Changed {text}")
  changed_args$trust_legacy_results <- TRUE
  changed_args$dry_run <- TRUE
  changed <- do.call(run_prompt_grid, changed_args)
  expect_equal(changed$pending_calls, 2)
})

test_that("checkpoint collection includes models outside the active phase", {
  testthat::local_mocked_bindings(
    run_structured_responses = function(data, model, ...) {
      tibble::tibble(
        id = data$id, text = data$text, completion = 1L,
        prompt = "preview", forecast = if (model == "model-a") 1 else 2
      )
    },
    .package = "nalanda"
  )
  checkpoint_dir <- file.path(tempdir(), paste0("nalanda-collect-", Sys.getpid()))
  dir.create(checkpoint_dir)
  on.exit(unlink(checkpoint_dir, recursive = TRUE), add = TRUE)
  common <- list(
    data = tibble::tibble(id = 1L, text = "A"),
    id_col = "id",
    prompt_variants = c(p = "Forecast {text}"),
    response_type = structure(list(fields = "forecast"), class = "mock_type"),
    output_dir = checkpoint_dir,
    progress = FALSE,
    integration = NULL,
    virtual_key = NULL,
    base_url = "https://example.invalid"
  )
  do.call(run_prompt_grid, c(common, list(model_config = c(a = "model-a"))))
  do.call(run_prompt_grid, c(common, list(model_config = c(b = "model-b"))))

  collected <- collect_prompt_grid_results(checkpoint_dir)

  expect_setequal(unique(collected$model_id), c("a", "b"))
  expect_equal(nrow(collected), 2L)
})

test_that("prompt grid can continue past a failed completion", {
  testthat::local_mocked_bindings(
    run_structured_responses = function(data, model, ...) {
      if (model == "bad-model") stop("mock failure")
      tibble::tibble(text = data$text, completion = 1L, prompt = "preview", forecast = 1)
    },
    .package = "nalanda"
  )

  out <- run_prompt_grid(
    data = tibble::tibble(text = "example"),
    prompt_variants = c(main = "Forecast {text}"),
    response_type = structure(list(), class = "mock_type"),
    model_config = c(good = "good-model", bad = "bad-model"),
    on_error = "continue",
    progress = FALSE,
    integration = NULL,
    virtual_key = NULL,
    base_url = "https://example.invalid"
  )

  expect_equal(nrow(out$results), 1)
  expect_equal(out$results$model_id, "good")
  expect_equal(nrow(out$errors), 1)
  expect_equal(out$errors$model_id, "bad")
  expect_match(out$errors$message, "mock failure")
})

test_that("forecast aggregation makes hierarchical equal weighting explicit", {
  raw <- tibble::tibble(
    condition_id = 1,
    family = c("A", "A", "A", "A", "B"),
    model_id = c("a1", "a1", "a1", "a2", "b1"),
    prompt_id = c("p1", "p1", "p2", "p1", "p1"),
    completion = c(1L, 2L, 1L, 1L, 1L),
    effect_1 = c(0, 2, 3, 6, 10),
    effect_2 = c(NA, NA, 4, 8, 12)
  )

  out <- aggregate_model_forecasts(
    raw,
    outcomes = c("effect_1", "effect_2"),
    unit_by = "condition_id"
  )

  expect_equal(out$prompt$effect_1, c(1, 3, 6, 10))
  expect_equal(out$model$effect_1, c(2, 6, 10))
  expect_equal(out$family$effect_1, c(4, 10))
  expect_equal(out$consensus$effect_1, 7)
  expect_equal(out$prompt$effect_2[[1]], NA_real_)
  expect_equal(out$prompt$n_completions, c(2L, 1L, 1L, 1L))
  expect_equal(out$consensus$n_families, 2L)
  expect_false(isTRUE(all.equal(out$consensus$effect_1, mean(raw$effect_1))))
})

test_that("forecast aggregation rejects ambiguous duplicate units", {
  raw <- tibble::tibble(
    condition_id = c(1, 1),
    family = "A",
    model_id = "a1",
    prompt_id = "p1",
    completion = 1L,
    effect = c(1, 2)
  )

  expect_error(
    aggregate_model_forecasts(raw, "effect", "condition_id"),
    "not unique"
  )
})

test_that("median aggregation is robust while preserving equal-weight stages", {
  raw <- tibble::tibble(
    condition_id = 1,
    family = c("A", "A", "A", "A", "A", "B"),
    model_id = c("a1", "a1", "a1", "a1", "a2", "b1"),
    prompt_id = c("p1", "p1", "p1", "p2", "p1", "p1"),
    completion = c(1L, 2L, 3L, 1L, 1L, 1L),
    effect = c(0, 0, 1000, 0, 2, 10)
  )

  mean_out <- aggregate_model_forecasts(
    raw, "effect", "condition_id", method = "mean"
  )
  default_out <- aggregate_model_forecasts(raw, "effect", "condition_id")
  median_out <- aggregate_model_forecasts(
    raw, "effect", "condition_id", method = "median"
  )

  expect_equal(default_out, mean_out)
  expect_equal(median_out$prompt$effect, c(0, 0, 2, 10))
  expect_equal(median_out$model$effect, c(0, 2, 10))
  expect_equal(median_out$family$effect, c(1, 10))
  expect_equal(median_out$consensus$effect, 5.5)
  expect_gt(mean_out$consensus$effect, 40)
  expect_equal(median_out$prompt$n_completions, c(3L, 1L, 1L, 1L))
  expect_equal(median_out$family$n_models, c(2L, 1L))
})

test_that("aggregation omits missing outcomes and retains structural counts", {
  raw <- tibble::tibble(
    condition_id = 1,
    family = "A",
    model_id = "a",
    prompt_id = "p",
    completion = 1:3,
    effect = c(NA, 2, 4),
    all_missing = c(NA_real_, NA_real_, NA_real_)
  )

  out <- aggregate_model_forecasts(
    raw,
    c("effect", "all_missing"),
    "condition_id",
    method = "median"
  )

  expect_equal(out$prompt$effect, 3)
  expect_true(is.na(out$consensus$all_missing))
  expect_equal(out$prompt$n_completions, 3L)
})
