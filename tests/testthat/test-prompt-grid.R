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
    "completion", "temperature", "output_mode", "seed"
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
