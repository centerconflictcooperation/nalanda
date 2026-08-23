test_that("make_annotation_prompt builds categorical prompts", {
  out <- make_annotation_prompt(
    question = "Is the sentiment of this text positive, neutral, or negative?",
    labels = c("positive", "neutral", "negative")
  )

  expect_match(out, "Is the sentiment of this text positive, neutral, or negative\\?")
  expect_match(out, "1 if positive, 2 if neutral, 3 if negative", fixed = TRUE)
  expect_match(out, "Here is the text:")
  expect_match(out, "\\{text\\}")
})

test_that("make_annotation_prompt builds Likert prompts", {
  out <- make_annotation_prompt(
    question = "How negative or positive is this headline on a 1 to 7 scale?",
    scale = c(1, 7),
    anchors = c("very negative", "very positive"),
    text_label = "Here is the headline:"
  )

  expect_match(out, "How negative or positive is this headline on a 1 to 7 scale\\?")
  expect_match(out, "1 being \"very negative\" and 7 being \"very positive\"", fixed = TRUE)
  expect_match(out, "Here is the headline:")
})

test_that("interpolate_prompt_template expands row placeholders", {
  out <- nalanda:::interpolate_prompt_template(
    "Is this {language} text positive?\n{text}",
    list(language = "Arabic", text = "example text")
  )

  expect_equal(out, "Is this Arabic text positive?\nexample text")
})

test_that("run_text_analysis validates required columns before API use", {
  expect_error(
    run_text_analysis(
      data = tibble::tibble(body = "hello"),
      text_col = "text",
      prompt = "Test {text}",
      response_type = structure(list(), class = "mock_type")
    ),
    "`text_col` was not found in `data`."
  )
})

test_that("run_text_analysis supports ellmer 0.4 structured prompt batches", {
  prompts_seen <- NULL

  testthat::local_mocked_bindings(
    new_portkey_chat = function(...) list(),
    .package = "nalanda"
  )
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(chat, prompts, type, ...) {
      if (!is.list(prompts)) {
        stop("`prompts` must be a list or prompt.")
      }
      prompts_seen <<- prompts
      lapply(seq_along(prompts), function(i) list(score = i))
    },
    .package = "ellmer"
  )

  out <- run_text_analysis(
    data = tibble::tibble(text = c("first", "second")),
    prompt = "Analyze {text}",
    response_type = structure(list(), class = "mock_type")
  )

  expect_type(prompts_seen, "list")
  expect_equal(prompts_seen, list("Analyze first", "Analyze second"))
  expect_identical(class(out), c("nalanda", "tbl_df", "tbl", "data.frame"))
  expect_no_error(mutated <- dplyr::mutate(out, doubled = score * 2))
  expect_equal(mutated$doubled, c(2, 4))
})

test_that("evaluate_text_analysis computes categorical metrics", {
  x <- tibble::tibble(
    language = c("English", "English", "English", "Arabic"),
    truth = c(1, 2, 3, 1),
    estimate = c(1, 2, 2, 1)
  )

  out <- evaluate_text_analysis(
    x,
    truth_col = "truth",
    estimate_col = "estimate",
    by = "language",
    metric = c("accuracy", "macro_precision", "macro_recall", "macro_f1")
  )

  expect_equal(nrow(out), 2)
  expect_true(all(c("accuracy", "macro_precision", "macro_recall", "macro_f1") %in% names(out)))
  expect_equal(out$accuracy[out$language == "Arabic"], 1)
  expect_equal(out$accuracy[out$language == "English"], 2 / 3)
})

test_that("evaluate_text_analysis computes spearman and weighted kappa", {
  x <- tibble::tibble(
    truth = c(1, 2, 3, 4, 5),
    estimate = c(1, 2, 2, 4, 5)
  )

  out <- evaluate_text_analysis(
    x,
    truth_col = "truth",
    estimate_col = "estimate",
    metric = c("spearman", "weighted_kappa")
  )

  expect_equal(nrow(out), 1)
  expect_true(is.finite(out$spearman))
  expect_true(is.finite(out$weighted_kappa))
  expect_gt(out$weighted_kappa, 0.7)
})
