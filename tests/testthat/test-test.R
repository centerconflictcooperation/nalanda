test_that("make_baseline_prompt expands groups with ingroup first", {
  out <- make_baseline_prompt(
    identity_context = "You are simulating an American Democrat.",
    question_template = "How warmly do you feel towards {group}s?",
    groups = c("Democrat", "Republican"),
    identity_label = "Democrat"
  )

  expect_match(out, "How warmly do you feel towards Democrats\\?")
  expect_match(out, "Democrats\\?.*Republicans\\?", perl = TRUE)
})

test_that("make_post_prompt keeps chapter text and question", {
  out <- make_post_prompt(
    chapter_text = "Chapter content here.",
    question_template = "How warmly do you feel towards your outgroup?",
    groups = c("Democrat", "Republican"),
    identity_label = "Democrat"
  )

  expect_match(out, "Chapter content here\\.")
  expect_match(out, "How warmly do you feel towards your outgroup\\?")
  expect_match(out, "You have just read the material below\\.")
  expect_false(grepl("book chapter", out, fixed = TRUE))
})

test_that("make_post_prompt_preview stores a cropped chapter preview", {
  chapter_text <- paste0(strrep("A", 80), strrep("B", 80))

  out <- nalanda:::make_post_prompt_preview(
    chapter_text = chapter_text,
    question_template = "How warmly do you feel towards your outgroup?",
    groups = c("Democrat", "Republican"),
    identity_label = "Democrat",
    excerpt_chars = 40
  )

  expect_match(out, strrep("A", 20), fixed = TRUE)
  expect_match(out, "\\[\\.\\.\\. chapter text cropped for storage \\.\\.\\.\\]")
  expect_match(out, strrep("B", 20), fixed = TRUE)
  expect_match(out, "How warmly do you feel towards your outgroup\\?")
  expect_false(grepl("book chapter", out, fixed = TRUE))
})

test_that("make_post_prompt_preview repairs invalid multibyte chapter text", {
  chapter_text <- rawToChar(as.raw(c(
    charToRaw(strrep("A", 80)),
    0xe9,
    charToRaw(strrep("B", 80))
  )))
  Encoding(chapter_text) <- "UTF-8"

  expect_error(
    nchar(chapter_text, type = "chars", allowNA = FALSE, keepNA = FALSE),
    "invalid"
  )

  out <- nalanda:::make_post_prompt_preview(
    chapter_text = chapter_text,
    question_template = "How warmly do you feel towards your outgroup?",
    groups = c("Democrat", "Republican"),
    identity_label = "Democrat",
    excerpt_chars = 40
  )

  expect_match(out, strrep("A", 20), fixed = TRUE)
  expect_match(out, "\\[\\.\\.\\. chapter text cropped for storage \\.\\.\\.\\]")
  expect_match(out, strrep("B", 20), fixed = TRUE)
  expect_match(out, "How warmly do you feel towards your outgroup\\?")
})

test_that("make_one_turn_prompt uses generic material wording", {
  out <- nalanda:::make_one_turn_prompt(
    chapter_text = "Prompt body here.",
    identity_context = "You are simulating a Democrat.",
    question_template = "How warmly do you feel towards your outgroup?",
    groups = c("Democrat", "Republican"),
    identity_label = "Democrat"
  )

  expect_match(out, "You have just been shown the material below\\.")
  expect_match(out, "Prompt body here\\.")
  expect_false(grepl("book chapter", out, fixed = TRUE))
})

test_that("summarize_chapter_scores tolerates missing chapter excerpt", {
  x <- tibble::tibble(
    book = c("Book A", "Book A"),
    chapter = c("chapter_1", "chapter_1"),
    pre_ingroup = c(60, 62),
    post_ingroup = c(64, 66),
    pre_outgroup = c(40, 42),
    post_outgroup = c(48, 50),
    pre_gap = c(20, 20),
    post_gap = c(16, 16),
    delta_outgroup = c(8, 8),
    delta_ingroup = c(4, 4),
    delta_gap = c(4, 4)
  )

  out <- summarize_chapter_scores(x)

  expect_false("chapter_excerpt" %in% names(out))
})

test_that("summarize_chapter_scores computes chapter summaries and keeps attrs", {
  x <- tibble::tibble(
    book = c("Book A", "Book A", "Book A", "Book A"),
    chapter = c("chapter_1", "chapter_1", "chapter_2", "chapter_2"),
    pre_ingroup = c(60, 62, 58, 60),
    post_ingroup = c(64, 66, 60, 62),
    pre_outgroup = c(40, 42, 45, 47),
    post_outgroup = c(48, 50, 49, 51),
    pre_gap = c(20, 20, 13, 13),
    post_gap = c(16, 16, 11, 11),
    delta_outgroup = c(8, 8, 4, 4),
    delta_ingroup = c(4, 4, 2, 2),
    delta_gap = c(4, 4, 2, 2)
  )
  attr(x, "model") <- "test-model"
  attr(x, "temperature") <- 0
  attr(x, "n_simulations") <- 2
  attr(x, "chapter_excerpts") <- tibble::tibble(
    book = "Book A",
    chapter = c("chapter_1", "chapter_2"),
    chapter_excerpt = c("x", "y")
  )

  out <- summarize_chapter_scores(x)

  expect_equal(nrow(out), 2)
  expect_true(all(
    c("mean_delta_gap", "sd_delta_gap", "chapter_index", "chapter_excerpt") %in% names(out)
  ))
  expect_equal(out$chapter_excerpt, c("x", "y"))
  expect_equal(attr(out, "model"), "test-model")
  expect_equal(attr(out, "temperature"), 0)
  expect_equal(attr(out, "n_simulations"), 2)
})

test_that("summarize_chapter_scores handles recovered full-book excerpt attrs", {
  x <- tibble::tibble(
    book = c("Book A", "Book A"),
    chapter = c("full_book", "full_book"),
    pre_ingroup = c(60, 62),
    post_ingroup = c(64, 66),
    pre_outgroup = c(40, 42),
    post_outgroup = c(48, 50),
    pre_gap = c(20, 20),
    post_gap = c(16, 16),
    delta_outgroup = c(8, 8),
    delta_ingroup = c(4, 4),
    delta_gap = c(4, 4)
  )
  attr(x, "chapter_excerpts") <- tibble::tibble(
    chapter = "full_book",
    chapter_excerpt = "whole book preview"
  )

  out <- summarize_chapter_scores(x)

  expect_equal(nrow(out), 1)
  expect_equal(out$book, "Book A")
  expect_equal(out$chapter, "full_book")
  expect_equal(out$chapter_excerpt, "whole book preview")
})

test_that("summarize_chapter_scores keeps recovered full books distinct", {
  make_book <- function(book, preview) {
    x <- tibble::tibble(
      book = c(book, book),
      chapter = c("full_book", "full_book"),
      pre_ingroup = c(60, 62),
      post_ingroup = c(64, 66),
      pre_outgroup = c(40, 42),
      post_outgroup = c(48, 50),
      pre_gap = c(20, 20),
      post_gap = c(16, 16),
      delta_outgroup = c(8, 8),
      delta_ingroup = c(4, 4),
      delta_gap = c(4, 4)
    )
    attr(x, "chapter_excerpts") <- tibble::tibble(
      chapter = "full_book",
      chapter_excerpt = preview
    )
    x
  }

  out <- summarize_chapter_scores(list(
    make_book("Book A", "preview a"),
    make_book("Book B", "preview b")
  ))

  expect_equal(nrow(out), 2)
  expect_equal(out$book, c("Book A", "Book B"))
  expect_equal(out$chapter_excerpt, c("preview a", "preview b"))
})

test_that("summarize_chapter_scores supports book-level aggregation", {
  x <- tibble::tibble(
    book = c("Book A", "Book A"),
    chapter = c("chapter_1", "chapter_2"),
    pre_ingroup = c(60, 58),
    post_ingroup = c(64, 60),
    pre_outgroup = c(40, 45),
    post_outgroup = c(48, 49),
    pre_gap = c(20, 13),
    post_gap = c(16, 11),
    delta_outgroup = c(8, 4),
    delta_ingroup = c(4, 2),
    delta_gap = c(4, 2),
    chapter_excerpt = c("x", "y")
  )

  out <- summarize_chapter_scores(x, aggregate_level = "book")

  expect_equal(nrow(out), 1)
  expect_true(all(c("book", "mean_delta_gap", "sim") %in% names(out)))
  expect_false("chapter_excerpt" %in% names(out))
})

test_that("summarize_chapter_scores can use last cumulative chapter for book aggregation", {
  x <- tibble::tibble(
    model = rep("test-model", 6),
    book = rep("Book A", 6),
    chapter = rep(c("chapter_1", "chapter_2", "chapter_3"), 2),
    chapter_index = rep(1:3, 2),
    sim = c(1, 1, 1, 2, 2, 2),
    identity = rep("Democrat", 6),
    party = rep("Democrat", 6),
    pre_ingroup = c(60, 60, 60, 62, 62, 62),
    post_ingroup = c(61, 62, 63, 63, 64, 65),
    pre_outgroup = c(40, 40, 40, 42, 42, 42),
    post_outgroup = c(42, 44, 46, 44, 46, 48),
    pre_gap = c(20, 20, 20, 20, 20, 20),
    post_gap = c(19, 18, 17, 19, 18, 17),
    delta_outgroup = c(2, 4, 6, 2, 4, 6),
    delta_ingroup = c(1, 2, 3, 1, 2, 3),
    delta_gap = c(1, 2, 3, 1, NA_real_, 3)
  )
  attr(x, "model") <- "test-model"
  attr(x, "temperature") <- 0.25
  attr(x, "n_simulations") <- 2

  out <- summarize_chapter_scores(
    x,
    aggregate_level = "book",
    book_chapter_strategy = "last",
    by_party = TRUE
  )

  expect_equal(nrow(out), 1)
  expect_equal(out$sim, 2)
  expect_equal(out$mean_delta_gap, 3)
  expect_equal(out$mean_delta_outgroup, 6)
  expect_equal(attr(out, "model"), "test-model")
  expect_equal(attr(out, "temperature"), 0.25)
  expect_equal(attr(out, "n_simulations"), 2)
})

test_that("summarize_chapter_scores infers last cumulative chapter order from chapter labels", {
  x <- tibble::tibble(
    book = rep("Book A", 4),
    chapter = c("chapter_1", "chapter_10", "chapter_1", "chapter_10"),
    sim = c(1, 1, 2, 2),
    identity = rep("Democrat", 4),
    pre_ingroup = c(60, 60, 62, 62),
    post_ingroup = c(61, 63, 63, 65),
    pre_outgroup = c(40, 40, 42, 42),
    post_outgroup = c(42, 46, 44, 48),
    pre_gap = c(20, 20, 20, 20),
    post_gap = c(19, 17, 19, 17),
    delta_outgroup = c(2, 6, 2, 6),
    delta_ingroup = c(1, 3, 1, 3),
    delta_gap = c(1, 3, 1, 3)
  )

  out <- summarize_chapter_scores(
    x,
    aggregate_level = "book",
    book_chapter_strategy = "last"
  )

  expect_equal(out$sim, 2)
  expect_equal(out$mean_delta_gap, 3)
})

test_that("summarize_chapter_scores can standardize metrics within model", {
  x <- tibble::tibble(
    model = rep(c("model-a", "model-b"), each = 3),
    book = rep(c("Book A", "Book B", "Book C"), 2),
    chapter = rep("full_book", 6),
    pre_ingroup = 0,
    post_ingroup = 0,
    pre_outgroup = 0,
    post_outgroup = 0,
    pre_gap = 0,
    post_gap = 0,
    delta_outgroup = c(1, 2, 3, 100, 200, 300),
    delta_ingroup = 0,
    delta_gap = c(1, 2, 3, 100, 200, 300)
  )

  out <- summarize_chapter_scores(
    x,
    aggregate_level = "book",
    standardize = "z"
  )

  expect_equal(out$mean_delta_gap[out$model == "model-a"], c(-1, 0, 1))
  expect_equal(out$mean_delta_gap[out$model == "model-b"], c(-1, 0, 1))
  expect_equal(attr(out, "score_scale"), "z")
})

test_that("summarize_chapter_scores can aggregate standardized model consensus", {
  x <- tibble::tibble(
    model = rep(c("model-a", "model-b"), each = 3),
    book = rep(c("Book A", "Book B", "Book C"), 2),
    chapter = rep("full_book", 6),
    pre_ingroup = 0,
    post_ingroup = 0,
    pre_outgroup = 0,
    post_outgroup = 0,
    pre_gap = 0,
    post_gap = 0,
    delta_outgroup = c(1, 2, 3, 300, 200, 100),
    delta_ingroup = 0,
    delta_gap = c(1, 2, 3, 300, 200, 100)
  )

  out <- summarize_chapter_scores(
    x,
    aggregate_level = "book",
    standardize = "z",
    model_aggregation = "mean"
  )

  expect_equal(nrow(out), 3)
  expect_false("model" %in% names(out))
  expect_equal(out$book, c("Book A", "Book B", "Book C"))
  expect_equal(out$n_models, c(2L, 2L, 2L))
  expect_equal(out$mean_delta_gap, c(0, 0, 0))
  expect_true("sd_model_mean_delta_gap" %in% names(out))
  expect_equal(attr(out, "model_aggregation"), "mean")
})

test_that("compute_run_ai_metrics preserves chapter excerpt index", {
  x <- tibble::tibble(
    chapter = c("chapter_1", "chapter_1"),
    sim = c(1, 1),
    identity = c("Democrat", "Democrat"),
    turn_type = c("baseline", "post"),
    target_group = c(NA_character_, NA_character_),
    rating = c(40, 45)
  )
  attr(x, "chapter_excerpts") <- tibble::tibble(
    chapter = "chapter_1",
    chapter_excerpt = "preview"
  )

  out <- compute_run_ai_metrics(x)

  expect_equal(
    attr(out, "chapter_excerpts"),
    attr(x, "chapter_excerpts")
  )
})

test_that("compute_run_ai_metrics binds list inputs before computing metrics", {
  x1 <- tibble::tibble(
    chapter = c("chapter_1", "chapter_1"),
    sim = c(1, 1),
    identity = c("Democrat", "Democrat"),
    turn_type = c("baseline", "post"),
    target_group = c(NA_character_, NA_character_),
    rating = c(40, 45)
  )
  x2 <- tibble::tibble(
    chapter = c("chapter_2", "chapter_2"),
    sim = c(1, 1),
    identity = c("Democrat", "Democrat"),
    turn_type = c("baseline", "post"),
    target_group = c(NA_character_, NA_character_),
    rating = c(50, 60)
  )
  attr(x1, "model") <- "test-model"
  attr(x1, "temperature") <- 0
  attr(x1, "n_simulations") <- 1
  attr(x1, "chapter_excerpts") <- tibble::tibble(
    chapter = "chapter_1",
    chapter_excerpt = "preview 1"
  )
  attr(x2, "chapter_excerpts") <- tibble::tibble(
    chapter = "chapter_2",
    chapter_excerpt = "preview 2"
  )

  out <- compute_run_ai_metrics(list(x1, x2))

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
  expect_equal(out$delta_outgroup, c(5, 10))
  expect_equal(attr(out, "model"), "test-model")
  expect_equal(attr(out, "temperature"), 0)
  expect_equal(attr(out, "n_simulations"), 1)
  expect_equal(
    attr(out, "chapter_excerpts"),
    tibble::tibble(
      chapter = c("chapter_1", "chapter_2"),
      chapter_excerpt = c("preview 1", "preview 2")
    )
  )
})

test_that("compute_run_ai_metrics keeps books distinct for list inputs", {
  make_book_turns <- function(book) {
    tibble::tibble(
      book = book,
      chapter = c("chapter_1", "chapter_1"),
      sim = c(1, 1),
      identity = c("Democrat", "Democrat"),
      turn_type = c("baseline", "post"),
      target_group = c(NA_character_, NA_character_),
      rating = c(40, 45)
    )
  }

  x <- list(
    make_book_turns("Book A"),
    make_book_turns("Book B")
  )

  out <- compute_run_ai_metrics(x)

  expect_equal(length(unique(out$book)), 2)
  expect_setequal(unique(out$book), c("Book A", "Book B"))
  expect_equal(nrow(out), 2)
})

test_that("compute_run_ai_metrics orders by book before chapter", {
  make_turns <- function(book, chapter) {
    tibble::tibble(
      book = book,
      chapter = c(chapter, chapter),
      sim = c(1, 1),
      identity = c("Democrat", "Democrat"),
      turn_type = c("baseline", "post"),
      target_group = c(NA_character_, NA_character_),
      rating = c(40, 45)
    )
  }

  x <- dplyr::bind_rows(
    make_turns("Book B", "chapter_10"),
    make_turns("Book A", "chapter_2"),
    make_turns("Book A", "chapter_1")
  )
  x$book <- factor(x$book, levels = c("Book B", "Book A"))
  x$model <- c(NA_character_, NA_character_, "test-model", NA_character_, NA_character_, NA_character_)

  out <- compute_run_ai_metrics(x)

  expect_equal(unique(out$model), "test-model")
  expect_equal(as.character(out$book), c("Book A", "Book A", "Book B"))
  expect_equal(out$chapter, c("chapter_1", "chapter_2", "chapter_10"))
})

test_that("compute_run_ai_metrics preserves rows when optional ids contain NA", {
  make_book_turns <- function(book, prompt) {
    tibble::tibble(
      model = "test-model",
      book = book,
      party = "Democrat",
      baseline_prompt = prompt,
      post_prompt = NA_character_,
      chapter = c("chapter_1", "chapter_1"),
      sim = c(1, 1),
      identity = c("Democrat", "Democrat"),
      turn_type = c("baseline", "post"),
      target_group = c(NA_character_, NA_character_),
      rating = c(40, 45)
    )
  }

  x <- list(
    make_book_turns("Book A", NA_character_),
    make_book_turns("Book B", "prompt")
  )

  out <- compute_run_ai_metrics(x)

  expect_equal(length(unique(out$book)), 2)
  expect_setequal(unique(out$book), c("Book A", "Book B"))
  expect_equal(nrow(out), 2)
})

test_that("normalize_model_name strips integration prefixes", {
  expect_equal(
    nalanda:::normalize_model_name("@gemini-8c2498/gemini-2.5-flash-lite"),
    "gemini-2.5-flash-lite"
  )
  expect_equal(
    nalanda:::normalize_model_name("vertexai/gemini-2.5-flash-lite"),
    "gemini-2.5-flash-lite"
  )
  expect_equal(
    nalanda:::normalize_model_name("gemini-2.5-flash-lite"),
    "gemini-2.5-flash-lite"
  )
})

test_that("run_ai_on_chapters rejects gpt-5-mini temperature before model calls", {
  calls <- 0L
  testthat::local_mocked_bindings(
    new_portkey_chat = function(...) {
      calls <<- calls + 1L
      stop("model should not be called")
    },
    .package = "nalanda"
  )

  expect_error(
    run_ai_on_chapters(
      book_texts = list("Book A" = list("chapter_1.txt" = "Ordinary chapter text.")),
      groups = c("Democrat", "Republican"),
      context_text = "You are simulating a {identity}.",
      question_text = "How warmly do you feel towards {group}s?",
      n_simulations = 1,
      temperature = 0,
      model = "@gpt-5-mini/gpt-5-mini"
    ),
    "`temperature` must be 1 when using model `gpt-5-mini`",
    fixed = TRUE
  )
  expect_equal(calls, 0L)
})

test_that("run_ai_on_chapters fails fast for route/model mismatch with skip errors", {
  calls <- 0L
  testthat::local_mocked_bindings(
    new_portkey_chat = function(...) {
      list(
        chat_structured = function(prompt, type) {
          calls <<- calls + 1L
          stop(
            "HTTP 412 Precondition Failed.\n",
            "Model gpt-4-mini is not allowed for this integration"
          )
        }
      )
    },
    .package = "nalanda"
  )

  expect_error(
    run_ai_on_chapters(
      book_texts = list(
        "Book A" = list(
          "chapter_1.txt" = "Ordinary chapter text.",
          "chapter_2.txt" = "More ordinary chapter text."
        )
      ),
      groups = c("Democrat", "Republican"),
      context_text = "You are simulating a {identity}.",
      question_text = "How warmly do you feel towards {group}s?",
      n_simulations = 1,
      model = "gpt-4-mini",
      on_error = "skip"
    ),
    "Model route validation failed for `gpt-4-mini`",
    fixed = TRUE
  )
  expect_equal(calls, 1L)
})

test_that("summarize_chapter_scores normalizes fully-qualified model attrs", {
  x <- tibble::tibble(
    book = c("Book A", "Book A"),
    chapter = c("chapter_1", "chapter_1"),
    pre_ingroup = c(60, 62),
    post_ingroup = c(64, 66),
    pre_outgroup = c(40, 42),
    post_outgroup = c(48, 50),
    pre_gap = c(20, 20),
    post_gap = c(16, 16),
    delta_outgroup = c(8, 8),
    delta_ingroup = c(4, 4),
    delta_gap = c(4, 4)
  )
  attr(x, "model") <- "@gemini-8c2498/gemini-2.5-flash-lite"
  attr(x, "temperature") <- 0

  out <- summarize_chapter_scores(x)

  expect_equal(attr(out, "model"), "gemini-2.5-flash-lite")
})

test_that("run_ai_on_chapters fails early on ambiguous chapter numbering", {
  bad_book_texts <- list(
    moraltribes = list(
      "4_Chapter_4.txt" = "chapter a",
      "4_Chapter3.txt" = "chapter b"
    )
  )

  expect_error(
    run_ai_on_chapters(
      book_texts = bad_book_texts,
      groups = c("Democrat", "Republican"),
      context_text = "You are simulating a {identity}.",
      question_text = "How warmly do you feel towards your outgroup?"
    ),
    "Duplicate chapters identified while parsing"
  )
})

test_that("run_ai_on_chapters errors clearly on empty book folders", {
  empty_book_texts <- list(whynationsfails = list())

  expect_error(
    run_ai_on_chapters(
      book_texts = empty_book_texts,
      groups = c("Democrat", "Republican"),
      context_text = "You are simulating a {identity}.",
      question_text = "How warmly do you feel towards your outgroup?"
    ),
    "No chapters found in `book_texts` for book 'whynationsfails'"
  )
})

test_that("run_ai_on_chapters skips missing chapter text without model calls", {
  calls <- 0L
  testthat::local_mocked_bindings(
    new_portkey_chat = function(...) {
      calls <<- calls + 1L
      list(
        chat_structured = function(prompt, type) {
          if (grepl("finished reading", prompt, fixed = TRUE)) {
            return(list(rating = 45))
          }
          list(party = "Democrat", rating = 40)
        }
      )
    },
    .package = "nalanda"
  )

  book_texts <- list(
    "Book A" = list(
      "chapter_1.txt" = "Ordinary chapter text.",
      "chapter_2.txt" = NA_character_
    )
  )

  expect_message(
    out <- run_ai_on_chapters(
      book_texts = book_texts,
      groups = c("Democrat", "Republican"),
      context_text = "You are simulating a {identity}.",
      question_text = "How warmly do you feel towards your outgroup?"
    ),
    "Skipping 1 chapter\\(s\\) with missing `chapter_text`: Book A - chapter_2.txt"
  )

  out_tbl <- out[["Book A"]]
  skipped <- out_tbl[out_tbl$chapter == "chapter_2.txt", ]

  expect_equal(calls, 2L)
  expect_equal(nrow(out_tbl), 8)
  expect_true(all(is.na(skipped$rating)))
  expect_equal(unique(skipped$party), c("Democrat", "Republican"))
  expect_true(all(is.na(skipped$baseline_prompt)))
  expect_true(all(is.na(skipped$post_prompt)))

  metrics <- compute_run_ai_metrics(out)
  skipped_metrics <- metrics[metrics$chapter == "chapter_2.txt", ]

  expect_equal(skipped_metrics$party, c("Democrat", "Republican"))
  expect_true(all(is.na(skipped_metrics$pre_outgroup)))
  expect_true(all(is.na(skipped_metrics$post_outgroup)))
  expect_true(all(is.na(skipped_metrics$delta_outgroup)))
})

test_that("run_ai_on_chapters_one_turn skips missing chapter text before batching", {
  calls <- 0L
  testthat::local_mocked_bindings(
    new_portkey_chat = function(...) {
      list()
    },
    .package = "nalanda"
  )
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(chat, prompts, type, ...) {
      calls <<- calls + length(prompts)
      lapply(prompts, function(prompt) {
        list(party = "Democrat", rating = 42)
      })
    },
    .package = "ellmer"
  )

  book_texts <- list(
    "Book A" = list(
      "chapter_1.txt" = "Ordinary chapter text.",
      "chapter_2.txt" = NA_character_
    )
  )

  expect_message(
    out <- run_ai_on_chapters_one_turn(
      book_texts = book_texts,
      groups = c("Democrat", "Republican"),
      context_text = "You are simulating a {identity}.",
      question_text = "How warmly do you feel towards your outgroup?"
    ),
    "Skipping 1 chapter\\(s\\) with missing `chapter_text`: Book A - chapter_2.txt"
  )

  out_tbl <- out[["Book A"]]
  skipped <- out_tbl[out_tbl$chapter == "chapter_2.txt", ]

  expect_equal(calls, 2L)
  expect_equal(nrow(out_tbl), 4)
  expect_true(all(is.na(skipped$rating)))
  expect_equal(skipped$party, c("Democrat", "Republican"))
  expect_true(all(is.na(skipped$prompt)))

  metrics <- compute_run_ai_metrics_one_turn(out)
  skipped_metrics <- metrics[metrics$chapter == "chapter_2.txt", ]

  expect_equal(skipped_metrics$party, c("Democrat", "Republican"))
  expect_true(all(is.na(skipped_metrics$outgroup_rating)))
})

test_that("validate_chapter_order accepts epilogue-style chapter labels", {
  expect_equal(
    nalanda:::validate_chapter_order("WFK_9_Epilog.txt"),
    9L
  )
})

test_that("run_ai_on_chapters_one_turn errors clearly on empty chapter input", {
  expect_error(
    run_ai_on_chapters_one_turn(
      book_texts = list(whynationsfails = list()),
      groups = c("Democrat", "Republican"),
      context_text = "You are simulating a {identity}.",
      question_text = "How warmly do you feel towards your outgroup?"
    ),
    "No chapters found in `book_texts`"
  )
})

test_that("run_ai_on_chapters rejects integration and virtual_key together", {
  expect_error(
    run_ai_on_chapters(
      book_texts = "chapter",
      groups = c("Democrat", "Republican"),
      context_text = "You are simulating a {identity}.",
      question_text = "How warmly do you feel towards your outgroup?",
      integration = "vertexai",
      virtual_key = "gemini-8c2498"
    ),
    "Please provide only one of `integration` or `virtual_key`"
  )
})

test_that("run_ai_on_chapters_one_turn rejects integration and virtual_key together", {
  expect_error(
    run_ai_on_chapters_one_turn(
      book_texts = "chapter",
      groups = c("Democrat", "Republican"),
      context_text = "You are simulating a {identity}.",
      question_text = "How warmly do you feel towards your outgroup?",
      integration = "vertexai",
      virtual_key = "gemini-8c2498"
    ),
    "Please provide only one of `integration` or `virtual_key`"
  )
})

test_that("run_ai_on_chapters ignores lingering virtual_key option when integration option is set", {
  withr::local_options(list(
    nalanda.integration = "vertexai",
    nalanda.virtual_key = "gemini-8c2498"
  ))

  bad_book_texts <- list(
    moraltribes = list(
      "4_Chapter_4.txt" = "chapter a",
      "4_Chapter3.txt" = "chapter b"
    )
  )

  expect_error(
    run_ai_on_chapters(
      book_texts = bad_book_texts,
      groups = c("Democrat", "Republican"),
      context_text = "You are simulating a {identity}.",
      question_text = "How warmly do you feel towards your outgroup?"
    ),
    "Duplicate chapters identified while parsing"
  )
})

test_that("run_ai_on_chapters respects explicit integration even with virtual_key option set", {
  withr::local_options(list(nalanda.virtual_key = "gemini-8c2498"))

  bad_book_texts <- list(
    moraltribes = list(
      "4_Chapter_4.txt" = "chapter a",
      "4_Chapter3.txt" = "chapter b"
    )
  )

  expect_error(
    run_ai_on_chapters(
      book_texts = bad_book_texts,
      groups = c("Democrat", "Republican"),
      context_text = "You are simulating a {identity}.",
      question_text = "How warmly do you feel towards your outgroup?",
      integration = "vertexai"
    ),
    "Duplicate chapters identified while parsing"
  )
})

test_that("run_ai_on_chapters_one_turn ignores lingering virtual_key option when integration option is set", {
  withr::local_options(list(
    nalanda.integration = "vertexai",
    nalanda.virtual_key = "gemini-8c2498"
  ))

  err <- NULL
  expect_message(
    err <- tryCatch(
      run_ai_on_chapters_one_turn(
        book_texts = "chapter",
        groups = c("Democrat", "Republican"),
        context_text = "You are simulating a {identity}.",
        question_text = "How warmly do you feel towards your outgroup?"
      ),
      error = identity
    ),
    "Both `nalanda.integration` and `nalanda.virtual_key` options are set; prioritizing `integration`.",
    fixed = TRUE
  )

  expect_s3_class(err, "error")
  expect_false(grepl(
    "Please provide only one of `integration` or `virtual_key`",
    conditionMessage(err),
    fixed = TRUE
  ))
})

test_that("new_portkey_chat error prefers integration guidance", {
  testthat::local_mocked_bindings(
    chat_portkey = function(...) {
      stop("Missing required env var PORTKEY_VIRTUAL_KEY", call. = FALSE)
    },
    .package = "ellmer"
  )

  expect_error(
    nalanda:::new_portkey_chat(
      model = "gemini-2.5-flash-lite",
      base_url = "https://example.com/v1/",
      temperature = 0,
      seed = 42
    ),
    paste0(
      "Please set `options\\(nalanda.integration=\\.\\.\\.\\)` first ",
      "\\(preferred\\), or provide `integration` directly\\. ",
      "Legacy fallback: set `options\\(nalanda.virtual_key=\\.\\.\\.\\)` ",
      "or provide `virtual_key` directly\\."
    )
  )
})

test_that("compute_run_ai_metrics_one_turn computes one-turn per-group summaries", {
  x <- tibble::tibble(
    chapter = c("chapter_1", "chapter_1", "chapter_2", "chapter_2"),
    sim = c(1, 1, 1, 1),
    identity = c("Democrat", "Democrat", "Democrat", "Democrat"),
    target_group = c("Democrat", "Republican", "Democrat", "Republican"),
    rating = c(70, 45, 68, 50),
    party = c("Democrat", "Democrat", "Democrat", "Democrat")
  )
  attr(x, "model") <- "test-model"
  attr(x, "temperature") <- 0
  attr(x, "n_simulations") <- 1

  out <- compute_run_ai_metrics_one_turn(x)

  expect_equal(nrow(out), 2)
  expect_equal(out$ingroup_rating, c(70, 68))
  expect_equal(out$outgroup_rating, c(45, 50))
  expect_equal(out$gap, c(25, 18))
  expect_equal(attr(out, "model"), "test-model")
  expect_equal(attr(out, "temperature"), 0)
  expect_equal(attr(out, "n_simulations"), 1)
})

test_that("compute_run_ai_metrics_one_turn handles single-question mode", {
  x <- tibble::tibble(
    chapter = c("chapter_1", "chapter_2"),
    sim = c(1, 1),
    identity = c("Democrat", "Democrat"),
    rating = c(42, 47)
  )

  out <- compute_run_ai_metrics_one_turn(x)

  expect_equal(out$overall_rating, c(42, 47))
  expect_equal(out$outgroup_rating, c(42, 47))
  expect_true(all(is.na(out$ingroup_rating)))
  expect_true(all(is.na(out$gap)))
})

test_that("make_treatment_prompt interpolates intervention text and identity", {
  out <- make_treatment_prompt(
    prompt_template = "Read this:\n{intervention_text}\nScore it as {identity}.",
    intervention_text = "BOOK TEXT",
    identity_context = "You are a Democrat.",
    identity_label = "Democrat"
  )

  expect_match(out, "You are a Democrat\\.")
  expect_match(out, "BOOK TEXT")
  expect_match(out, "Democrat")
})

test_that("make_treatment_prompt works without identity", {
  out <- make_treatment_prompt(
    prompt_template = "Read this:\n{intervention_text}\nScore it.",
    intervention_text = "BOOK TEXT",
    identity_context = "",
    identity_label = NA_character_
  )

  expect_match(out, "BOOK TEXT")
  expect_false(grepl("\\{identity\\}|\\{group\\}", out))
})

test_that("build_simulate_treatment_prompt remains an alias", {
  out <- build_simulate_treatment_prompt(
    prompt_template = "Read this:\n{intervention_text}\nScore it as {identity}.",
    intervention_text = "BOOK TEXT",
    identity_context = "You are a Democrat.",
    identity_label = "Democrat"
  )

  expect_match(out, "You are a Democrat\\.")
  expect_match(out, "BOOK TEXT")
  expect_match(out, "Democrat")
})

test_that("simulate_treatment defaults intervention_text to empty string", {
  expect_identical(formals(simulate_treatment)$intervention_text, "")
})

test_that("build_chapter_jobs can use intervention-style default ids", {
  out <- nalanda:::build_chapter_jobs(
    "A short intervention.",
    default_unit_id = "intervention_1"
  )

  expect_equal(out$chapter, "intervention_1")
})

test_that("build_chapter_jobs preserves whole-book names", {
  out <- nalanda:::build_chapter_jobs(
    list("Book A" = list(full_book = "A full book."))
  )

  expect_equal(out$book, "Book A")
  expect_equal(out$chapter, "full_book")
  expect_equal(out$chapter_text, "A full book.")
})

test_that("run_ai_on_chapters accepts one named full-book unit per book", {
  expect_error(
    run_ai_on_chapters(
      book_texts = list("Book A" = list(full_book = "A full book.")),
      groups = c("Democrat", "Republican"),
      context_text = "You are a {identity}.",
      question_text = "How warmly do you feel toward {group}s?",
      n_simulations = 0
    ),
    "`n_simulations` must be >= 1.",
    fixed = TRUE
  )
})

test_that("run_ai_on_chapters rejects flat one-book chapter lists clearly", {
  flat_book <- list(
    "1_preface.txt" = "Preface text.",
    "2_introduction.txt" = "Introduction text."
  )

  expect_error(
    run_ai_on_chapters(
      book_texts = flat_book,
      groups = c("Democrat", "Republican"),
      context_text = "You are a {identity}.",
      question_text = "How warmly do you feel toward {group}s?",
      n_simulations = 1
    ),
    "book_texts\\[\"hownottoage\"\\]"
  )
})

test_that("run_ai_on_chapters accepts read_book_texts dollar selections", {
  one_book <- list(
    "1_preface.txt" = "Preface text.",
    "2_introduction.txt" = "Introduction text."
  )
  attr(one_book, "book") <- "hownottoage"

  out <- nalanda:::build_chapter_jobs(one_book)

  expect_equal(out$book, c("hownottoage", "hownottoage"))
  expect_equal(out$chapter, c("1_preface.txt", "2_introduction.txt"))
})

test_that("run_ai_on_chapters checkpoints completed simulation units", {
  checkpoint_dir <- file.path(tempdir(), paste0("nalanda-checkpoints-", Sys.getpid()))
  dir.create(checkpoint_dir)
  on.exit(unlink(checkpoint_dir, recursive = TRUE), add = TRUE)

  expect_message(
    run_ai_on_chapters(
      book_texts = list("Book A" = list(full_book = NA_character_)),
      groups = c("Democrat", "Republican"),
      context_text = "You are a {identity}.",
      question_text = "How warmly do you feel toward {group}s?",
      n_simulations = 1,
      checkpoint_dir = checkpoint_dir,
      checkpoint_prefix = "FULLBOOK_results"
    ),
    "Skipping 1 chapter"
  )

  files <- list.files(checkpoint_dir, pattern = "\\.Rds$", full.names = TRUE)
  expect_length(files, 2)

  out <- dplyr::bind_rows(lapply(files, readRDS))
  expect_true(all(c("model", "book", "chapter", "sim", "identity") %in% names(out)))
  expect_equal(unique(out$book), "Book A")
  expect_equal(unique(out$chapter), "full_book")
  expect_equal(sort(unique(out$identity)), c("Democrat", "Republican"))
})

test_that("run_ai_on_chapters resumes completed units from checkpoint_dir", {
  checkpoint_dir <- file.path(tempdir(), paste0("nalanda-resume-checkpoints-", Sys.getpid()))
  dir.create(checkpoint_dir)
  on.exit(unlink(checkpoint_dir, recursive = TRUE), add = TRUE)

  nalanda:::write_simulation_checkpoint(
    rows = list(
      list(
        book = "Book A",
        chapter = "chapter_1.txt",
        sim = 1L,
        identity = "Democrat",
        party = "Democrat",
        turn_index = 1L,
        turn_type = "baseline",
        target_group = NA_character_,
        rating = 99,
        baseline_prompt = "checkpoint baseline",
        post_prompt = "checkpoint post"
      ),
      list(
        book = "Book A",
        chapter = "chapter_1.txt",
        sim = 1L,
        identity = "Democrat",
        party = "Democrat",
        turn_index = 2L,
        turn_type = "post",
        target_group = NA_character_,
        rating = 100,
        baseline_prompt = "checkpoint baseline",
        post_prompt = "checkpoint post"
      )
    ),
    long_cols = c("baseline_prompt", "post_prompt"),
    checkpoint_dir = checkpoint_dir,
    checkpoint_prefix = "resume-test",
    model_label = "gemini-2.5-flash-lite",
    book = "Book A",
    chapter = "chapter_1.txt",
    identity = "Democrat",
    sim = 1L
  )

  calls <- 0L
  testthat::local_mocked_bindings(
    new_portkey_chat = function(...) {
      calls <<- calls + 1L
      list(
        chat_structured = function(prompt, type) {
          if (grepl("finished reading", prompt, fixed = TRUE)) {
            return(list(rating = 45))
          }
          list(party = "Republican", rating = 40)
        }
      )
    },
    .package = "nalanda"
  )

  out <- run_ai_on_chapters(
    book_texts = list("Book A" = list("chapter_1.txt" = "Chapter text.")),
    groups = c("Democrat", "Republican"),
    context_text = "You are a {identity}.",
    question_text = "How warmly do you feel toward your outgroup?",
    n_simulations = 1,
    checkpoint_dir = checkpoint_dir,
    checkpoint_prefix = "resume-test"
  )

  out_tbl <- out[["Book A"]]
  democrat <- out_tbl[out_tbl$identity == "Democrat", ]
  republican <- out_tbl[out_tbl$identity == "Republican", ]

  expect_equal(calls, 1L)
  expect_equal(democrat$rating, c(99, 100))
  expect_equal(democrat$baseline_prompt, c("checkpoint baseline", "checkpoint baseline"))
  expect_equal(republican$rating, c(40, 45))
})

test_that("run_ai_on_chapters saves one completed file per book", {
  save_dir <- file.path(tempdir(), paste0("nalanda-book-save-", Sys.getpid()))
  dir.create(save_dir)
  on.exit(unlink(save_dir, recursive = TRUE), add = TRUE)

  expect_message(
    run_ai_on_chapters(
      book_texts = list("Book A" = list(full_book = NA_character_)),
      groups = c("Democrat", "Republican"),
      context_text = "You are a {identity}.",
      question_text = "How warmly do you feel toward {group}s?",
      n_simulations = 1,
      save_dir = save_dir,
      save_prefix = "FULLBOOK_results"
    ),
    "Skipping 1 chapter"
  )

  files <- list.files(save_dir, pattern = "\\.Rds$", full.names = TRUE)
  expect_equal(basename(files), "FULLBOOK_results_Book-A.Rds")

  out <- readRDS(files[[1]])
  expect_true(inherits(out, "nalanda"))
  expect_true(all(c("model", "book", "chapter", "sim", "identity") %in% names(out)))
  expect_equal(unique(out$book), "Book A")
  expect_equal(unique(out$chapter), "full_book")
  expect_equal(attr(out, "n_simulations"), 1)
})

test_that("run_ai_on_chapters adds model suffix to multi-model book saves", {
  save_dir <- file.path(tempdir(), paste0("nalanda-book-save-models-", Sys.getpid()))
  dir.create(save_dir)
  on.exit(unlink(save_dir, recursive = TRUE), add = TRUE)

  expect_message(
    run_ai_on_chapters(
      book_texts = list("Book A" = list(full_book = NA_character_)),
      groups = c("Democrat", "Republican"),
      context_text = "You are a {identity}.",
      question_text = "How warmly do you feel toward {group}s?",
      n_simulations = 1,
      model = c("model one", "model two"),
      save_dir = save_dir,
      save_prefix = "FULLBOOK_results"
    ),
    "Skipping 1 chapter"
  )

  files <- sort(basename(list.files(save_dir, pattern = "\\.Rds$", full.names = TRUE)))
  expect_equal(
    files,
    c(
      "FULLBOOK_results_Book-A_model-one.Rds",
      "FULLBOOK_results_Book-A_model-two.Rds"
    )
  )
})

test_that("rename_treatment_output_columns renames treatment-facing columns", {
  x <- tibble::tibble(
    chapter = "intervention_1",
    chapter_index = 1L,
    sim = 1L
  )

  out <- nalanda:::rename_treatment_output_columns(x)

  expect_true("treatment" %in% names(out))
  expect_true("treatment_index" %in% names(out))
  expect_false("chapter" %in% names(out))
  expect_false("chapter_index" %in% names(out))
})

test_that("summarize_treatment_results summarizes readability fields by treatment", {
  x <- tibble::tibble(
    treatment = c("treatment_1", "treatment_1", "treatment_2"),
    sim = c(1, 2, 1),
    turn_type = "turn_1",
    readability_score = c(6, 8, 7),
    readability_confidence = c(4, 5, 4)
  )
  attr(x, "model") <- "@gemini-8c2498/gemini-2.5-flash-lite"
  attr(x, "temperature") <- 0
  attr(x, "n_simulations") <- 2

  out <- summarize_treatment_results(x)

  expect_equal(nrow(out), 2)
  expect_true(all(c(
    "mean_readability_score",
    "sd_readability_score",
    "mean_readability_confidence",
    "treatment_index",
    "turn_type"
  ) %in% names(out)))
  expect_equal(out$mean_readability_score, c(7, 7))
  expect_equal(attr(out, "model"), "gemini-2.5-flash-lite")
  expect_equal(attr(out, "temperature"), 0)
  expect_equal(attr(out, "n_simulations"), 2)
})

test_that("summarize_treatment_results ignores unrelated grouping columns", {
  x <- tibble::tibble(
    book = c("Book A", "Book A", "Book B"),
    treatment = c("treatment_1", "treatment_2", "treatment_1"),
    sim = c(1, 2, 1),
    turn_type = "turn_1",
    readability_score = c(6, 8, 7),
    readability_confidence = c(4, 5, 4)
  )

  out <- summarize_treatment_results(x)

  expect_equal(nrow(out), 2)
  expect_false("book" %in% names(out))
  expect_true(all(c("treatment", "sim", "mean_readability_score") %in% names(out)))
  expect_equal(out$mean_readability_score, c(6.5, 8))
})

test_that("summarize_treatment_results can split by identity", {
  x <- tibble::tibble(
    treatment = c("treatment_1", "treatment_1"),
    sim = c(1, 1),
    identity = c("Democrat", "Republican"),
    turn_type = "turn_1",
    readability_score = c(6, 8)
  )

  out <- summarize_treatment_results(x, by_identity = TRUE)

  expect_equal(nrow(out), 2)
  expect_true("identity" %in% names(out))
  expect_equal(out$mean_readability_score, c(6, 8))
})

test_that("summarize_treatment_results can aggregate readability fields by book", {
  x <- tibble::tibble(
    book = c("Book A", "Book A", "Book B", "Book B"),
    treatment = c("chapter_1", "chapter_2", "chapter_1", "chapter_2"),
    sim = c(1, 1, 1, 1),
    turn_type = "turn_1",
    readability_score = c(6, 8, 7, 9),
    readability_confidence = c(4, 5, 3, 5)
  )

  out <- summarize_treatment_results(
    x,
    aggregate_level = "book"
  )

  expect_equal(out$book, c("Book A", "Book B"))
  expect_false("treatment" %in% names(out))
  expect_equal(out$mean_readability_score, c(7, 8))
  expect_equal(out$mean_readability_confidence, c(4.5, 4))
  expect_equal(attr(out, "aggregate_level"), "book")
})

test_that("summarize_identity_adherence deduplicates repeated one-turn rows", {
  x <- tibble::tibble(
    book = c("Book A", "Book A", "Book A", "Book A", "Book A", "Book A"),
    chapter = c("chapter_1", "chapter_1", "chapter_1", "chapter_1", "chapter_1", "chapter_1"),
    sim = c(1, 1, 2, 2, 3, 3),
    identity = c("Democrat", "Democrat", "Democrat", "Democrat", "Democrat", "Democrat"),
    party = c("Democrat", "Democrat", "Republican", "Republican", "Independent", "Independent"),
    target_group = c("Democrat", "Republican", "Democrat", "Republican", "Democrat", "Republican"),
    rating = c(70, 45, 68, 40, 55, 50)
  )
  attr(x, "model") <- "@vertexai/gemini-3.1-pro-preview"
  attr(x, "temperature") <- 0
  attr(x, "n_simulations") <- 3

  out <- summarize_identity_adherence(x)

  expect_equal(nrow(out), 3)
  expect_equal(out$n, c(1, 1, 1))
  expect_equal(out$total_n, c(3, 3, 3))
  expect_equal(out$prop, c(1 / 3, 1 / 3, 1 / 3))
  expect_equal(out$matches_requested, c(TRUE, FALSE, FALSE))
  expect_equal(attr(out, "model"), "gemini-3.1-pro-preview")
  expect_equal(attr(out, "temperature"), 0)
  expect_equal(attr(out, "n_simulations"), 3)
})

test_that("summarize_identity_adherence can aggregate over requested identity only", {
  x <- tibble::tibble(
    sim = c(1, 1, 2, 2, 3, 3, 4, 4),
    identity = c("Democrat", "Democrat", "Democrat", "Democrat", "Republican", "Republican", "Republican", "Republican"),
    party = c("Democrat", "Democrat", "Independent", "Independent", "Republican", "Republican", "Republican", "Republican"),
    target_group = c("Democrat", "Republican", "Democrat", "Republican", "Democrat", "Republican", "Democrat", "Republican"),
    rating = c(70, 40, 55, 50, 65, 25, 66, 30)
  )

  out <- summarize_identity_adherence(x, by = "identity")

  expect_equal(nrow(out), 3)
  expect_equal(out$identity, c("Democrat", "Democrat", "Republican"))
  expect_equal(out$party, c("Democrat", "Independent", "Republican"))
  expect_equal(out$n, c(1, 1, 2))
  expect_equal(out$total_n, c(2, 2, 2))
  expect_equal(out$prop, c(0.5, 0.5, 1))
  expect_equal(out$matches_requested, c(TRUE, FALSE, TRUE))
})

test_that("simulate_treatment binds multiple models into one raw table", {
  testthat::local_mocked_bindings(
    new_portkey_chat = function(model, base_url, temperature, seed) {
      list(
        chat_structured = function(full_prompt, type) {
          if (grepl("Democrat", full_prompt, fixed = TRUE)) {
            return(list(party = "Democrat"))
          }
          list(party = "Republican")
        }
      )
    },
    .package = "nalanda"
  )

  out <- simulate_treatment(
    model = c("gemini-2.5-pro", "@vertexai/gemini-3.1-pro-preview"),
    groups = c("Democrat", "Republican"),
    context_text = "You are simulating a {identity}.",
    prompt = "Which political identity best describes you?",
    response_type = ellmer::type_object(
      party = ellmer::type_string()
    )
  )

  expect_equal(nrow(out), 4)
  expect_true(all(c("model", "identity", "party") %in% names(out)))
  expect_equal(
    unique(out$model),
    c("gemini-2.5-pro", "gemini-3.1-pro-preview")
  )
  expect_equal(attr(out, "model"), c("gemini-2.5-pro", "gemini-3.1-pro-preview"))
  expect_equal(attr(out, "models"), c("gemini-2.5-pro", "gemini-3.1-pro-preview"))
})

test_that("simulate_treatment can parse soft structured text output", {
  seen_prompt <- NULL
  testthat::local_mocked_bindings(
    new_portkey_chat = function(model, base_url, temperature, seed) {
      list(
        chat = function(full_prompt) {
          seen_prompt <<- full_prompt
          '{"score": "7"}'
        }
      )
    },
    .package = "nalanda"
  )

  out <- simulate_treatment(
    intervention_text = "A short passage.",
    prompt = "Rate the passage from 1 to 10.",
    response_type = ellmer::type_object(score = ellmer::type_number()),
    output_mode = "text"
  )

  expect_equal(out$score, 7)
  expect_equal(out$raw_response, '{"score": "7"}')
  expect_match(seen_prompt, "Response format requirement", fixed = TRUE)
  expect_match(seen_prompt, '"score": <value>', fixed = TRUE)
})

test_that("simulate_treatment checkpoints completed simulation units", {
  checkpoint_dir <- file.path(tempdir(), paste0("nalanda-sim-checkpoints-", Sys.getpid()))
  dir.create(checkpoint_dir)
  on.exit(unlink(checkpoint_dir, recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    new_portkey_chat = function(model, base_url, temperature, seed) {
      list(
        chat_structured = function(full_prompt, type) {
          list(score = 7)
        }
      )
    },
    .package = "nalanda"
  )

  out <- simulate_treatment(
    intervention_text = list("Book A" = list(full_book = "Treatment text.")),
    groups = c("Democrat", "Republican"),
    context_text = "You are a {identity}.",
    prompt = "Score this treatment.",
    response_type = ellmer::type_object(score = ellmer::type_number()),
    checkpoint_dir = checkpoint_dir,
    checkpoint_prefix = "treatment-checkpoint"
  )

  files <- list.files(checkpoint_dir, pattern = "\\.Rds$", full.names = TRUE)
  expect_length(files, 2)
  expect_equal(sort(unique(out[["Book A"]]$identity)), c("Democrat", "Republican"))

  checkpoint_rows <- dplyr::bind_rows(lapply(files, readRDS))
  expect_true(all(c("model", "book", "chapter", "sim", "identity", "score") %in% names(checkpoint_rows)))
  expect_equal(unique(checkpoint_rows$book), "Book A")
  expect_equal(unique(checkpoint_rows$chapter), "full_book")
})

test_that("simulate_treatment resumes completed units from checkpoint_dir", {
  checkpoint_dir <- file.path(tempdir(), paste0("nalanda-sim-resume-", Sys.getpid()))
  dir.create(checkpoint_dir)
  on.exit(unlink(checkpoint_dir, recursive = TRUE), add = TRUE)

  nalanda:::write_simulation_checkpoint(
    rows = list(
      list(
        book = "Book A",
        chapter = "full_book",
        sim = 1L,
        identity = "Democrat",
        turn_index = 1L,
        turn_type = "score",
        prompt = "checkpoint prompt",
        score = 99
      )
    ),
    long_cols = "prompt",
    checkpoint_dir = checkpoint_dir,
    checkpoint_prefix = "treatment-resume",
    model_label = "gemini-2.5-flash-lite",
    book = "Book A",
    chapter = "full_book",
    identity = "Democrat",
    sim = 1L
  )

  calls <- 0L
  testthat::local_mocked_bindings(
    new_portkey_chat = function(model, base_url, temperature, seed) {
      calls <<- calls + 1L
      list(
        chat_structured = function(full_prompt, type) {
          list(score = 10)
        }
      )
    },
    .package = "nalanda"
  )

  out <- simulate_treatment(
    intervention_text = list("Book A" = list(full_book = "Treatment text.")),
    groups = c("Democrat", "Republican"),
    context_text = "You are a {identity}.",
    prompt = c(score = "Score this treatment."),
    response_type = ellmer::type_object(score = ellmer::type_number()),
    checkpoint_dir = checkpoint_dir,
    checkpoint_prefix = "treatment-resume"
  )

  out_tbl <- out[["Book A"]]
  expect_equal(calls, 1L)
  expect_equal(out_tbl$score[out_tbl$identity == "Democrat"], 99)
  expect_equal(out_tbl$prompt[out_tbl$identity == "Democrat"], "checkpoint prompt")
  expect_equal(out_tbl$score[out_tbl$identity == "Republican"], 10)
})

test_that("simulate_treatment saves one completed file per book", {
  save_dir <- file.path(tempdir(), paste0("nalanda-sim-save-", Sys.getpid()))
  dir.create(save_dir)
  on.exit(unlink(save_dir, recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    new_portkey_chat = function(model, base_url, temperature, seed) {
      list(
        chat_structured = function(full_prompt, type) {
          list(score = 7)
        }
      )
    },
    .package = "nalanda"
  )

  simulate_treatment(
    intervention_text = list("Book A" = list(full_book = "Treatment text.")),
    groups = c("Democrat", "Republican"),
    context_text = "You are a {identity}.",
    prompt = "Score this treatment.",
    response_type = ellmer::type_object(score = ellmer::type_number()),
    save_dir = save_dir,
    save_prefix = "SIM_results"
  )

  files <- list.files(save_dir, pattern = "\\.Rds$", full.names = TRUE)
  expect_equal(basename(files), "SIM_results_Book-A.Rds")

  out <- readRDS(files[[1]])
  expect_true(inherits(out, "nalanda"))
  expect_true(all(c("model", "book", "treatment", "sim", "identity", "score") %in% names(out)))
  expect_false("chapter" %in% names(out))
  expect_equal(unique(out$treatment), "full_book")
})

test_that("run_ai_on_chapters can parse text-mode JSON into rating rows", {
  testthat::local_mocked_bindings(
    new_portkey_chat = function(model, base_url, temperature, seed) {
      list(
        chat = function(prompt) {
          if (grepl('"party": <value>', prompt, fixed = TRUE)) {
            return('{"party": "Democrat", "rating_democrat": 80, "rating_republican": 20}')
          }
          '{"rating_democrat": 75, "rating_republican": 25}'
        }
      )
    },
    .package = "nalanda"
  )

  out <- run_ai_on_chapters(
    book_texts = "Chapter text.",
    groups = c("Democrat", "Republican"),
    context_text = "You are simulating a {identity}.",
    question_text = "How warmly do you feel towards {group}s?",
    output_mode = "text"
  )

  expect_equal(nrow(out), 8)
  expect_true(all(c("baseline_raw_response", "post_raw_response") %in% names(out)))
  democrat_baseline <- out[
    out$identity == "Democrat" &
      out$turn_type == "baseline" &
      out$target_group == "Democrat",
  ]
  democrat_post <- out[
    out$identity == "Democrat" &
      out$turn_type == "post" &
      out$target_group == "Republican",
  ]

  expect_equal(democrat_baseline$rating, 80)
  expect_equal(democrat_post$rating, 25)
  expect_equal(unique(out$party), "Democrat")
})

test_that("summarize_identity_match_rates computes one row per model and identity", {
  x <- tibble::tibble(
    model = c(
      "gemini-3.1-pro-preview", "gemini-3.1-pro-preview",
      "gemini-3.1-pro-preview", "gemini-3.1-pro-preview",
      "gemini-2.5-pro", "gemini-2.5-pro",
      "gemini-2.5-pro", "gemini-2.5-pro"
    ),
    sim = c(1, 2, 1, 2, 1, 2, 1, 2),
    identity = c(
      "Democrat", "Democrat", "Republican", "Republican",
      "Democrat", "Democrat", "Republican", "Republican"
    ),
    party = c(
      "Republican", "Independent", "Republican", "Republican",
      "Democrat", "Democrat", "Republican", "Independent"
    )
  )

  out <- summarize_identity_match_rates(x)

  expect_equal(nrow(out), 4)
  expect_equal(
    out$model,
    c(
      "gemini-2.5-pro", "gemini-2.5-pro",
      "gemini-3.1-pro-preview", "gemini-3.1-pro-preview"
    )
  )
  expect_equal(
    out$identity,
    c("Democrat", "Republican", "Democrat", "Republican")
  )
  expect_equal(out$n_requested, c(2, 2, 2, 2))
  expect_equal(out$n_match, c(2, 1, 0, 2))
  expect_equal(out$adoption_rate, c(1, 0.5, 0, 1))
  expect_equal(out$n_mismatch, c(0, 1, 2, 0))
})

test_that("summarize_identity_match_rates can return compact one-row-per-model output", {
  x <- tibble::tibble(
    model = c(
      "gemini-3.1-pro-preview", "gemini-3.1-pro-preview",
      "gemini-3.1-pro-preview", "gemini-3.1-pro-preview",
      "gemini-2.5-pro", "gemini-2.5-pro",
      "gemini-2.5-pro", "gemini-2.5-pro"
    ),
    sim = c(1, 2, 1, 2, 1, 2, 1, 2),
    identity = c(
      "Democrat", "Democrat", "Republican", "Republican",
      "Democrat", "Democrat", "Republican", "Republican"
    ),
    party = c(
      "Republican", "Independent", "Republican", "Republican",
      "Democrat", "Democrat", "Republican", "Independent"
    )
  )

  out <- summarize_identity_match_rates(x, compact = TRUE)

  expect_equal(nrow(out), 2)
  expect_equal(out$model, c("gemini-2.5-pro", "gemini-3.1-pro-preview"))
  expect_equal(out$n, c(2, 2))
  expect_equal(out$rate_democrat, c(1, 0))
  expect_equal(out$rate_republican, c(0.5, 1))
})

test_that("summarize_identity_adherence can return compact one-row-per-model output", {
  x <- tibble::tibble(
    model = c(
      "gemini-3.1-pro-preview", "gemini-3.1-pro-preview",
      "gemini-3.1-pro-preview", "gemini-3.1-pro-preview",
      "gemini-2.5-pro", "gemini-2.5-pro",
      "gemini-2.5-pro", "gemini-2.5-pro"
    ),
    sim = c(1, 2, 3, 4, 1, 2, 3, 4),
    identity = c(
      "Democrat", "Democrat", "Republican", "Republican",
      "Democrat", "Democrat", "Republican", "Republican"
    ),
    party = c(
      "Republican", "Independent", "Republican", "Republican",
      "Democrat", "Democrat", "Republican", "Independent"
    )
  )

  out <- summarize_identity_adherence(x, by = "model", compact = TRUE)

  expect_equal(nrow(out), 2)
  expect_equal(out$model, c("gemini-2.5-pro", "gemini-3.1-pro-preview"))
  expect_equal(out$n, c(4, 4))
  expect_equal(out$rate_democrat, c(0.5, NA))
  expect_equal(out$rate_republican, c(0.25, 0.75))
  expect_equal(out$rate_independent, c(0.25, 0.25))
})

test_that("identity summaries preserve original model order when available", {
  x <- tibble::tibble(
    model = c(
      "gemini-3.1-pro-preview", "gemini-3.1-pro-preview",
      "gemini-2.5-pro", "gemini-2.5-pro"
    ),
    sim = c(1, 2, 1, 2),
    identity = c("Democrat", "Republican", "Democrat", "Republican"),
    party = c("Republican", "Republican", "Democrat", "Independent")
  )
  attr(x, "models") <- c("gemini-3.1-pro-preview", "gemini-2.5-pro")

  out_match <- summarize_identity_match_rates(x, compact = TRUE)
  out_adherence <- summarize_identity_adherence(x, by = "model", compact = TRUE)

  expect_equal(
    out_match$model,
    c("gemini-3.1-pro-preview", "gemini-2.5-pro")
  )
  expect_equal(
    out_adherence$model,
    c("gemini-3.1-pro-preview", "gemini-2.5-pro")
  )
})


test_that("compute_run_ai_metrics_cumulative compares each chapter to baseline", {
  x <- tibble::tibble(
    book = c("Book A", "Book A", "Book A", "Book A", "Book A", "Book A"),
    chapter = c("baseline", "baseline", "chapter_1", "chapter_1", "chapter_2", "chapter_2"),
    chapter_index = c(0L, 0L, 1L, 1L, 2L, 2L),
    sim = c(1, 1, 1, 1, 1, 1),
    identity = c("Democrat", "Democrat", "Democrat", "Democrat", "Democrat", "Democrat"),
    party = c("Democrat", "Democrat", "Democrat", "Democrat", "Democrat", "Democrat"),
    turn_type = c("baseline", "baseline", "post", "post", "post", "post"),
    target_group = c("Democrat", "Republican", "Democrat", "Republican", "Democrat", "Republican"),
    rating = c(70, 40, 68, 45, 66, 50),
    baseline_prompt = "baseline",
    post_prompt = c(NA, NA, "chapter 1", "chapter 1", "chapter 2", "chapter 2")
  )
  attr(x, "model") <- "test-model"
  attr(x, "temperature") <- 0
  attr(x, "n_simulations") <- 1

  out <- compute_run_ai_metrics_cumulative(x)

  expect_equal(nrow(out), 2)
  expect_equal(out$chapter, c("chapter_1", "chapter_2"))
  expect_equal(out$pre_outgroup, c(40, 40))
  expect_equal(out$post_outgroup, c(45, 50))
  expect_equal(out$delta_outgroup, c(5, 10))
  expect_equal(out$delta_gap, c(7, 14))
  expect_equal(attr(out, "model"), "test-model")
})

test_that("compute_run_ai_metrics delegates cumulative raw output", {
  x <- tibble::tibble(
    book = c("Book A", "Book A", "Book A", "Book A", "Book A", "Book A"),
    chapter = c("baseline", "baseline", "chapter_1", "chapter_1", "chapter_2", "chapter_2"),
    chapter_index = c(0L, 0L, 1L, 1L, 2L, 2L),
    sim = c(1, 1, 1, 1, 1, 1),
    identity = c("Democrat", "Democrat", "Democrat", "Democrat", "Democrat", "Democrat"),
    party = c("Democrat", "Democrat", "Democrat", "Democrat", "Democrat", "Democrat"),
    turn_type = c("baseline", "baseline", "post", "post", "post", "post"),
    target_group = c("Democrat", "Republican", "Democrat", "Republican", "Democrat", "Republican"),
    rating = c(70, 40, 68, 45, 66, 50),
    baseline_prompt = "baseline",
    post_prompt = c(NA, NA, "chapter 1", "chapter 1", "chapter 2", "chapter 2")
  )

  out <- compute_run_ai_metrics(x)

  expect_equal(nrow(out), 2)
  expect_equal(out$chapter, c("chapter_1", "chapter_2"))
  expect_equal(out$pre_outgroup, c(40, 40))
  expect_equal(out$post_outgroup, c(45, 50))
})

test_that("run_ai_cumulative_chapters skips missing chapter text while preserving party", {
  calls <- 0L
  testthat::local_mocked_bindings(
    new_portkey_chat = function(...) {
      list(
        chat_structured = function(prompt, type) {
          calls <<- calls + 1L
          party <- if (grepl("IDENTITY:Democrat", prompt, fixed = TRUE)) {
            "Democrat"
          } else {
            "Republican"
          }
          list(
            party = party,
            rating_democrat = 60,
            rating_republican = 40
          )
        }
      )
    },
    .package = "nalanda"
  )

  book_texts <- list(
    "Book A" = list(
      "chapter_1.txt" = "Ordinary chapter text.",
      "chapter_2.txt" = NA_character_
    )
  )

  expect_message(
    out <- run_ai_cumulative_chapters(
      book_texts = book_texts,
      groups = c("Democrat", "Republican"),
      context_text = "IDENTITY:{identity}.",
      question_text = "How warmly do you feel towards {group}s?"
    ),
    "Skipping 1 chapter\\(s\\) with missing `chapter_text`: Book A - chapter_2.txt"
  )

  out_tbl <- out[["Book A"]]
  skipped <- out_tbl[out_tbl$chapter == "chapter_2.txt", ]

  expect_equal(calls, 4L)
  expect_equal(nrow(skipped), 4)
  expect_true(all(is.na(skipped$rating)))
  expect_equal(unique(skipped$party), c("Democrat", "Republican"))
  expect_true(all(is.na(skipped$post_prompt)))

  metrics <- compute_run_ai_metrics_cumulative(out)
  skipped_metrics <- metrics[metrics$chapter == "chapter_2.txt", ]

  expect_equal(skipped_metrics$party, c("Democrat", "Republican"))
  expect_true(all(is.na(skipped_metrics$post_outgroup)))
  expect_true(all(is.na(skipped_metrics$delta_outgroup)))
})

test_that("run_ai_cumulative_chapters checkpoints completed conversations", {
  checkpoint_dir <- file.path(tempdir(), paste0("nalanda-cumulative-checkpoints-", Sys.getpid()))
  dir.create(checkpoint_dir)
  on.exit(unlink(checkpoint_dir, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(
    new_portkey_chat = function(...) {
      list(
        chat_structured = function(prompt, type) {
          party <- if (grepl("Democrat", prompt, fixed = TRUE)) {
            "Democrat"
          } else {
            "Republican"
          }
          list(
            party = party,
            rating_democrat = 60,
            rating_republican = 40
          )
        }
      )
    },
    .package = "nalanda"
  )

  expect_message(
    run_ai_cumulative_chapters(
      book_texts = list("Book A" = list("chapter_1.txt" = NA_character_)),
      groups = c("Democrat", "Republican"),
      context_text = "You are a {identity}.",
      question_text = "How warmly do you feel toward {group}s?",
      n_simulations = 1,
      checkpoint_dir = checkpoint_dir,
      checkpoint_prefix = "CUMULATIVE_results"
    ),
    "Skipping 1 chapter"
  )

  files <- list.files(checkpoint_dir, pattern = "\\.Rds$", full.names = TRUE)
  expect_length(files, 2)
  expect_true(all(grepl("cumulative", basename(files), fixed = TRUE)))

  out <- dplyr::bind_rows(lapply(files, readRDS))
  expect_true(all(c("model", "book", "chapter", "sim", "identity") %in% names(out)))
  expect_equal(unique(out$book), "Book A")
  expect_equal(sort(unique(out$chapter)), c("baseline", "chapter_1.txt"))
  expect_equal(sort(unique(out$identity)), c("Democrat", "Republican"))
})

test_that("run_ai_cumulative_chapters saves one completed file per book", {
  save_dir <- file.path(tempdir(), paste0("nalanda-cumulative-save-", Sys.getpid()))
  dir.create(save_dir)
  on.exit(unlink(save_dir, recursive = TRUE), add = TRUE)
  testthat::local_mocked_bindings(
    new_portkey_chat = function(...) {
      list(
        chat_structured = function(prompt, type) {
          party <- if (grepl("Democrat", prompt, fixed = TRUE)) {
            "Democrat"
          } else {
            "Republican"
          }
          list(
            party = party,
            rating_democrat = 60,
            rating_republican = 40
          )
        }
      )
    },
    .package = "nalanda"
  )

  expect_message(
    run_ai_cumulative_chapters(
      book_texts = list("Book A" = list("chapter_1.txt" = NA_character_)),
      groups = c("Democrat", "Republican"),
      context_text = "You are a {identity}.",
      question_text = "How warmly do you feel toward {group}s?",
      n_simulations = 1,
      save_dir = save_dir,
      save_prefix = "CUMULATIVE_results"
    ),
    "Skipping 1 chapter"
  )

  files <- list.files(save_dir, pattern = "\\.Rds$", full.names = TRUE)
  expect_equal(basename(files), "CUMULATIVE_results_Book-A.Rds")

  out <- readRDS(files[[1]])
  expect_true(inherits(out, "nalanda"))
  expect_true(all(c("model", "book", "chapter", "sim", "identity") %in% names(out)))
  expect_equal(unique(out$book), "Book A")
  expect_equal(sort(unique(out$chapter)), c("baseline", "chapter_1.txt"))
  expect_equal(attr(out, "n_simulations"), 1)
})

test_that("run_ai_cumulative_chapters rejects non-nested input", {
  expect_error(
    run_ai_cumulative_chapters(
      book_texts = "chapter",
      groups = c("Democrat", "Republican"),
      context_text = "You are simulating a {identity}.",
      question_text = "How warmly do you feel towards your outgroup?"
    ),
    "nested list of books and chapters"
  )
})

test_that("toy_sim_results is available as package data", {
  data_env <- new.env(parent = emptyenv())
  utils::data("toy_sim_results", package = "nalanda", envir = data_env)

  expect_true(exists("toy_sim_results", envir = data_env, inherits = FALSE))
  expect_s3_class(data_env$toy_sim_results, "data.frame")
  expect_equal(attr(data_env$toy_sim_results, "model"), "gemini-2.5-flash-lite")
  expect_equal(attr(data_env$toy_sim_results, "temperature"), 0)
})

test_that("summarize_chapter_scores does not split by identity when by_party = FALSE", {
  x <- tibble::tibble(
    book = c("Book A", "Book A"),
    chapter = c("chapter_1", "chapter_1"),
    party = c("Democrat", "Republican"),
    identity = c("Democrat", "Republican"),
    pre_ingroup = c(60, 58),
    post_ingroup = c(64, 60),
    pre_outgroup = c(40, 42),
    post_outgroup = c(48, 46),
    pre_gap = c(20, 16),
    post_gap = c(16, 14),
    delta_outgroup = c(8, 4),
    delta_ingroup = c(4, 2),
    delta_gap = c(4, 2),
    chapter_excerpt = c("x", "x")
  )

  out <- summarize_chapter_scores(x, by_party = FALSE)

  expect_equal(nrow(out), 1)
  expect_false("party" %in% names(out))
  expect_false("identity" %in% names(out))
})

test_that("summarize_chapter_scores splits by party when by_party = TRUE", {
  x <- tibble::tibble(
    book = c("Book A", "Book A"),
    chapter = c("chapter_1", "chapter_1"),
    party = c("Democrat", "Republican"),
    identity = c("Democrat", "Republican"),
    pre_ingroup = c(60, 58),
    post_ingroup = c(64, 60),
    pre_outgroup = c(40, 42),
    post_outgroup = c(48, 46),
    pre_gap = c(20, 16),
    post_gap = c(16, 14),
    delta_outgroup = c(8, 4),
    delta_ingroup = c(4, 2),
    delta_gap = c(4, 2),
    chapter_excerpt = c("x", "x")
  )

  out <- summarize_chapter_scores(x, by_party = TRUE)

  expect_equal(nrow(out), 2)
  expect_true("party" %in% names(out))
  expect_false("identity" %in% names(out))
})

test_that("summarize_simulation_stability compacts chapter-level variation", {
  x <- tibble::tibble(
    book = c("Book A", "Book A", "Book A", "Book A"),
    chapter = c("chapter_1", "chapter_1", "chapter_2", "chapter_2"),
    party = c("Democrat", "Republican", "Democrat", "Republican"),
    sim = c(2, 2, 2, 2),
    sd_pre_ingroup = c(0, 0, 0, 0),
    sd_post_ingroup = c(1, 0, 0.5, 0),
    sd_pre_outgroup = c(0, 0, 0, 0),
    sd_post_outgroup = c(0, 0, 0.25, 0),
    sd_pre_gap = c(0, 0, 0, 0),
    sd_post_gap = c(1, 0, 0.25, 0),
    sd_delta_outgroup = c(0, 0, 0.25, 0),
    sd_delta_ingroup = c(1, 0, 0.5, 0),
    sd_delta_gap = c(1, 0, 0.25, 0)
  )
  attr(x, "model") <- "@vertexai/gemini-3.1-pro-preview"
  attr(x, "temperature") <- 0
  attr(x, "n_simulations") <- 2

  out <- summarize_simulation_stability(x)

  expect_equal(nrow(out), 2)
  expect_equal(out$party, c("Democrat", "Republican"))
  expect_equal(out$n_units, c(2, 2))
  expect_equal(out$prop_units_any_pre_variation, c(0, 0))
  expect_equal(out$prop_units_any_post_variation, c(1, 0))
  expect_equal(out$all_stable, c(FALSE, TRUE))
  expect_equal(attr(out, "model"), "gemini-3.1-pro-preview")
  expect_equal(attr(out, "temperature"), 0)
  expect_equal(attr(out, "n_simulations"), 2)
})

test_that("summarize_simulation_stability can derive summaries from raw metrics", {
  x <- tibble::tibble(
    book = c("Book A", "Book A", "Book A", "Book A"),
    chapter = c("chapter_1", "chapter_1", "chapter_1", "chapter_1"),
    sim = c(1, 2, 1, 2),
    identity = c("Democrat", "Democrat", "Republican", "Republican"),
    party = c("Democrat", "Democrat", "Republican", "Republican"),
    pre_ingroup = c(60, 60, 55, 55),
    post_ingroup = c(64, 66, 58, 58),
    pre_outgroup = c(40, 40, 42, 42),
    post_outgroup = c(48, 50, 46, 46),
    pre_gap = c(20, 20, 13, 13),
    post_gap = c(16, 16, 12, 12),
    delta_outgroup = c(8, 10, 4, 4),
    delta_ingroup = c(4, 6, 3, 3),
    delta_gap = c(4, 4, 1, 1)
  )

  out <- summarize_simulation_stability(x, by = c("book", "party"))

  expect_equal(nrow(out), 2)
  expect_equal(out$book, c("Book A", "Book A"))
  expect_equal(out$party, c("Democrat", "Republican"))
  expect_equal(out$n_units, c(1, 1))
  expect_equal(out$prop_units_any_pre_variation, c(0, 0))
  expect_equal(out$prop_units_any_post_variation, c(1, 0))
  expect_equal(out$all_stable, c(FALSE, TRUE))
})

test_that("summarize_simulation_stability handles recovered full-book raw metrics", {
  x <- tibble::tibble(
    book = rep("Book A", 4),
    chapter = rep("full_book", 4),
    sim = c(1, 2, 1, 2),
    identity = c("Democrat", "Democrat", "Republican", "Republican"),
    party = c("Democrat", "Democrat", "Republican", "Republican"),
    pre_ingroup = c(60, 60, 55, 55),
    post_ingroup = c(64, 66, 58, 58),
    pre_outgroup = c(40, 40, 42, 42),
    post_outgroup = c(48, 50, 46, 46),
    pre_gap = c(20, 20, 13, 13),
    post_gap = c(16, 16, 12, 12),
    delta_outgroup = c(8, 10, 4, 4),
    delta_ingroup = c(4, 6, 3, 3),
    delta_gap = c(4, 4, 1, 1)
  )
  attr(x, "chapter_excerpts") <- tibble::tibble(
    chapter = "full_book",
    chapter_excerpt = "whole book preview"
  )

  out <- summarize_simulation_stability(x, by = c("book", "party"))

  expect_equal(nrow(out), 2)
  expect_equal(out$book, c("Book A", "Book A"))
  expect_equal(out$party, c("Democrat", "Republican"))
  expect_equal(out$n_units, c(1, 1))
  expect_equal(out$prop_units_any_post_variation, c(1, 0))
})

test_that("summarize_simulation_stability handles recovered full-book lists", {
  make_book <- function(book, post_values) {
    x <- tibble::tibble(
      book = rep(book, 4),
      chapter = rep("full_book", 4),
      sim = c(1, 2, 1, 2),
      identity = c("Democrat", "Democrat", "Republican", "Republican"),
      party = c("Democrat", "Democrat", "Republican", "Republican"),
      pre_ingroup = c(60, 60, 55, 55),
      post_ingroup = post_values,
      pre_outgroup = c(40, 40, 42, 42),
      post_outgroup = c(48, 50, 46, 46),
      pre_gap = c(20, 20, 13, 13),
      post_gap = c(16, 16, 12, 12),
      delta_outgroup = c(8, 10, 4, 4),
      delta_ingroup = c(4, 6, 3, 3),
      delta_gap = c(4, 4, 1, 1)
    )
    attr(x, "chapter_excerpts") <- tibble::tibble(
      chapter = "full_book",
      chapter_excerpt = paste("preview", book)
    )
    x
  }

  out <- summarize_simulation_stability(
    list(
      make_book("Book A", c(64, 66, 58, 58)),
      make_book("Book B", c(61, 61, 57, 59))
    ),
    by = c("book", "party")
  )

  expect_equal(nrow(out), 4)
  expect_equal(out$book, c("Book A", "Book A", "Book B", "Book B"))
  expect_equal(out$party, rep(c("Democrat", "Republican"), 2))
})

test_that("summarize_simulation_stability warns when no units are testable", {
  x <- tibble::tibble(
    book = c("Book A", "Book A"),
    chapter = c("chapter_1", "chapter_1"),
    party = c("Democrat", "Republican"),
    sim = c(1, 1),
    sd_pre_ingroup = c(NA_real_, NA_real_),
    sd_post_ingroup = c(NA_real_, NA_real_),
    sd_pre_outgroup = c(NA_real_, NA_real_),
    sd_post_outgroup = c(NA_real_, NA_real_)
  )

  expect_warning(
    out <- summarize_simulation_stability(x),
    "no assessed units had `sim > 1`"
  )

  expect_equal(out$prop_units_any_pre_variation, c(NA_real_, NA_real_))
  expect_equal(out$prop_units_any_post_variation, c(NA_real_, NA_real_))
  expect_equal(out$all_stable, c(TRUE, TRUE))
})

test_that("plot_forest_books can prepare internally from summary data", {
  summary_books <- tibble::tibble(
    book = c("Book A", "Book B"),
    sim = c(10, 12),
    mean_delta_gap = c(1.2, 0.8),
    sd_delta_gap = c(0.6, 0.5)
  )

  expect_no_error(
    plot_forest_books(
      summary_books,
      dv = "delta_gap",
      show_overall = FALSE,
      ci.vertices = FALSE
    )
  )
})

test_that("plot_forest_books still accepts precomputed forest data", {
  forest_df <- tibble::tibble(
    book = c("Book A (n = 10)", "Book B (n = 12)"),
    mean = c(1.2, 0.8),
    lower = c(0.8, 0.5),
    upper = c(1.6, 1.1),
    ci = c("1.2 [0.8, 1.6]", "0.8 [0.5, 1.1]")
  )

  expect_no_error(
    plot_forest_books(
      forest_df,
      show_overall = FALSE,
      ci.vertices = FALSE
    )
  )
})

test_that("plot_forest_books appends model info on a new title line", {
  summary_books <- tibble::tibble(
    book = c("Book A", "Book B"),
    sim = c(10, 12),
    mean_delta_gap = c(1.2, 0.8),
    sd_delta_gap = c(0.6, 0.5)
  )
  attr(summary_books, "model") <- "@gemini-8c2498/gemini-2.5-flash-lite"
  attr(summary_books, "temperature") <- 0

  p <- plot_forest_books(
    summary_books,
    dv = "delta_gap",
    title = "Reduction in polarization gap",
    show_overall = FALSE,
    ci.vertices = FALSE
  )

  expect_equal(
    p$title,
    "Reduction in polarization gap (model = \"gemini-2.5-flash-lite\"; temperature = 0)"
  )
})

test_that("plot_forest_books treats zero = NULL as no reference line", {
  summary_books <- tibble::tibble(
    book = c("Book A", "Book B"),
    sim = c(10, 12),
    mean_delta_gap = c(1.2, 0.8),
    sd_delta_gap = c(0.6, 0.5)
  )

  expect_no_error(
    plot_forest_books(
      summary_books,
      dv = "delta_gap",
      zero = NULL,
      show_overall = FALSE,
      ci.vertices = FALSE
    )
  )
})

test_that("party inputs are consolidated to one row per book", {
  forest_df <- tibble::tibble(
    book = c("Book A", "Book A", "Book B", "Book B"),
    party = c("Democrat", "Republican", "Democrat", "Republican"),
    mean = c(8, 12, 10, 14),
    lower = c(7, 11, 9, 13),
    upper = c(9, 13, 11, 15),
    ci = c("8 [7, 9]", "12 [11, 13]", "10 [9, 11]", "14 [13, 15]")
  )

  built <- nalanda:::.build_forestplot_inputs(
    forest_df,
    label_cols = c("book"),
    show_ci_label = TRUE,
    ci_multiline = TRUE,
    ci_show_party = FALSE
  )

  expect_equal(nrow(built$label_mat), 2)
  expect_true(is.matrix(built$mean))
  expect_equal(ncol(built$mean), 2)
})

test_that("grouped CI labels can be multiline and without party prefixes", {
  forest_df <- tibble::tibble(
    book = c("Book A", "Book A"),
    party = c("Democrat", "Republican"),
    mean = c(8, 12),
    lower = c(7, 11),
    upper = c(9, 13),
    ci = c("8 [7, 9]", "12 [11, 13]")
  )

  built <- nalanda:::.build_forestplot_inputs(
    forest_df,
    label_cols = c("book"),
    show_ci_label = TRUE,
    ci_multiline = TRUE,
    ci_show_party = FALSE
  )

  expect_match(built$label_mat[1, "ci"], "\\n")
  expect_false(grepl("Democrat:", built$label_mat[1, "ci"]))
})

test_that("plot_forest_books allows compact CI text styling", {
  grouped <- tibble::tibble(
    book = c("Book A", "Book A", "Book B", "Book B"),
    party = c("Democrat", "Republican", "Democrat", "Republican"),
    mean = c(8, 12, 10, 14),
    lower = c(7, 11, 9, 13),
    upper = c(9, 13, 11, 15),
    ci = c("8 [7, 9]", "12 [11, 13]", "10 [9, 11]", "14 [13, 15]")
  )

  expect_no_error(
    plot_forest_books(
      grouped,
      show_ci_label = TRUE,
      ci_multiline = TRUE,
      ci_label_fontsize = 8,
      ci_label_lineheight = 0.8,
      show_overall = FALSE
    )
  )
})

test_that("grouped forest plot with header prints", {
  grouped <- tibble::tibble(
    book = c("Book A", "Book A", "Book B", "Book B"),
    party = c("Democrat", "Republican", "Democrat", "Republican"),
    sim = c(10, 10, 12, 12),
    mean_delta_gap = c(8, 12, 10, 14),
    sd_delta_gap = c(1, 1, 1, 1)
  )

  p <- plot_forest_books(
    grouped,
    label_cols = "book",
    dv = "delta_gap",
    header = c("Book tested", "Effect [95% CI]"),
    ci_label_fontsize = 8,
    ci_label_lineheight = 0.8
  )

  expect_no_error(print(p))
})

test_that("grouped forest plot prints with one simulation per party", {
  grouped <- tibble::tibble(
    book = c("Book A", "Book A", "Book B", "Book B"),
    party = c("Democrat", "Republican", "Democrat", "Republican"),
    sim = c(1, 1, 1, 1),
    mean_delta_gap = c(8, 12, 10, 14),
    sd_delta_gap = c(NA_real_, NA_real_, NA_real_, NA_real_)
  )

  p <- plot_forest_books(
    grouped,
    label_cols = "book",
    dv = "delta_gap",
    header = c("Book tested", "Effect [95% CI]"),
    ci_label_fontsize = 8,
    ci_label_lineheight = 0.8,
    show_overall = FALSE
  )

  expect_no_error(print(p))
})

test_that("plot_forest_books ignores extra unnamed headers without CI labels", {
  grouped <- tibble::tibble(
    book = c("Book A", "Book A", "Book B", "Book B"),
    party = c("Democrat", "Republican", "Democrat", "Republican"),
    sim = c(1, 1, 1, 1),
    mean_delta_gap = c(8, 12, 10, 14),
    sd_delta_gap = c(NA_real_, NA_real_, NA_real_, NA_real_)
  )

  p <- plot_forest_books(
    grouped,
    label_cols = "book",
    dv = "delta_gap",
    header = c("Book tested", "Effect [95% CI]"),
    show_ci_label = FALSE,
    show_overall = FALSE
  )

  expect_no_error(print(p))
})

test_that("plot_forest_books reports all-missing estimates clearly", {
  grouped <- tibble::tibble(
    book = c("Book A", "Book A"),
    party = c("Democrat", "Republican"),
    sim = c(10, 10),
    mean_delta_gap = c(NA_real_, NA_real_),
    sd_delta_gap = c(NA_real_, NA_real_)
  )

  expect_error(
    plot_forest_books(
      grouped,
      label_cols = "book",
      dv = "delta_gap",
      header = c("Book tested", "Effect [95% CI]")
    ),
    "No finite estimates are available"
  )
})

test_that("plot_forest_books computes readable integer x-axis ticks by default", {
  ticks <- nalanda:::.compute_forest_xticks(
    lower = c(6.56, 18.81, 22.02),
    upper = c(9.80, 26.19, 29.23),
    mean = c(8.18, 22.50, 27.92)
  )

  expect_equal(ticks$xticks, c(5, 10, 15, 20, 25, 30))
  expect_equal(ticks$xticks.digits, 0L)
})

test_that("plot_forest_books respects manual x-axis tick settings", {
  ticks <- nalanda:::.compute_forest_xticks(
    lower = c(0.12, 0.35),
    upper = c(0.44, 0.61),
    mean = c(0.22, 0.48),
    xticks = c(0.2, 0.4, 0.6),
    xticks.digits = 1
  )

  expect_equal(ticks$xticks, c(0.2, 0.4, 0.6))
  expect_equal(ticks$xticks.digits, 1)
})

test_that("ungrouped forest plot prints without fn.ci_norm mismatch", {
  summary_books <- tibble::tibble(
    book = c("Book A", "Book B"),
    sim = c(10, 12),
    mean_delta_outgroup = c(1.2, 0.8),
    sd_delta_outgroup = c(0.6, 0.5)
  )

  p <- plot_forest_books(
    summary_books,
    label_cols = "book",
    dv = "delta_outgroup",
    header = c("Book tested", "Effect [95% CI]"),
    show_overall = FALSE
  )

  expect_no_error(print(p))
})

