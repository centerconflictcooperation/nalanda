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

