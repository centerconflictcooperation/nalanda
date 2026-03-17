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
  expect_equal(
    attr(out, "chapter_excerpts"),
    tibble::tibble(
      chapter = c("chapter_1", "chapter_2"),
      chapter_excerpt = c("preview 1", "preview 2")
    )
  )
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

  out <- compute_run_ai_metrics_one_turn(x)

  expect_equal(nrow(out), 2)
  expect_equal(out$ingroup_rating, c(70, 68))
  expect_equal(out$outgroup_rating, c(45, 50))
  expect_equal(out$gap, c(25, 18))
  expect_equal(attr(out, "model"), "test-model")
  expect_equal(attr(out, "temperature"), 0)
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

