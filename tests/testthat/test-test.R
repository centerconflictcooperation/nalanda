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

test_that("summarize_chapter_scores computes chapter summaries and keeps attrs", {
  x <- tibble::tibble(
    book = c("Book A", "Book A", "Book A", "Book A"),
    chapter = c("chapter_1", "chapter_1", "chapter_2", "chapter_2"),
    pre_ingroup = c(60, 62, 58, 60),
    post_ingroup = c(64, 66, 60, 62),
    pre_outgroup = c(40, 42, 45, 47),
    post_outgroup = c(48, 50, 49, 51),
    gap_pre = c(20, 20, 13, 13),
    gap_post = c(16, 16, 11, 11),
    delta_outgroup = c(8, 8, 4, 4),
    delta_ingroup = c(4, 4, 2, 2),
    delta_gap = c(4, 4, 2, 2),
    chapter_excerpt = c("x", "x", "y", "y")
  )
  attr(x, "model") <- "test-model"
  attr(x, "temperature") <- 0

  out <- summarize_chapter_scores(x)

  expect_equal(nrow(out), 2)
  expect_true(all(
    c("mean_delta_gap", "sd_delta_gap", "chapter_index") %in% names(out)
  ))
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
    gap_pre = c(20, 13),
    gap_post = c(16, 11),
    delta_outgroup = c(8, 4),
    delta_ingroup = c(4, 2),
    delta_gap = c(4, 2),
    chapter_excerpt = c("x", "y")
  )

  out <- summarize_chapter_scores(x, aggregate_level = "book")

  expect_equal(nrow(out), 1)
  expect_true(all(c("book", "mean_delta_gap", "sim") %in% names(out)))
  expect_true(all(is.na(out$chapter_excerpt)))
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
    gap_pre = c(20, 16),
    gap_post = c(16, 14),
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
    gap_pre = c(20, 16),
    gap_post = c(16, 14),
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
