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
  expect_true(all(c("mean_delta_gap", "sd_delta_gap", "chapter_index") %in% names(out)))
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
