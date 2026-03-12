test_that("plot_chapters_over_time appends model info to subtitle by default", {
  skip_if_not_installed("Rmisc")

  chapters <- tibble::tibble(
    book = c("Book A", "Book A", "Book A", "Book A"),
    chapter = c("Chapter 1", "Chapter 2", "Chapter 1", "Chapter 2"),
    sim = c(1, 1, 2, 2),
    delta_gap = c(1, 2, 1.5, 2.5)
  )
  attr(chapters, "model") <- "gpt-test"
  attr(chapters, "temperature") <- 0.2

  p <- plot_chapters_over_time(chapters)

  expect_equal(p$labels$subtitle, "Model: gpt-test; Temp: 0.2")
})

test_that("plot_chapters_over_time appends model info to provided subtitle", {
  skip_if_not_installed("Rmisc")

  chapters <- tibble::tibble(
    book = c("Book A", "Book A", "Book A", "Book A"),
    chapter = c("Chapter 1", "Chapter 2", "Chapter 1", "Chapter 2"),
    sim = c(1, 1, 2, 2),
    delta_gap = c(1, 2, 1.5, 2.5)
  )
  attr(chapters, "model") <- "gpt-test"
  attr(chapters, "temperature") <- 0.2

  p <- plot_chapters_over_time(chapters, plot_subtitle = "My subtitle")

  expect_equal(
    p$labels$subtitle,
    "My subtitle | Model: gpt-test; Temp: 0.2"
  )
})

test_that("plot_chapters_over_time can disable model info appending", {
  skip_if_not_installed("Rmisc")

  chapters <- tibble::tibble(
    book = c("Book A", "Book A", "Book A", "Book A"),
    chapter = c("Chapter 1", "Chapter 2", "Chapter 1", "Chapter 2"),
    sim = c(1, 1, 2, 2),
    delta_gap = c(1, 2, 1.5, 2.5)
  )
  attr(chapters, "model") <- "gpt-test"
  attr(chapters, "temperature") <- 0.2

  p <- plot_chapters_over_time(
    chapters,
    append_model_info = FALSE,
    plot_subtitle = "My subtitle"
  )

  expect_equal(p$labels$subtitle, "My subtitle")
})
