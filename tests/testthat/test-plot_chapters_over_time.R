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

  expect_null(p$labels$title)
  expect_equal(p$labels$subtitle, "(model = \"gpt-test\"; temperature = 0.2)")
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
    "My subtitle (model = \"gpt-test\"; temperature = 0.2)"
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

test_that("plot_chapters_over_time strips integration from stored model names", {
  skip_if_not_installed("Rmisc")

  chapters <- tibble::tibble(
    book = c("Book A", "Book A", "Book A", "Book A"),
    chapter = c("Chapter 1", "Chapter 2", "Chapter 1", "Chapter 2"),
    sim = c(1, 1, 2, 2),
    delta_gap = c(1, 2, 1.5, 2.5)
  )
  attr(chapters, "model") <- "@gemini-8c2498/gemini-2.5-flash-lite"
  attr(chapters, "temperature") <- 0

  p <- plot_chapters_over_time(chapters)

  expect_equal(
    p$labels$subtitle,
    "(model = \"gemini-2.5-flash-lite\"; temperature = 0)"
  )
})

test_that("plot_chapters_over_time accepts a manual title", {
  skip_if_not_installed("Rmisc")

  chapters <- tibble::tibble(
    book = c("Book A", "Book A", "Book A", "Book A"),
    chapter = c("Chapter 1", "Chapter 2", "Chapter 1", "Chapter 2"),
    sim = c(1, 1, 2, 2),
    delta_gap = c(1, 2, 1.5, 2.5)
  )

  p <- plot_chapters_over_time(chapters, plot_title = "Custom title")

  expect_equal(p$labels$title, "Custom title")
})

test_that("plot_chapters_over_time replaces points with images when point_images is provided", {
  skip_if_not_installed("Rmisc")
  skip_if_not_installed("ggimage")
  skip_if_not_installed("ggtext")

  chapters <- tibble::tibble(
    book = rep("Book A", 8),
    chapter = rep(c("Chapter 1", "Chapter 2"), 4),
    party = c(
      "Democrat",
      "Democrat",
      "Democrat",
      "Democrat",
      "Republican",
      "Republican",
      "Republican",
      "Republican"
    ),
    sim = c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L),
    delta_gap = c(-5, -10, -3, -8, -15, -20, -12, -18)
  )

  # Create minimal valid PNG files (1x1 pixel) from raw bytes
  png_bytes <- as.raw(c(
    0x89,
    0x50,
    0x4e,
    0x47,
    0x0d,
    0x0a,
    0x1a,
    0x0a,
    0x00,
    0x00,
    0x00,
    0x0d,
    0x49,
    0x48,
    0x44,
    0x52,
    0x00,
    0x00,
    0x00,
    0x01,
    0x00,
    0x00,
    0x00,
    0x01,
    0x08,
    0x02,
    0x00,
    0x00,
    0x00,
    0x90,
    0x77,
    0x53,
    0xde,
    0x00,
    0x00,
    0x00,
    0x0c,
    0x49,
    0x44,
    0x41,
    0x54,
    0x08,
    0xd7,
    0x63,
    0xf8,
    0xcf,
    0xc0,
    0x00,
    0x00,
    0x00,
    0x02,
    0x00,
    0x01,
    0xe2,
    0x21,
    0xbc,
    0x33,
    0x00,
    0x00,
    0x00,
    0x00,
    0x49,
    0x45,
    0x4e,
    0x44,
    0xae,
    0x42,
    0x60,
    0x82
  ))
  tmp_dem <- tempfile(fileext = ".png")
  tmp_rep <- tempfile(fileext = ".png")
  writeBin(png_bytes, tmp_dem)
  writeBin(png_bytes, tmp_rep)

  p <- plot_chapters_over_time(
    chapters,
    group = "party",
    point_images = list(Democrat = tmp_dem, Republican = tmp_rep),
    image_size = 0.04
  )

  # Should have a GeomImage layer
  has_image <- any(vapply(
    p$layers,
    function(l) {
      inherits(l$geom, "GeomImage")
    },
    logical(1)
  ))
  expect_true(has_image)

  # Should NOT have a GeomPoint layer (replaced)
  has_point <- any(vapply(
    p$layers,
    function(l) {
      inherits(l$geom, "GeomPoint")
    },
    logical(1)
  ))
  expect_false(has_point)

  colour_scale <- p$scales$get_scales("colour")
  expect_true(all(grepl(
    "<img src='",
    unname(colour_scale$labels),
    fixed = TRUE
  )))
  expect_true(all(grepl("Democrat|Republican", unname(colour_scale$labels))))
  expect_s3_class(p$theme$legend.text, "element_markdown")

  unlink(c(tmp_dem, tmp_rep))
})

test_that("plot_chapters_over_time supports image nudging", {
  skip_if_not_installed("Rmisc")
  skip_if_not_installed("ggimage")

  chapters <- tibble::tibble(
    book = rep("Book A", 8),
    chapter = rep(c("Chapter 1", "Chapter 2"), 4),
    party = c(
      "Democrat",
      "Democrat",
      "Democrat",
      "Democrat",
      "Republican",
      "Republican",
      "Republican",
      "Republican"
    ),
    sim = c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L),
    delta_gap = c(-5, -10, -3, -8, -15, -20, -12, -18)
  )

  png_bytes <- as.raw(c(
    0x89,
    0x50,
    0x4e,
    0x47,
    0x0d,
    0x0a,
    0x1a,
    0x0a,
    0x00,
    0x00,
    0x00,
    0x0d,
    0x49,
    0x48,
    0x44,
    0x52,
    0x00,
    0x00,
    0x00,
    0x01,
    0x00,
    0x00,
    0x00,
    0x01,
    0x08,
    0x02,
    0x00,
    0x00,
    0x00,
    0x90,
    0x77,
    0x53,
    0xde,
    0x00,
    0x00,
    0x00,
    0x0c,
    0x49,
    0x44,
    0x41,
    0x54,
    0x08,
    0xd7,
    0x63,
    0xf8,
    0xcf,
    0xc0,
    0x00,
    0x00,
    0x00,
    0x02,
    0x00,
    0x01,
    0xe2,
    0x21,
    0xbc,
    0x33,
    0x00,
    0x00,
    0x00,
    0x00,
    0x49,
    0x45,
    0x4e,
    0x44,
    0xae,
    0x42,
    0x60,
    0x82
  ))
  tmp_dem <- tempfile(fileext = ".png")
  tmp_rep <- tempfile(fileext = ".png")
  writeBin(png_bytes, tmp_dem)
  writeBin(png_bytes, tmp_rep)

  p <- plot_chapters_over_time(
    chapters,
    group = "party",
    point_images = list(Democrat = tmp_dem, Republican = tmp_rep),
    image_nudge_x = 0.2,
    image_nudge_y = 1.5
  )

  image_layer <- p$layers[[length(p$layers)]]
  expect_true(all(
    image_layer$data$x_image == as.numeric(image_layer$data$Time) + 0.2
  ))
  expect_true(all(image_layer$data$y_image == image_layer$data$value + 1.5))

  unlink(c(tmp_dem, tmp_rep))
})

test_that("plot_chapters_over_time keeps points when point_images is NULL", {
  skip_if_not_installed("Rmisc")

  chapters <- tibble::tibble(
    book = c("Book A", "Book A", "Book A", "Book A"),
    chapter = c("Chapter 1", "Chapter 2", "Chapter 1", "Chapter 2"),
    sim = c(1, 1, 2, 2),
    delta_gap = c(1, 2, 1.5, 2.5)
  )

  p <- plot_chapters_over_time(chapters)

  has_point <- any(vapply(
    p$layers,
    function(l) {
      inherits(l$geom, "GeomPoint")
    },
    logical(1)
  ))
  expect_true(has_point)
})

test_that("plot_chapters_over_time passes facet_ncol to facet_wrap", {
  skip_if_not_installed("Rmisc")

  chapters <- tibble::tibble(
    book = rep(c("Book A", "Book B", "Book C"), each = 4),
    chapter = rep(c("Chapter 1", "Chapter 2"), 6),
    sim = rep(c(1, 1, 2, 2), 3),
    delta_gap = c(1, 2, 1.5, 2.5, 3, 4, 3.5, 4.5, 5, 6, 5.5, 6.5)
  )

  p <- plot_chapters_over_time(
    chapters,
    group = "book",
    facet = "book",
    facet_ncol = 2
  )

  expect_equal(p$facet$params$ncol, 2)
})

test_that("plot_chapters_over_time accepts list inputs from compute_run_ai_metrics", {
  skip_if_not_installed("Rmisc")

  raw_a <- tibble::tibble(
    book = c("Book A", "Book A", "Book A", "Book A"),
    chapter = c("Chapter 1", "Chapter 1", "Chapter 2", "Chapter 2"),
    sim = c(1, 1, 1, 1),
    identity = c("Democrat", "Democrat", "Democrat", "Democrat"),
    turn_type = c("baseline", "post", "baseline", "post"),
    target_group = c(NA_character_, NA_character_, NA_character_, NA_character_),
    rating = c(40, 45, 42, 47)
  )
  raw_b <- tibble::tibble(
    book = c("Book A", "Book A", "Book A", "Book A"),
    chapter = c("Chapter 1", "Chapter 1", "Chapter 2", "Chapter 2"),
    sim = c(2, 2, 2, 2),
    identity = c("Democrat", "Democrat", "Democrat", "Democrat"),
    turn_type = c("baseline", "post", "baseline", "post"),
    target_group = c(NA_character_, NA_character_, NA_character_, NA_character_),
    rating = c(41, 46, 43, 48)
  )
  attr(raw_a, "model") <- "gpt-test"
  attr(raw_a, "temperature") <- 0.2
  attr(raw_a, "n_simulations") <- 2

  chapters <- compute_run_ai_metrics(list(raw_a, raw_b))

  p <- plot_chapters_over_time(chapters)

  expect_s3_class(p, "ggplot")
  expect_equal(attr(chapters, "n_simulations"), 2)
  expect_equal(p$labels$subtitle, "(model = \"gpt-test\"; temperature = 0.2)")
})

test_that("plot_chapters_over_time errors on ambiguous chapter numbering", {
  skip_if_not_installed("Rmisc")

  chapters <- tibble::tibble(
    book = c("Book A", "Book A", "Book A", "Book A"),
    chapter = c(
      "4_Chapter_4.txt",
      "4_Chapter_4.txt",
      "4_Chapter3.txt",
      "4_Chapter3.txt"
    ),
    sim = c(1, 1, 1, 1),
    identity = c("Democrat", "Republican", "Democrat", "Republican"),
    party = c("Democrat", "Republican", "Democrat", "Republican"),
    delta_outgroup = c(-5, -5, -7, -7)
  )

  expect_error(
    plot_chapters_over_time(
      chapters,
      dv = "delta_outgroup",
      reverse_score = TRUE
    ),
    "Duplicate chapters identified while parsing"
  )
})

test_that("plot_chapters_over_time_one_turn computes metrics from raw output", {
  skip_if_not_installed("Rmisc")

  chapters <- tibble::tibble(
    chapter = c("chapter_1", "chapter_1", "chapter_2", "chapter_2"),
    sim = c(1, 1, 1, 1),
    identity = c("Democrat", "Democrat", "Democrat", "Democrat"),
    party = c("Democrat", "Democrat", "Democrat", "Democrat"),
    target_group = c("Democrat", "Republican", "Democrat", "Republican"),
    rating = c(70, 45, 68, 50)
  )
  attr(chapters, "model") <- "gpt-test"
  attr(chapters, "temperature") <- 0.2

  p <- plot_chapters_over_time_one_turn(chapters, group = "party")

  expect_s3_class(p, "ggplot")
  expect_null(p$labels$title)
  expect_equal(p$labels$subtitle, "(model = \"gpt-test\"; temperature = 0.2)")
})
