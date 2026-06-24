test_that("split_book_section_by_headings splits on known chapter titles", {
  root <- withr::local_tempdir()
  input <- file.path(root, "3_part1.txt")
  output <- file.path(root, "chapters")

  writeLines(
    c(
      "I. Slowing Eleven",
      "Pathways of Aging",
      "",
      "INTRODUCTION",
      "",
      "Intro chapter text.",
      "",
      "HOW IMPORTANT ARE YOUR GENES?",
      "",
      "This uppercase subsection should stay inside the introduction.",
      "",
      "AMPK",
      "",
      "AMPK chapter text.",
      "",
      "AUTOPHAGY",
      "",
      "Autophagy chapter text."
    ),
    input
  )

  out <- split_book_section_by_headings(
    input,
    chapter_titles = c("Introduction", "AMPK", "Autophagy"),
    output_dir = output
  )

  expect_equal(
    out$output_file,
    c("01_introduction.txt", "02_ampk.txt", "03_autophagy.txt")
  )
  expect_equal(out$heading_line, c(4L, 12L, 16L))
  expect_true(any(grepl("HOW IMPORTANT ARE YOUR GENES", readLines(file.path(output, "01_introduction.txt")))))
  expect_false(file.exists(file.path(output, "04_how_important_are_your_genes.txt")))
})

test_that("split_book_section_by_headings supports custom chapter ids", {
  root <- withr::local_tempdir()
  input <- file.path(root, "6_part4.txt")

  writeLines(
    c(
      "IV. DR. GREGER'S ANTI-AGING EIGHT",
      "",
      "NUTS",
      "Nut text.",
      "",
      "NAD+",
      "NAD text."
    ),
    input
  )

  out <- split_book_section_by_headings(
    input,
    chapter_titles = c("Nuts", "NAD+"),
    chapter_ids = c("01_nuts", "02_nad"),
    output_dir = file.path(root, "out")
  )

  expect_equal(out$output_file, c("01_nuts.txt", "02_nad.txt"))
  expect_equal(readLines(out$output_path[[2]]), c("NAD+", "NAD text."))
})

test_that("split_book_section_by_headings reports missing headings", {
  root <- withr::local_tempdir()
  input <- file.path(root, "4_part2.txt")
  writeLines(c("DIET", "Diet text."), input)

  expect_error(
    split_book_section_by_headings(input, c("Diet", "Exercise")),
    "Could not find heading(s): Exercise",
    fixed = TRUE
  )
})
