test_that("extract_pdf_text_with_llm validates pdf_path", {
  expect_error(
    extract_pdf_text_with_llm("missing-file.pdf"),
    "`pdf_path` does not exist",
    fixed = TRUE
  )

  tmp <- tempfile(fileext = ".txt")
  writeLines("not a pdf", tmp)
  expect_error(
    extract_pdf_text_with_llm(tmp),
    "`pdf_path` must point to a `.pdf` file.",
    fixed = TRUE
  )
})

test_that("extract_pdf_text_with_llm validates output_path for multi-file input", {
  expect_error(
    extract_pdf_text_with_llm(c("a.pdf", "b.pdf"), output_path = "out.txt"),
    "When `pdf_path` has length > 1 or is a list, `output_path` must be a directory-like path without a file extension.",
    fixed = TRUE
  )

  expect_error(
    extract_pdf_text_with_llm(list(book = c("a.pdf", "b.pdf")), output_path = "out.txt"),
    "When `pdf_path` has length > 1 or is a list, `output_path` must be a directory-like path without a file extension.",
    fixed = TRUE
  )
})

test_that("strip_pdf_preface_boilerplate removes repeated Gemini-style prefaces", {
  x <- paste0(
    "The following is the main body text from the PDF:\n\n",
    "The following is the main body text from the PDF:\n\n",
    "Actual chapter text."
  )

  expect_equal(
    strip_pdf_preface_boilerplate(x),
    "Actual chapter text."
  )
})
