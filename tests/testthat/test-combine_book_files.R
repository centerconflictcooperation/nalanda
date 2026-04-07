test_that("combine_book_files groups chapter files by title stem", {
  input_dir <- file.path(tempdir(), "combine-book-files-input")
  output_dir <- file.path(tempdir(), "combine-book-files-output")

  unlink(input_dir, recursive = TRUE, force = TRUE)
  unlink(output_dir, recursive = TRUE, force = TRUE)
  dir.create(input_dir, recursive = TRUE)

  writeLines("How Can You - Chapter 1", file.path(input_dir, "1_howcanyou.txt"))
  writeLines("How Can You - Chapter 2", file.path(input_dir, "2_howcanyou.txt"))
  writeLines("Dragon Tales - Chapter 6", file.path(input_dir, "6_dragontales.txt"))
  writeLines("Dragon Tales - Chapter 7", file.path(input_dir, "7_dragontales.txt"))

  out <- nalanda:::combine_book_files(
    input_dir = input_dir,
    output_dir = output_dir
  )

  expect_equal(out$book_slug, c("howcanyou", "dragontales"))
  expect_equal(out$book_id, c(1L, 2L))
  expect_equal(out$n_chapters, c(2L, 2L))
  expect_equal(
    readLines(file.path(output_dir, "1_howcanyou.txt")),
    c("How Can You - Chapter 1", "", "How Can You - Chapter 2")
  )
  expect_equal(
    readLines(file.path(output_dir, "2_dragontales.txt")),
    c("Dragon Tales - Chapter 6", "", "Dragon Tales - Chapter 7")
  )
  expect_false(file.exists(file.path(output_dir, "book_key.csv")))
})

test_that("combine_book_files errors on unexpected file names", {
  input_dir <- file.path(tempdir(), "combine-book-files-bad-input")

  unlink(input_dir, recursive = TRUE, force = TRUE)
  dir.create(input_dir, recursive = TRUE)

  writeLines("Oops", file.path(input_dir, "howcanyou.txt"))

  expect_error(
    nalanda:::combine_book_files(input_dir = input_dir),
    "do not match the expected `number_title.txt` format"
  )
})
