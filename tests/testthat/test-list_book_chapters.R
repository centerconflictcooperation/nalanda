test_that("list_book_chapters supports configurable extension", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "book_a"))
  dir.create(file.path(root, "book_b"))

  file.create(file.path(root, "book_a", "2_chapter.pdf"))
  file.create(file.path(root, "book_a", "1_chapter.pdf"))
  file.create(file.path(root, "book_b", "chapter_a.pdf"))
  file.create(file.path(root, "book_b", "chapter_b.txt"))

  out <- list_book_chapters(root, extension = "pdf")

  expect_named(out, c("book_a", "book_b"))
  expect_equal(
    basename(out$book_a),
    c("1_chapter.pdf", "2_chapter.pdf")
  )
  expect_equal(
    basename(out$book_b),
    "chapter_a.pdf"
  )
})

test_that("list_book_chapters supports a flat folder of chapter files", {
  root <- withr::local_tempdir()

  file.create(file.path(root, "jerks_Part10.pdf"))
  file.create(file.path(root, "jerks_Part2.pdf"))
  file.create(file.path(root, "jerks_Part3.pdf"))
  file.create(file.path(root, "ignore_me.txt"))

  out <- list_book_chapters(root, extension = "pdf")

  expect_named(out, basename(root))
  expect_equal(
    basename(out[[1]]),
    c("jerks_Part2.pdf", "jerks_Part3.pdf", "jerks_Part10.pdf")
  )
})
