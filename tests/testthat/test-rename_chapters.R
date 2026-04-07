test_that("rename_chapters supports configurable extension", {
  root <- withr::local_tempdir()

  file.create(file.path(root, "jerks_Part10.pdf"))
  file.create(file.path(root, "jerks_Part2.pdf"))
  file.create(file.path(root, "intro.pdf"))

  out <- rename_chapters(root, extension = "pdf")

  expect_equal(
    out$new_name,
    c("chapter1.pdf", "chapter2.pdf", "chapter3.pdf")
  )
  expect_true(all(file.exists(out$new_path)))
})

test_that("rename_chapters avoids in-place rename collisions", {
  root <- withr::local_tempdir()

  file.create(file.path(root, "chapter11.pdf"))
  file.create(file.path(root, "chapter2.pdf"))

  out <- rename_chapters(root, extension = "pdf")

  expect_equal(out$new_name, c("chapter1.pdf", "chapter2.pdf"))
  expect_true(all(file.exists(out$new_path)))
  expect_false(any(grepl("nalanda_tmp", list.files(root))))
})
