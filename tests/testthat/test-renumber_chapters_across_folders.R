test_that("renumber_chapters_across_folders renumbers files across ordered folders", {
  root <- withr::local_tempdir()
  part1 <- file.path(root, "part1")
  part2 <- file.path(root, "part2")
  dir.create(part1)
  dir.create(part2)

  writeLines("Intro", file.path(part1, "01_introduction.txt"))
  writeLines("AMPK", file.path(part1, "02_ampk.txt"))
  writeLines("Diet", file.path(part2, "01_diet.txt"))
  writeLines("Beverages", file.path(part2, "02_beverages.txt"))

  out <- renumber_chapters_across_folders(c(part1, part2))

  expect_equal(
    out$new_name,
    c("01_introduction.txt", "02_ampk.txt", "03_diet.txt", "04_beverages.txt")
  )
  expect_true(all(file.exists(out$new_path)))
  expect_false(file.exists(file.path(part2, "01_diet.txt")))
  expect_false(any(grepl("nalanda_tmp", list.files(part1, all.files = TRUE))))
  expect_false(any(grepl("nalanda_tmp", list.files(part2, all.files = TRUE))))
})

test_that("renumber_chapters_across_folders supports dry runs", {
  root <- withr::local_tempdir()
  part1 <- file.path(root, "part1")
  part2 <- file.path(root, "part2")
  dir.create(part1)
  dir.create(part2)

  file.create(file.path(part1, "01_a.txt"))
  file.create(file.path(part2, "01_b.txt"))

  out <- renumber_chapters_across_folders(c(part1, part2), dry_run = TRUE)

  expect_equal(out$new_name, c("01_a.txt", "02_b.txt"))
  expect_true(file.exists(file.path(part1, "01_a.txt")))
  expect_true(file.exists(file.path(part2, "01_b.txt")))
  expect_false(file.exists(file.path(part2, "02_b.txt")))
})

test_that("renumber_chapters_across_folders supports custom starts and widths", {
  root <- withr::local_tempdir()
  part <- file.path(root, "part")
  dir.create(part)

  file.create(file.path(part, "01_nuts.txt"))
  file.create(file.path(part, "02_greens.txt"))

  out <- renumber_chapters_across_folders(part, start = 14, width = 3)

  expect_equal(out$new_name, c("014_nuts.txt", "015_greens.txt"))
  expect_true(all(file.exists(out$new_path)))
})
