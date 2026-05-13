test_that("combine_split_chapter_files combines ranged chunks", {
  root <- withr::local_tempdir()

  writeLines("Preface", file.path(root, "1_preface.txt"))
  writeLines("Part 1 A", file.path(root, "3_part1-001-050.txt"))
  writeLines("Part 1 B", file.path(root, "3_part1-051-100.txt"))
  writeLines("Part 1 C", file.path(root, "3_part1-101-138.txt"))
  writeLines("Part 2 A", file.path(root, "4_part2-001-050.txt"))

  out <- combine_split_chapter_files(root)

  expect_equal(
    out$output_file,
    c("1_preface.txt", "3_part1.txt", "4_part2.txt")
  )
  expect_equal(out$action, c("unchanged", "combined", "combined"))
  expect_equal(
    readLines(file.path(root, "3_part1.txt")),
    c("Part 1 A", "", "Part 1 B", "", "Part 1 C")
  )
  expect_equal(readLines(file.path(root, "4_part2.txt")), "Part 2 A")
})

test_that("combine_split_chapter_files can write to a separate output folder", {
  root <- withr::local_tempdir()
  output <- file.path(root, "combined")

  writeLines("Intro", file.path(root, "2_introduction.txt"))
  writeLines("Part 3 A", file.path(root, "5_part3-051-100.txt"))
  writeLines("Part 3 B", file.path(root, "5_part3-001-050.txt"))

  out <- combine_split_chapter_files(root, output_dir = output)

  expect_equal(out$output_file, c("2_introduction.txt", "5_part3.txt"))
  expect_equal(out$action, c("copied", "combined"))
  expect_equal(readLines(file.path(output, "2_introduction.txt")), "Intro")
  expect_equal(
    readLines(file.path(output, "5_part3.txt")),
    c("Part 3 B", "", "Part 3 A")
  )
})

test_that("combine_split_chapter_files protects existing outputs", {
  root <- withr::local_tempdir()

  writeLines("Existing", file.path(root, "3_part1.txt"))
  writeLines("Part 1 A", file.path(root, "3_part1-001-050.txt"))

  expect_error(
    combine_split_chapter_files(root),
    "Output file(s) already exist. Set `overwrite = TRUE` to replace them: 3_part1.txt",
    fixed = TRUE
  )
})

test_that("combine_split_chapter_files overwrites from chunks only", {
  root <- withr::local_tempdir()

  writeLines("Existing consolidated text", file.path(root, "3_part1.txt"))
  writeLines("Part 1 A", file.path(root, "3_part1-001-050.txt"))
  writeLines("Part 1 B", file.path(root, "3_part1-051-100.txt"))

  out <- combine_split_chapter_files(root, overwrite = TRUE)

  expect_equal(out$output_file, "3_part1.txt")
  expect_equal(out$source_files, "3_part1-001-050.txt, 3_part1-051-100.txt")
  expect_equal(readLines(file.path(root, "3_part1.txt")), c("Part 1 A", "", "Part 1 B"))
})
