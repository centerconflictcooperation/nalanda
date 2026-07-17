test_that("interpolate_spotify_audiobook_duration estimates from file size", {
  reference <- tibble::tibble(
    book = c("known_a", "known_b"),
    file_size_bytes = c(100, 200),
    spotify_duration_minutes = c(10, 20)
  )
  target <- tibble::tibble(
    chapter = c("chapter_1", "chapter_2"),
    file_size_bytes = c(50, 150)
  )

  out <- interpolate_spotify_audiobook_duration(
    reference,
    target,
    duration_col = "spotify_duration_minutes",
    size_col = "file_size_bytes",
    duration_unit = "minutes",
    output_unit = "minutes"
  )

  expect_equal(out$estimated_duration_minutes, c(5, 15))
  expect_equal(out$.duration_seconds, c(300, 900))
  expect_equal(attr(out, "estimated_total_minutes"), 20)
  expect_equal(out$.duration_measure, rep("file_size", 2))
  expect_equal(out$.duration_method, rep("ratio", 2))
})

test_that("interpolate_spotify_audiobook_duration can measure text files", {
  root <- withr::local_tempdir()
  known_file <- file.path(root, "known.txt")
  target_file <- file.path(root, "target.txt")

  writeBin(charToRaw("abcd"), known_file)
  writeBin(charToRaw("abcdefghij"), target_file)

  reference <- tibble::tibble(
    file = known_file,
    spotify_duration_seconds = 40
  )
  target <- tibble::tibble(
    chapter = "chapter_1",
    file = target_file
  )

  out <- interpolate_spotify_audiobook_duration(
    reference,
    target,
    duration_col = "spotify_duration_seconds",
    file_col = "file",
    output_unit = "seconds"
  )

  expect_equal(out$estimated_duration_seconds, 100)
})

test_that("interpolate_spotify_audiobook_duration aggregates chapter reference rows by book", {
  reference <- tibble::tibble(
    book = c("known_a", "known_a", "known_b", "known_b"),
    chapter = c("a1", "a2", "b1", "b2"),
    file_size_bytes = c(50, 50, 100, 100),
    spotify_duration_minutes = c(10, 10, 20, 20)
  )
  target <- tibble::tibble(
    chapter = c("chapter_1", "chapter_2"),
    file_size_bytes = c(25, 75)
  )

  out <- interpolate_spotify_audiobook_duration(
    reference,
    target,
    duration_col = "spotify_duration_minutes",
    reference_book_col = "book",
    size_col = "file_size_bytes",
    duration_unit = "minutes",
    output_unit = "minutes"
  )

  expect_equal(out$estimated_duration_minutes, c(2.5, 7.5))
  expect_equal(attr(out, "estimated_total_minutes"), 10)
})

test_that("interpolate_spotify_audiobook_duration builds chapter inputs from book folders", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "known_a"))
  dir.create(file.path(root, "known_b"))
  dir.create(file.path(root, "missing_book"))

  writeBin(charToRaw("aaaaa"), file.path(root, "known_a", "chapter1.txt"))
  writeBin(charToRaw("aaaaa"), file.path(root, "known_a", "chapter2.txt"))
  writeBin(charToRaw("bbbbbbbbbbbbbbbbbbbb"), file.path(root, "known_b", "chapter1.txt"))
  writeBin(charToRaw("ccccc"), file.path(root, "missing_book", "chapter1.txt"))

  rubric <- tibble::tibble(
    book = c("known_a", "known_b", "missing_book"),
    spotify_audiobook_duration = c("0:10:00", "0:20:00", "")
  )

  out <- interpolate_spotify_audiobook_duration(
    reference = rubric,
    books_path = root,
    target_book = "missing_book",
    duration_col = "spotify_audiobook_duration",
    duration_unit = "hms",
    output_unit = "minutes"
  )

  expect_equal(out$book, "missing_book")
  expect_equal(out$estimated_duration_minutes, 5)
  expect_equal(attr(out, "estimated_total_minutes"), 5)
})

test_that("interpolate_spotify_audiobook_duration can format estimates as hms", {
  reference <- tibble::tibble(
    book = "known",
    file_size_bytes = 100,
    spotify_duration = "1:30:00"
  )
  target <- tibble::tibble(
    chapter = c("chapter_1", "chapter_2"),
    file_size_bytes = c(50, 150)
  )

  out <- interpolate_spotify_audiobook_duration(
    reference,
    target,
    duration_col = "spotify_duration",
    size_col = "file_size_bytes",
    duration_unit = "hms",
    output_unit = "hms"
  )

  expect_equal(out$estimated_duration_hms, c("0:45:00", "2:15:00"))
  expect_equal(attr(out, "estimated_total_hms"), "3:00:00")
})

test_that("interpolate_spotify_audiobook_duration supports word counts", {
  reference <- tibble::tibble(
    book = "known",
    words = 100,
    duration_hours = 2
  )
  target <- tibble::tibble(
    chapter = c("chapter_1", "chapter_2"),
    words = c(25, 75)
  )

  out <- interpolate_spotify_audiobook_duration(
    reference,
    target,
    duration_col = "duration_hours",
    words_col = "words",
    measure = "word_count",
    duration_unit = "hours",
    output_unit = "minutes"
  )

  expect_equal(out$estimated_duration_minutes, c(30, 90))
  expect_equal(attr(out, "estimated_total_minutes"), 120)
})

test_that("interpolate_spotify_audiobook_duration repairs text before counting words", {
  root <- withr::local_tempdir()
  known_file <- file.path(root, "known.txt")
  target_file <- file.path(root, "target.txt")

  writeBin(charToRaw("one two three four"), known_file)
  writeBin(c(
    charToRaw("one two"),
    as.raw(0x92),
    charToRaw("three four")
  ), target_file)

  reference <- tibble::tibble(
    file = known_file,
    spotify_duration_seconds = 40
  )
  target <- tibble::tibble(
    chapter = "chapter_1",
    file = target_file
  )

  expect_no_warning(
    out <- interpolate_spotify_audiobook_duration(
      reference,
      target,
      duration_col = "spotify_duration_seconds",
      file_col = "file",
      measure = "word_count",
      output_unit = "seconds"
    )
  )

  expect_equal(out$estimated_duration_seconds, 40)
})

test_that("interpolate_spotify_audiobook_duration validates required columns", {
  expect_error(
    interpolate_spotify_audiobook_duration(
      reference = tibble::tibble(size = 100, duration = 60),
      target = tibble::tibble(size = 50),
      duration_col = "duration",
      size_col = "missing"
    ),
    "not present"
  )
})
