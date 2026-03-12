test_that("rank_weighted adds only weighted_score and keeps originals", {
  x <- tibble::tibble(
    book = c("A", "B", "C"),
    simulations_score = c(10, 20, 30),
    best_chapter_score = c(3, 2, 1)
  )

  out <- rank_weighted(
    x,
    weights = c(simulations_score = 0.7, best_chapter_score = 0.3),
    normalize = FALSE
  )

  expect_true("weighted_score" %in% names(out))
  expect_false(any(grepl("_weighted$", names(out))))
  expect_equal(out$simulations_score, c(30, 20, 10))
  expect_equal(out$best_chapter_score, c(1, 2, 3))
  expect_equal(out$weighted_score, sort(out$weighted_score, decreasing = TRUE))
})

test_that("rank_weighted normalizes internally without changing output vars", {
  x <- tibble::tibble(
    book = c("A", "B", "C"),
    v1 = c(1, 2, 3),
    v2 = c(3, 2, 1)
  )

  out <- rank_weighted(
    x,
    weights = c(v1 = 0.5, v2 = 0.5),
    normalize = TRUE,
    decreasing = FALSE
  )

  expect_equal(out$weighted_score, sort(out$weighted_score, decreasing = FALSE))
  expect_equal(out$v1, c(1, 2, 3))
  expect_equal(out$v2, c(3, 2, 1))
  expect_false(any(grepl("_weighted$", names(out))))
})

test_that("rank_weighted validates inputs", {
  x <- tibble::tibble(a = 1:3, b = 2:4)

  expect_error(
    rank_weighted(x, weights = c(0.4, 0.6)),
    "named numeric vector"
  )
  expect_error(
    rank_weighted(x, weights = c(a = 0.4, b = 0.5)),
    "Current sum is"
  )
  expect_error(
    rank_weighted(x, weights = c(a = 0.5, missing = 0.5)),
    "missing from `data`"
  )
})
