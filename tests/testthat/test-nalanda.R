test_that("nalanda() returns a character string", {
  result <- nalanda()
  expect_type(result, "character")
  expect_length(result, 1)
})

test_that("nalanda() returns one of the expected facts", {
  # Get all possible facts from the function
  expected_facts <- c(
    "Nalanda University was founded in the 5th century CE in present-day Bihar, India.",
    "Nalanda was one of the world's first residential universities, hosting thousands of students and teachers.",
    "Excavations at Nalanda reveal an extensive campus with monasteries, temples, and lecture halls.",
    "At its height, Nalanda attracted scholars from many regions, including China, Korea, and Southeast Asia.",
    "The Nalanda library, known as Dharmaganja, was reputed to house hundreds of thousands of manuscripts.",
    "Xuanzang, the 7th-century Chinese monk and scholar, studied at Nalanda for several years and documented its curriculum.",
    "Nalanda remained an active center of learning for roughly 700 years until the 12th century."
  )
  
  result <- nalanda()
  expect_true(result %in% expected_facts)
})

test_that("nalanda() returns different facts when called multiple times", {
  # Set seed for reproducibility but expect some variation
  set.seed(123)
  results <- replicate(50, nalanda())
  
  # Should have more than one unique fact in 50 calls
  expect_gt(length(unique(results)), 1)
})
