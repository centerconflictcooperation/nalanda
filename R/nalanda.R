#' A Random Historical Fact About Nalanda University
#'
#' `nalanda()` returns a neutral, research-friendly fun fact about the
#' ancient Nalanda University. The goal is simply to provide a small,
#' informative piece of historical context with no evaluative or cultural
#' interpretation.
#'
#' @return A character string containing one factual statement about Nalanda.
#'
#' @examples
#' nalanda()
#'
#' @export
nalanda <- function() {
  facts <- c(
    "Nalanda University was founded in the 5th century CE in present-day Bihar, India.",
    "Nalanda was one of the world's first residential universities, hosting thousands of students and teachers.",
    "Excavations at Nalanda reveal an extensive campus with monasteries, temples, and lecture halls.",
    "At its height, Nalanda attracted scholars from many regions, including China, Korea, and Southeast Asia.",
    "The Nalanda library, known as Dharmaganja, was reputed to house hundreds of thousands of manuscripts.",
    "Xuanzang, the 7th-century Chinese monk and scholar, studied at Nalanda for several years and documented its curriculum.",
    "Nalanda remained an active center of learning for roughly 700 years until the 12th century."
  )

  sample(facts, 1)
}
