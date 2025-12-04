#' Fix text file encoding and normalize punctuation/whitespace
#'
#' Read a file as raw bytes, drop NUL characters, guess or use a provided
#' source encoding, convert to UTF-8 and normalize common punctuation and newlines.
#'
#' @param path Character scalar. Path to the text file.
#' @param from Optional character. Source encoding to feed to iconv. If NULL the function
#'   will try to guess and fall back to WINDOWS-1252.
#' @return A character scalar with cleaned text (UTF-8).
#' @export
fix_text_file <- function(path, from = NULL) {
  # 1) Read as raw and strip NULs (avoids "nul character not allowed")
  raw <- readBin(path, what = "raw", n = file.info(path)$size)
  raw <- raw[raw != as.raw(0x00)] # drop NULs
  s <- rawToChar(raw) # bytes -> native encoding string (no NULs now)

  # 2) Pick a source encoding
  if (is.null(from)) {
    # try to guess, else fall back to Windows-1252 then Latin-1
    enc_guess <- tryCatch(
      readr::guess_encoding(raw, n_max = 1e5)$encoding[1],
      error = function(e) NA
    )
    from <- if (!is.na(enc_guess)) enc_guess else "WINDOWS-1252"
  }

  # 3) Convert to UTF-8
  s <- iconv(s, from = from, to = "UTF-8", sub = "")

  # 4) Normalize whitespace/newlines
  s <- gsub("\r\n?", "\n", s, perl = TRUE) # CRLF/LF -> LF
  s <- gsub("\u00A0", " ", s, perl = TRUE) # NBSP -> space
  s <- gsub("[ \t]{2,}", " ", s, perl = TRUE) # collapse runs of spaces/tabs

  # 5) Normalize punctuation (smart quotes/dashes/ellipsis -> ASCII)
  s <- gsub("\u201C|\u201D", '"', s, perl = TRUE) # “ ”
  s <- gsub("\u2018|\u2019", "'", s, perl = TRUE) # ‘ ’
  s <- gsub("\u2013|\u2014", "-", s, perl = TRUE) # – —
  s <- gsub("\u2026", "...", s, perl = TRUE) # …
  s
}
