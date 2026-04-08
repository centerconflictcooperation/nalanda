# Extract text from a PDF with a multimodal LLM

This helper uploads a local PDF to a multimodal model through `ellmer`
and asks the model to return clean running text. It is useful when
ordinary OCR struggles with stamps, overlays, or poor scan quality but
the target model can read PDFs directly.

## Usage

``` r
extract_pdf_text_with_llm(
  pdf_path,
  prompt = paste("Transcribe the main body text from this PDF as plain UTF-8 text.",
    "Keep the wording faithful to the source.",
    "Ignore repeated stamps, watermarks, page numbers, headers, footers,",
    "and other obvious non-book overlays when they are not part of the book.",
    "Preserve paragraph breaks.", "Return only the extracted text.",
    "Do not add any introduction, explanation, summary, XML, markdown fences,",
    "or labels such as 'The following is the main body text from the PDF:'.", sep = " "),
  model = "gpt-5-mini",
  integration = getOption("nalanda.integration"),
  virtual_key = getOption("nalanda.virtual_key"),
  base_url = getOption("nalanda.base_url"),
  temperature = 1,
  seed = 42,
  output_path = NULL,
  timeout_s = getOption("ellmer_timeout_s", 120),
  max_tries = getOption("ellmer_max_tries", 5),
  retry_wait = 3,
  overwrite = FALSE
)
```

## Arguments

- pdf_path:

  Character scalar path to a local PDF file, a character vector of PDF
  paths, or a named/nested list of PDF paths such as the output of
  `list_book_chapters(extension = "pdf")`.

- prompt:

  Character scalar instruction shown alongside the PDF. The default asks
  for faithful transcription while ignoring obvious non-book overlays
  such as repeated stamps, page numbers, and headers/footers.

- model:

  Character. Model name for the chat backend.

- integration:

  Optional Portkey/gateway route slug. If supplied and `model` is not
  fully-qualified, nalanda will build `"@{integration}/{model}"`.

- virtual_key:

  Optional legacy virtual key. If supplied and `model` is not
  fully-qualified, nalanda will build `"@{virtual_key}/{model}"`.

- base_url:

  Character. Base URL for API calls.

- temperature:

  Numeric. Sampling temperature passed to the backend.

- seed:

  Integer. Random seed for reproducibility.

- output_path:

  Optional output target. For a single PDF, this may be either an exact
  `.txt` file path or a directory path. For a character vector or nested
  list of PDFs, supply a directory-like path without a file extension;
  nalanda will write one `.txt` per PDF incrementally, preserving
  partial progress if a later file fails.

- timeout_s:

  Numeric scalar request timeout in seconds. Applied via
  `options(ellmer_timeout_s = ...)` for the duration of the call.

- max_tries:

  Integer scalar total number of request attempts. Applied via
  `options(ellmer_max_tries = ...)` for the duration of the call.

- retry_wait:

  Numeric scalar seconds to wait between manual retries after a failed
  single-file attempt.

- overwrite:

  Logical scalar. If `TRUE`, replace existing output files at
  `output_path`. Defaults to `FALSE`.

## Value

If `pdf_path` is a single file, a character scalar containing the
extracted text. If `pdf_path` is a character vector or nested list,
returns text with the same structure and names as the input. If
`output_path` is supplied, text files are also written to disk.

## Details

In testing through the NYU Portkey/gateway path, PDF extraction was more
reliable with `gpt-5-mini` than with Gemini routes. Gemini-family models
may still work in other environments, but PDF handling through
`chat_portkey()` was inconsistent in our tests.
