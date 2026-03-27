# TODO

## Package split

- Evaluate splitting the current package into:
  - `simulum`: a generic core package for AI-mediated simulations, prompt-first treatment workflows, text annotation, and generic summarization/evaluation helpers.
  - `nalanda`: a project-specific package for the book workflow, chapter/file helpers, chapter metrics, and book-focused plotting.
- Make `simulum` the engine package and have `nalanda` import it later.
- Move shared internals into `simulum` first, especially the simulation pipeline, model-routing helpers, prompt interpolation, and result-finalization code.
- Keep book-specific wrappers and plotting in `nalanda` unless they become reusable outside the book project.

## Naming check for `simulum`

- Quick check on 2026-03-26: no current evidence found of a CRAN or Bioconductor package named `simulum`.
- A final package-name availability check should still be done immediately before publishing.
- There appears to be unrelated older software/projects using the name `Simulum`, so collision risk outside CRAN is low but not zero.

## Next steps

- Classify all exported functions into `generic simulation`, `book-specific`, and `shared core`.
- Identify which internal helpers can move to `simulum` without dragging book-specific assumptions with them.
- Draft the minimal first release scope for `simulum` so the split does not export too much too early.
