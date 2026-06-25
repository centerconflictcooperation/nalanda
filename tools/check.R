clean_codex_checkpoints <- function(path = ".") {
  root <- normalizePath(path, winslash = "/", mustWork = TRUE)
  git_dir <- file.path(root, ".git")
  if (!dir.exists(git_dir)) {
    return(invisible(FALSE))
  }

  target <- file.path(git_dir, "refs", "codex", "turn-diffs", "checkpoints")
  if (!dir.exists(target)) {
    return(invisible(FALSE))
  }

  target_path <- normalizePath(target, winslash = "/", mustWork = TRUE)
  git_path <- normalizePath(git_dir, winslash = "/", mustWork = TRUE)
  if (!startsWith(target_path, paste0(git_path, "/"))) {
    stop("Refusing to remove path outside `.git`: ", target_path, call. = FALSE)
  }

  unlink(target_path, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

clean_codex_checkpoints()

rstudio_pandoc <- "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools"
if (!nzchar(Sys.getenv("RSTUDIO_PANDOC")) && file.exists(file.path(rstudio_pandoc, "pandoc.exe"))) {
  Sys.setenv(RSTUDIO_PANDOC = rstudio_pandoc)
}

devtools::check(check_dir = dirname(getwd()), error_on = "never")
