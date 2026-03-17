# Normalize model metadata for storage/display. Fully-qualified Portkey model
# strings should be reduced to the actual model name in nalanda attributes.
normalize_model_name <- function(model) {
  if (is.null(model)) {
    return(NULL)
  }

  model <- as.character(model)[1]
  if (!nzchar(model)) {
    return(model)
  }

  sub("^@?[^/]+/", "", model)
}
