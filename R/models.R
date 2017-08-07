#' SMB Models
#'
#' Creates an object inherting from class smb_models.
#'
#' @param ... Named objects.
#' @return An object inheriting from class smb_models.
#' @export
models <- function(...) {
  UseMethod("models")
}

#' @export
models.smb_model <- function(...) {

  x <- list(...)

  if (!is.list(x)) error("x must be a list")

  if (length(x)) {
    if (!all(purrr::map_lgl(x, is.smb_model)))
      error("all elements must inherit from 'smb_model'")

    class <- purrr::map(x, class)
    if (!identical(length(unique(class)), 1L))
      error("all model objects must have the same class")

  }
  class(x) <- "smb_models"
  x

}
