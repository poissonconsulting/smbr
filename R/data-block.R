#' Create Data Block from Data List
#'
#' Automate writing the STAN data block from your data.
#'
#' @param x A nlist
#'
#' @return A string
#' @export
#'
#' @examples
#' mod_data <- nlist::as_nlist(list(
#'   X = c(1L, 2L, 3L, 4L),
#'   Y = c(1.2, 7.3, 8.9, 2.6),
#'   nObs = 4L
#' ))
#' data_block(mod_data)
data_block <- function(x) {
  nlist::chk_nlist(x)
  nobs <- get_nobs(x)
  x <- purrr::compact(x)
  strings <- purrr::imap_chr(x, .f = data_block_element, nobs = nobs)
  if (length(strings)) {
    strings <- paste0("  ", strings, ";\n", collapse = "")
  }
  paste0("data {\n", strings, "}", collapse = "")
}

get_nobs <- function(x) {
  nobs <- x$nObs
  if (rlang::is_scalar_integer(nobs)) {
    return(nobs)
  }
  0L
}
