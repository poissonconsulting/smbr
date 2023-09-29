#' Write Element of the Data Block
#'
#' @param x A object
#' @param name A object name
#' @param nobs A integer value of nObs
#' @param ... rlang::args_dots_empty
#'
#' @return A string
#' @noRd
#'
#' @examples
#' data_block_element(1L, "X")
#' data_block_element(1L, "X", nobs = 2L)
#' data_block_element(c(1, 2), "X")
#' data_block_element(c(1, 2), "X", nobs = 2L)
data_block_element <- function(x, name, nobs = NULL, ...) {
  chk::chk_unused(...)
  UseMethod("data_block_element")
}

#' @export
data_block_element.array <- function(x, name, nobs = NULL, ...) {
  chk::err("array data type not currently implemented")
}

#' @export
data_block_element.matrix <- function(x, name, nobs = NULL, ...) {
  chk::err("matrix data type not currently implemented")
}

#' @export
data_block_element.integer <- function(x, name, nobs = NULL, ...) {
  if(is.null(nobs)) {
    nobs <- 0L
  }
  chk::chk_count(nobs)

  if(!length(x)) return(NULL)

  if(rlang::is_scalar_integer(x)) {
    out <- paste0("int ", name, sep = "")
    return(out)
  }
  n <- length(x)
  if(nobs == n) {
    n <- "nObs"
  }
  paste0("int ", name, "[", n ,"]", sep = "")
}

#' @export
data_block_element.double <- function(x, name, nobs = NULL, ...) {
  if(is.null(nobs)) {
    nobs <- 0L
  }
  chk::chk_count(nobs)

  if(!length(x)) return(NULL)

  if(rlang::is_scalar_double(x)) {
    out <- paste0("real ", name, sep = "")
    return(out)
  }
  n <- length(x)
  if(nobs == n) {
    n <- "nObs"
  }
  paste0("real ", name, "[", n ,"]", sep = "")
}
