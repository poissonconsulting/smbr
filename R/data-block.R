#' Create Data Block from Data List
#'
#' Automate writing the STAN data block from your data.
#'
#' @param data A nlist
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
  x <- purrr::keep(x, has_length)
  strings <- purrr::imap_chr(x, .f = data_block_element, nobs = nobs)
  if(length(strings)) {
    strings <- paste0("  ", strings, ";\n", collapse = "")
  }
  paste0("data {\n", strings, "}", collapse = "")
}

data_block_element <- function(x, name, nobs = NULL, ...) {
  chk_unused(...)
  UseMethod("data_block_element")
}

# blow away zero lengths...
data_block_element.array <- function(x, name, nobs = NULL, ...) {
  err("array data type not currently implemented")
}

data_block_element.matrix <- function(x, name, nobs = NULL, ...) {
  err("matrix data type not currently implemented")
}

data_block_element.integer <- function(x, name, nobs = NULL, ...) {
  if(is.null(nobs)) {
    nobs <- 0L
  }
  chk_count(nobs)

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

data_block_element.double <- function(x, name, nobs = NULL, ...) {
  if(is.null(nobs)) {
    nobs <- 0L
  }
  chk_count(nobs)

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

has_length <- function(x) {
  length(x) != 0
}

get_nobs <- function(x) {
  nobs <- x$nObs
  if(rlang::is_scalar_integer(nobs)) return(nobs)
  0L
}
