#' Create Data Block from Data List
#'
#' Automate the data block straight from your data.
#'
#' @param data A list of parameters
#'
#' @return A string
#' @export
#'
#' @examples
#' mod_data <- list(
#'   X = c(1L, 2L, 3L, 4L),
#'   Y = c(1.2, 7.3, 8.9, 2.6),
#'   nObs = 4L
#' )
#' data_block(mod_data)
data_block <- function(data) {

  chk::chk_list(data)
  data <- as_nlist(data)
  # this checks that they are named, valid prams names, and all numeric values
  nlist::chk_nlist(data)

  parm_names <- names(data)

  block <- c("data {")
  if ("nObs" %in% parm_names) {
    for (i in parm_names) {
      if (inherits(data[[i]], "integer")) {
        if (length(data[[i]]) == data[["nObs"]]) {
          msg <- paste0("int ", i, "[nObs]", ";")
          block <- c(block, msg)
        } else {
          msg <- paste0("int ", i, ";")
          block <- c(block, msg)
        }
      }
      if (inherits(data[[i]], "numeric")) {
        if (length(data[[i]]) > 1) {
          msg <- paste0("real ", i, "[nObs]", ";")
          block <- c(block, msg)
        } else {
          msg <- paste0("real ", i, ";")
          block <- c(block, msg)
        }
      }
    }
    block <- c(block, "}")
    block <- paste0(block, collapse = "")
    block
  } else {
    for (i in parm_names) {
      if (inherits(data[[i]], "integer")) {
        if (length(data[[i]]) > 1) {
          msg <- paste0("int ", i, "[", length(data[[i]]), "]", ";")
          block <- c(block, msg)
        } else {
          msg <- paste0("int ", i, ";")
          block <- c(block, msg)
        }
      }
      if (inherits(data[[i]], "numeric")) {
        if (length(data[[i]]) > 1) {
          msg <- paste0("real ", i, "[", length(data[[i]]), "]", ";")
          block <- c(block, msg)
        } else {
          msg <- paste0("real ", i, ";")
          block <- c(block, msg)
        }
      }
    }
    block <- c(block, "}")
    block <- paste0(block, collapse = "")
    block
  }
}
