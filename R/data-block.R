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
  for (i in parm_names) {

    if (inherits(data[[i]], "integer")) {
      if (i != "nObs") {
        msg <- paste0("int ", i, "[nObs]", ";")
        block <- c(block, msg)
      } else {
        msg <- paste0("int ", i, ";")
        block <- c(block, msg)
      }
    }

    if (inherits(data[[i]], "numeric")) {
      msg <- paste0("real ", i, "[nObs]" ,";")
      block <- c(block, msg)
    }
  }
  block <- c(block, "}")
  block <- paste0(block, collapse = "")
  block
}


# need to check the length of things to attach the nObs
# need to check length if length the same then use nObs if not then use the length


# the type (real/int) are required
# the lower/upper are nice but not required
# Use the select data to set the constraint for upper/lower bound
# take the type from the select

# if its a matrix in the modify data, you could take the dimensions but it would be hard to set it to the n thing

# the doy was set to be real because it was centered/scaled, but really its the scaling part

# inputs will be model and the data

#
# data <- data.frame(
#   X = c(1L, 2L, 3L, 4L),
#   Y = c(1.2, 7.3, 8.9, 2.6)
# )
#
# "data {int nObs;int X[nObs];real Y[nObs];}"

# data <- data.frame(
#   annual = c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L),
#   site = factor(c(1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L)),
#   quadrat = c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L),
#   kelpline = c(0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 1L, 0L),
#   year = c(0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L),
#   count = c(0L, 20L, 4L, 10L, 2L, 3L, 2L, 5L, 4L, 12L, 7L, 17L),
#   temp = c(10.2, -0.3, 12.7, 16.4, 10.3, 12.6, 14.3, 13.2, 17.3, 13.4, 12.5, 14.5)
# )
#
#
#
# # output of the data block
# "data {
#     int nObs;
#     int nsite;
#     int annual[nObs];
#     int site[nObs];
#     int quadrat[nObs];
#     int kelpline[nObs];
#     int year[nObs];
#     int count[nObs];
#     real temp[nObs];
#   }"
