#' Is STAN Code
#'
#' Tests whether x is an object of class 'smb_code'
#'
#' @param x The object to test.
#' @return A flag indicating whether the test was positive.
#' @export
is.smb_code <- function(x) {
  inherits(x, "smb_code")
}

#' Is a STAN Model
#'
#' Tests whether x is an object of class 'smb_model'
#'
#' @param x The object to test.
#'
#' @return A flag indicating whether the test was positive.
#' @export
is.smb_model <- function(x) {
  inherits(x, "smb_model")
}

#' Is a STAN Analysis
#'
#' Tests whether x is an object of class 'smb_analysis'
#'
#' @param x The object to test.
#'
#' @return A flag indicating whether the test was positive.
#' @export
is.smb_analysis <- function(x) {
  inherits(x, "smb_analysis")
}
