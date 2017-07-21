#' Log-Likelihood
#'
#' Log-Likelihood for a SMB analysis.
#'
#' @param object The smb_analysis object.
#' @param ... unused.
#' @export
logLik.smb_analysis <- function(object, ...) {
  log_lik <- derive_data(object, term = "log_lik")
  log_lik
}
