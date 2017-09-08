log_lik_matrix <- function(x) {
  log_lik <- derive(x, term = "logLik")

  dim <- dim(log_lik[["logLik"]])

  if (!identical(length(dim), 3L))
    error("logLik term 'logLik' must be a vector")

  n <- dim[3]

  if (!identical(n, sample_size(x)))
    warning("number of logLik terms does not equal number of rows of data")

  log_lik %<>%
    mcmcr::collapse_chains() %>%
    magrittr::use_series("logLik") %>%
    matrix(ncol = n)

  log_lik
}

#' Log-Likelihood
#'
#' Log-Likelihood for a SMB analysis.
#'
#' @param object The smb_analysis object.
#' @param ... unused.
#' @export
logLik.smb_analysis <- function(object, ...) {
  object %<>% log_lik_matrix() %>%
    logColMeansExp() %>%
    sum()
  object
}
