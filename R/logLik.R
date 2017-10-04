derive_logLik <- function(x) {
  logLik <- derive(x, term = "logLik")

  dim <- dim(logLik[["logLik"]])

  if (!identical(length(dim), 3L))
    error("logLik term 'logLik' must be a vector")

  n <- dim[3]

  if (!identical(n, sample_size(x)))
    warning("number of logLik terms does not equal number of rows of data")

  logLik
}

#' Log-Likelihood
#'
#' Log-Likelihood for a SMB analysis.
#'
#' @param object The smb_analysis object.
#' @param ... unused.
#' @export
logLik.smb_analysis <- function(object, ...) {

  logLik <- derive_logLik(object) %>%
    mcmcr::collapse_chains() %>%
    magrittr::use_series("logLik") %>%
    matrix(ncol = sample_size(object)) %>%
    logColMeansExp() %>%
    sum()

  logLik
}
