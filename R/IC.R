#' @export
IC.smb_analysis <- function(object, ...) {

  log_lik <- derive_log_lik(object) %>%
    mcmcr::collapse_chains() %>%
    magrittr::use_series("log_lik") %>%
    matrix(ncol = sample_size(object))

  npars <- log_lik %>%
    matrixStats::colVars() %>%
    sum()

  log_lik %<>%
    logColMeansExp() %>%
    sum()

  - 2 * (log_lik - npars)
}
