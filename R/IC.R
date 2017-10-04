#' @export
IC.smb_analysis <- function(object, ...) {

  logLik <- derive_logLik(object) %>%
    mcmcr::collapse_chains() %>%
    magrittr::use_series("logLik") %>%
    matrix(ncol = sample_size(object))

  npars <- logLik %>%
    matrixStats::colVars() %>%
    sum()

  logLik %<>%
    logColMeansExp() %>%
    sum()

  - 2 * (logLik - npars)
}
