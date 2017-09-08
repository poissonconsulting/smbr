#' @export
IC.smb_analysis <- function(object, ...) {
  object %<>% log_lik_matrix()

  logLik <- object %>%
    logColMeansExp() %>%
    sum()

  npars <- object %>%
    matrixStats::colVars() %>%
    sum()

  - 2 * (logLik - npars)
}
