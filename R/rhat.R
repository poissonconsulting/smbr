#' @export
rhat.smb_analysis <- function(x, ...) {

  x <- As.mcmc.list(x$stanfit)

  if (length(x) < 2) error("x must have at least two chains")

  mpsrf <- coda::gelman.diag(x)$mpsrf %>% round(2)

  mpsrf

}
