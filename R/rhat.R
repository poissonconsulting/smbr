#' @export
rhat.smb_analysis <- function(x, ...) {

  #x <- As.mcmc.list(x$stan_fit)
  fit <- x$stan_fit
  Rhat <- rstan::summary(x$stan_fit)$summary[, "Rhat"]

  #if (length(x) < 2) error("x must have at least two chains")

  #mpsrf <- coda::gelman.diag(x)$mpsrf %>% round(2)
  mpsrf <- round(max(Rhat), 2)

  mpsrf

}
