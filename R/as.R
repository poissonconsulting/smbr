#' @method as.mcmc.list stanfit
#' @export
as.mcmc.list.stanfit <- function(x, ...) as.mcmc.list(as.mcmcr(x))

#' @export
as.mcmcr.stanfit <- function(x, ...) {
  x <- as.mcmcr(rstan::As.mcmc.list(x))
  x["lp__"] <- NULL
  x
}
