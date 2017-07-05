#' WAIC Stan Analysis
#'
#' Widely applicable information criterion (WAIC)
#'
#' @param analysis The smb_analysis object.
#' @param ... Other arguments. Currently ignored.
#'
#' @return A named list (of class \code{'loo'}) with components:
#'
#' \describe{
#' \item{\code{elpd_waic, se_elpd_waic}}{expected log pointwise predictive
#' density and standard error}
#' \item{\code{p_waic, se_p_waic}}{estimated effective number of parameters and
#' standard error}
#' \item{\code{waic, se_waic}}{\code{-2 * elpd_waic} (i.e., converted to the
#' deviance scale) and standard error}
#' \item{\code{pointwise}}{the pointwise contributions of each of the above
#' measures}
#' }
#'
#' @seealso \code{\link{waic}}
#'
#' @export

waic.smb_analysis <- function(analysis, ...) {

  if (!("stan_fit" %in% names(analysis))) {
    stop("Analysis must contain stan_fit")
  }

  fit <- analysis$stan_fit

  stopifnot(inherits(fit, "stanfit"))

  log_lik <- loo::extract_log_lik(fit)

  waic <- loo::waic(log_lik)

  waic

}
