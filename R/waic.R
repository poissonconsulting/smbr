#' WAIC Stan Analysis
#'
#' Widely applicable information criterion (WAIC)
#'
#' @param analysis The mb_analysis object.
#' @param ... Other arguments. Currently ignored.
#'
#' @return A tidy tibble of the coefficient terms.
#'
#' @seealso \code{\link{loo::waic}}
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
