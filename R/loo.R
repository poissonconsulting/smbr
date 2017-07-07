#' LOO Stan Analysis
#'
#' Leave-one-out cross-validation (LOO)
#'
#' Efficient approximate leave-one-out cross-validation for Bayesian models. See
#' \link{loo-package} and Vehtari, Gelman, and Gabry (2016, 2017) for
#' background.
#'
#' @param analysis The smb_analysis object.
#' @param ... Optional arguments to pass to \code{\link{psislw}}. See \code{\link{loo}}.
#'
#' @return See \code{\link{loo}}
#'
#'
#' @seealso \code{\link{loo}}
#'
loo <- function(analysis, ...) {
  UseMethod("loo")
}

#' @export
loo.smb_analysis <- function(analysis, ...) {

  if (!("stan_fit" %in% names(analysis))) {
    stop("Analysis must contain stan_fit")
  }

  fit <- analysis$stan_fit

  stopifnot(inherits(fit, "stanfit"))

  log_lik <- loo::extract_log_lik(fit)

  loo <- loo::loo(log_lik)

  loo

}
