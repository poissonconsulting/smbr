#' IC Stan Analysis
#'
#' Widely applicable information criterion (WAIC)
#'
#' @param object The smb_analysis object.
#' @param ... Other arguments. Currently ignored.
#'
#' @return See \code{\link{waic}}
#'
#' @seealso \code{\link{waic}}
#'
#' @export
IC <- function(object, ...) {
  UseMethod("IC")
}

#' @export
IC.smb_analysis <- function(object, n = NULL, ...) {

  # Code adapted from loo version 1.0.0
  checkor(check_null(n), check_count(n))
  if (is.null(n))
    n <- sample_size(object)

  log_lik_mat <- derive(object, term = "log_lik") %>%
    mcmcr::collapse_chains() %>%
    use_series("log_lik") %>%
    matrix(nrow = n) %>% t()

  lpd <- logColMeansExp(log_lik_mat)
  p_waic <- matrixStats::colVars(log_lik_mat)
  elpd_waic <- lpd - p_waic
  waic <- -2 * elpd_waic
  pointwise <- nlist(elpd_waic, p_waic, waic)
  out <- totals(pointwise)
  nms <- names(pointwise)
  names(out) <- c(nms, paste0("se_", nms))
  out$pointwise <- cbind_list(pointwise)
  cat("waic:", out$waic)
  invisible(out)

}

logColMeansExp <- function(x) {
  # should be more stable than log(colMeans(exp(x)))
  S <- nrow(x)
  matrixStats::colLogSumExps(x) - log(S)
}

totals <- function(pointwise) {
  N <- length(pointwise[[1L]])
  total  <- unlist_lapply(pointwise, sum)
  se <- sqrt(N * unlist_lapply(pointwise, var))
  as.list(c(total, se))
}

unlist_lapply <- function(X, FUN, ...) {
  unlist(lapply(X, FUN, ...), use.names = FALSE)
}

cbind_list <- function(x) {
  do.call(cbind, x)
}

nlist <- function(...) {
  m <- match.call()
  out <- list(...)
  no_names <- is.null(names(out))
  has_name <- if (no_names) FALSE else nzchar(names(out))
  if (all(has_name))
    return(out)
  nms <- as.character(m)[-1L]
  if (no_names) {
    names(out) <- nms
  } else {
    names(out)[!has_name] <- nms[!has_name]
  }

  return(out)
}
