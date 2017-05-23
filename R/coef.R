pvalue <- function(x) {
  n <- length(x)
  d <- sum(x >= 0)
  p <- min(d, n - d) * 2
  p <- max(p, 1)
  round(p / n, 4)
}

#' Coef Stan Analysis
#'
#' Coefficients for a Stan analysis.
#'
#' The \code{zscore} is mean / sd.
#'
#' @param object The mb_analysis object.
#' @param param_type A flag specifying whether 'fixed', 'random' or 'derived' terms.
#' @param include_constant A flag specifying whether to include constant terms.
#' @param conf_level A number specifying the confidence level. By default 0.95.
#' @param ... Not used.
#' @return A tidy tibble of the coefficient terms.
#' @export
coef.smb_analysis <- function(object, param_type = "fixed", include_constant = TRUE, conf_level = 0.95, ...) {

  check_scalar(param_type, c("fixed", "random", "derived"))
  check_flag(include_constant)
  check_number(conf_level, c(0.5, 0.99))

  parameters <- parameters(object, param_type)

  # Extract posterior of parameters
  ex <- extract(object$stanfit) %>%
    as.data.frame() %>%
    select_(.dots = as.list(parameters))

  s <- summary(object$stanfit, pars = parameters,
               probs = c((1 - conf_level) / 2, 0.5 + conf_level / 2)) %>%
    use_series(summary)

  s <- s[, c("mean", "sd", colnames(s)[grepl("[0-9]+%", colnames(s))])] %>%
    as.data.frame() %>% as.tbl()

  s$term <- parameters
  s$zscore <- s$mean / s$sd
  s$pvalue <- apply(ex, 2, pvalue)

  s %<>% set_colnames(c("estimate", "sd", "lower", "upper", "term",
                        "zscore", "pvalue")) %>%
    select(term, estimate, sd, zscore, lower, upper, pvalue)

  if (!include_constant) {
    s %<>% dplyr::filter_(~lower != upper)
  }

  s

}
