#' @export
parameters.smb_analysis <- function(x, param_type = "fixed", ...) {
  check_scalar(param_type, c("fixed", "random", "derived"))

  random <- names(random_effects(x))
  if (is.null(random)) random <- character(0)
  derived <- x$model$derived

  if (identical(param_type, "random")) return(random)
  if (identical(param_type, "derived")) return(derived)

  parameters <- x$stanfit@model_pars

  parameters %<>% setdiff(random) %>% setdiff(derived)
  parameters
}
