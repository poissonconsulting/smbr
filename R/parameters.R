#' @export
parameters.smb_code <- function(x, param_type = "fixed", scalar = TRUE, ...) {

  # Find parameter block and extract parameter types and names
  check_scalar(param_type, c("fixed", "random", "derived"))
  check_flag(scalar)

  if(param_type == "derived") {

  }

  x <- y # for debuggin

  x %<>% template()

  parameters <- get_block(x, "parameters")
  transformed <- get_block(x, "transformed parameters")

  scalar <- !str_detect(x, "\\[")

  x %<>% str_extract("\\w+$")

  if(scalar) x <- x[scalar]

  x %<>% sort()

  x
}

#' @export
parameters.smb_analysis <- function(x, param_type = "fixed", ...) {
  check_scalar(param_type, c("fixed", "random", "derived"))

  random <- names(random_effects(x))
  if (is.null(random)) random <- character(0)
  derived <- x$model$derived

  if (identical(param_type, "random")) return(random)
  if (identical(param_type, "derived")) return(derived)

  parameters <- x$stan_fit@model_pars

  parameters %<>% setdiff(random) %>% setdiff(derived)
  parameters
}
