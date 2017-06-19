#' @export
#'

parameters.smb_code <- function(x, param_type = "fixed", scalar = TRUE, ...) {

  # Find parameter block and extract parameter types and names
  check_scalar(param_type, c("fixed", "random", "derived"))
  check_flag(scalar)

  x %<>% template()

  if (identical(param_type, "random")) {
    random <- names(random_effects(x))
    if (is.null(random)) random <- character(0)
    return(random)
  }

  if (param_type == "derived") {
    parameters <- types <- c()
    if (str_detect(x, "transformed parameters\\s*[{]{1}")) {
      parameters %<>% c(get_par_names(x, "transformed parameters"))
      types %<>% c(get_par_types(x, "transformed parameters"))
    }
    if (str_detect(x, "generated quantities\\s*[{]{1}")) {
      parameters %<>% c(get_par_names(x, "generated quantities"))
      types %<>% c(get_par_types(x, "generated quantities"))
    }
  } else {
    parameters <- get_par_names(x, "parameters")
    types <- get_par_types(x, "parameters")
  }

  # Which parameters are scalars?
  s <- types %in% c("int", "real")

  if (scalar) parameters <- parameters[s]

  parameters %<>% sort()

  parameters

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
